{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module HsNixPkgs.HsBuilder.Generate
  ( HsQ,
    ModuleType (..),
    ModuleM,
    TExprM (..),
    declFun,
    mainFunc,
    useModule,
    HsModule,
    stringModule,
    declModule,
    HsExecutable (..),
    hsExecutable,
  )
where

import Control.Applicative
import Control.Monad.RWS.Strict
import Control.Monad.State.Strict
import Control.Monad.Writer
import Data.Functor
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe
import Data.String
import HsNixPkgs.HsBuilder.Internal.BuildMod
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

newtype HsQ a = QM (State Int a)
  deriving (Functor, Applicative, Monad)

instance Quote HsQ where
  newName n =
    QM
      ( state (\i -> (i, i + 1))
          <&> mkNameU n . fromIntegral
      )

type PkgDep s = (HM.HashMap String s, HS.HashSet String)

type DepM s = WriterT (PkgDep s) (State (HM.HashMap String Bool))

data Mod s = Mod
  { depM :: DepM s (),
    modDec :: [Dec] -> [Dec]
  }

instance Semigroup (Mod s) where
  l <> r =
    Mod
      { depM = depM l >> depM r,
        modDec = modDec l . modDec r
      }

instance Monoid (Mod s) where
  mempty =
    Mod
      { depM = pure (),
        modDec = id
      }

data ModuleType
  = MainModule
  | OtherModule

newtype ModuleM s (t :: ModuleType) a = M (RWS String (Mod s) Int a)
  deriving (Functor, Applicative, Monad)

newtype TExprM s t et = TExprM {unTExpr :: ModuleM s t Exp}

runModuleM :: String -> ModuleM s t a -> (a, DepM s (), [Dec])
runModuleM n (M m) =
  let (v, d) = evalRWS m n 0
   in (v, depM d, modDec d [])

declFun :: HsQ [Dec] -> ModuleM s t Name
declFun (QM md) =
  M
    ( do
        ds <- state (runState md)
        tell (Mod {depM = pure (), modDec = (ds ++)})
        Name
          (OccName (showName (fromJust (findName ds))))
          . NameQ
          . ModName
          <$> ask
    )
  where
    findName [] = Nothing
    findName (x : xs) =
      findName xs
        <|> case x of
          FunD n _ -> Just n
          ValD (VarP n) _ _ -> Just n
          TySynD n _ _ -> Just n
          _ -> Nothing

mainFunc :: Code HsQ (IO ()) -> ModuleM s 'MainModule (TExp (IO ()))
mainFunc (Code (QM md)) = M (state (runState md))

useModule :: HsModule s 'OtherModule a -> ModuleM s t a
useModule m = M (modVal m <$ tell (mempty {depM = modDepM m}))

data HsModule s (t :: ModuleType) a = HSModule
  { modDepM :: DepM s (),
    modVal :: a
  }

addDep :: String -> PkgDep s -> DepM s () -> DepM s ()
addDep name val dm =
  gets (HM.lookup name) >>= \case
    Just True -> pure ()
    Just False -> error "cycle detected in module dependency graph"
    Nothing -> dm >> tell val >> modify (HM.insert name True)

stringModule :: String -> s -> [String] -> a -> HsModule s 'OtherModule a
stringModule name text p v =
  HSModule
    { modDepM =
        addDep
          name
          (HM.singleton name text, HS.fromList p)
          (pure ()),
      modVal = v
    }

declModule ::
  IsString s =>
  String ->
  [Extension] ->
  ModuleM s 'OtherModule a ->
  HsModule s 'OtherModule a
declModule name ext c =
  let (v, dm, decs) = runModuleM name c
      (dt, pd) = buildMod name ext decs
   in HSModule
        { modDepM =
            addDep name (HM.singleton name (fromString dt), pd) $
              modify (HM.insert name False) >> dm,
          modVal = v
        }

data HsExecutable s = HsExecutable
  { execMainModule :: s,
    execOtherModules :: HM.HashMap String s,
    execDependencies :: HS.HashSet String
  }
  deriving (Show)

hsExecutable :: IsString s => [Extension] -> ModuleM s 'MainModule (TExp (IO ())) -> HsExecutable s
hsExecutable ext m =
  let (e, dm, decs) = runModuleM "Main" m
      (om, opd) = evalState (execWriterT dm) HM.empty
      (dt, pd) =
        buildMod
          "Main"
          ext
          ( let main = mkName "main"
             in SigD main (ConT ''IO `AppT` TupleT 0)
                  : FunD main [Clause [] (NormalB (unType e)) []]
                  : decs
          )
   in HsExecutable
        { execMainModule = fromString dt,
          execOtherModules = om,
          execDependencies = HS.union opd pd
        }