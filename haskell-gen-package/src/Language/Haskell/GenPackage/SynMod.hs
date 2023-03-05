{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Language.Haskell.GenPackage.SynMod (
  HsQ,
  ModuleM,
  declFun,
  useModule,
  otherModule,
  mainModule,
) where

import Control.Applicative
import Control.Monad.RWS.Strict
import Control.Monad.State.Strict
import Data.Functor
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.String
import Language.Haskell.GenPackage.BuildMod
import Language.Haskell.GenPackage.Types
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

data Mod = Mod
  { depM :: DepM (),
    modDec :: [Dec] -> [Dec]
  }

instance Semigroup Mod where
  l <> r =
    Mod
      { depM = depM l >> depM r,
        modDec = modDec l . modDec r
      }

instance Monoid Mod where
  mempty =
    Mod
      { depM = pure (),
        modDec = id
      }

newtype ModuleM (t :: ModuleType) a = M (RWS String Mod Int a)
  deriving (Functor, Applicative, Monad)

runModuleM :: String -> ModuleM t a -> (a, DepM (), [Dec])
runModuleM n (M m) =
  let (v, d) = evalRWS m n 0
   in (v, depM d, modDec d [])

declFun :: HsQ [Dec] -> ModuleM t Name
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

useModule :: HsModule 'OtherModule a -> ModuleM t a
useModule m = M (modVal m <$ tell (mempty {depM = modDepM m}))

declModule ::
  String ->
  [Extension] ->
  ModuleM mt a ->
  HsModule mt a
declModule name ext c =
  let (v, dm, decs) = runModuleM name c
      (dt, pd) = buildMod name ext decs
   in HSModule
        { modDepM =
            addDep name (HM.singleton name (fromString dt), pd) $
              modify (HM.insert name False) >> dm,
          modVal = v
        }

otherModule ::
  String ->
  [Extension] ->
  ModuleM 'OtherModule a ->
  HsModule 'OtherModule a
otherModule = declModule

mainModule :: [Extension] -> ModuleM 'MainModule (Code HsQ (IO ())) -> HsModule 'MainModule ()
mainModule exts m =
  declModule "Main" exts $ do
    expr <- m
    void
      ( declFun
          [d|
            main :: IO ()
            main = $(unTypeCode expr)
            |]
      )