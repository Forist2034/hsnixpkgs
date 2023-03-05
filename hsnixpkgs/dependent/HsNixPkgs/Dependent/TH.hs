{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module HsNixPkgs.Dependent.TH (genProgFunc) where

import Control.Applicative
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Foldable
import Data.Functor
import qualified Data.HashSet as HS
import Data.Hashable
import Data.Maybe
import Data.Singletons
import Data.Type.Equality
import HsNixPkgs.Dependent.Types
import Language.Haskell.TH

data Offset
  = Build
  | Host
  | Target
  deriving (Eq, Ord, Show)

instance Enum Offset where
  fromEnum Build = -1
  fromEnum Host = 0
  fromEnum Target = 1

  toEnum (-1) = Build
  toEnum 0 = Host
  toEnum 1 = Target
  toEnum _ = error "Invalid platform"

type DepType = (Offset, Offset)

inRange :: Offset -> Offset -> Bool
inRange l r =
  let s = fromEnum l + fromEnum r
   in -1 <= s && s <= 1

mapOffset :: DepType -> Offset -> Offset
mapOffset (h, t) i =
  toEnum
    ( fromEnum i
        + if i /= Target
          then fromEnum h
          else fromEnum t - 1
    )

progDep :: DepType -> DepType -> Maybe DepType
progDep p0@(h0, _) (h1, t1)
  | inRange h0 h1 && inRange h0 t1 = Just (mapOffset p0 h1, mapOffset p0 t1)
  | otherwise = Nothing

allDepType :: [DepType]
allDepType =
  [ (Build, Build),
    (Build, Host),
    (Build, Target),
    (Host, Host),
    (Host, Target),
    (Target, Target)
  ]

type ProgM a b h t = State (SimpleDeps HS.HashSet a b h t, SimpleDeps [] a b h t)

guardVertex ::
  Hashable elem =>
  elem ->
  (SimpleDeps HS.HashSet a b h t -> HS.HashSet elem) ->
  ProgM a b h t () ->
  ProgM a b h t ()
guardVertex v sel f =
  gets (HS.member v . sel . fst) >>= \e -> unless e f

selector :: DepType -> Name
selector (Build, Build) = 'depsBuildBuild
selector (Build, Host) = 'depsBuildHost
selector (Build, Target) = 'depsBuildTarget
selector (Host, Host) = 'depsHostHost
selector (Host, Target) = 'depsHostTarget
selector (Target, Target) = 'depsTargetTarget
selector _ = error "Invalid offset"

progExpr :: DepType -> Name -> (Name, Name) -> (DepType -> Name) -> ExpQ
progExpr dt v (b, h) pf = do
  d <- newName "d"
  (mf, mt) <-
    let f e =
          newName "s" <&> \s ->
            LamE [VarP s] (RecUpdE (VarE s) [(sel, e (VarE sel `AppE` VarE s))])
     in liftA2
          (,)
          [|
            modify
              ( first
                  $(f (\s -> VarE 'HS.insert `AppE` VarE v `AppE` s))
              )
            |]
          [|
            modify
              (second $(f (InfixE (Just (VarE v)) (ConE '(:)) . Just)))
            |]
  ps <-
    sequence
      ( case dt of
          (Target, Target) ->
            [ [|
                case testEquality $(pure (VarE 'sing `AppTypeE` VarT b)) $(pure (VarE 'sing `AppTypeE` VarT h)) of
                  Just Refl -> do
                    traverse_ $(varE (pf (Host, Host))) (depsBuildBuild $(varE d))
                    traverse_ $(varE (pf (Host, Target))) (depsBuildHost $(varE d))
                  Nothing -> pure ()
                |],
              [|traverse_ $(varE (pf (Target, Target))) (depsTargetTarget $(varE d))|]
            ]
          _ -> mapMaybe (prog d) allDepType
      )
  pure
    ( VarE 'guardVertex
        `AppE` VarE v
        `AppE` VarE sel
        `AppE` DoE
          Nothing
          ( NoBindS mf
              : LetS [ValD (VarP d) (NormalB (VarE 'propagatedDep `AppE` VarE v)) []]
              : (NoBindS <$> ps)
              ++ [NoBindS mt]
          )
    )
  where
    sel = selector dt
    prog d dt2 =
      progDep dt dt2 <&> \pdt ->
        pure
          ( VarE 'traverse_
              `AppE` VarE (pf pdt)
              `AppE` (VarE (selector dt2) `AppE` VarE d)
          )

progFuncName :: Name
progFuncName = mkName "propagateDependencies"

genProgFunc :: DecsQ
genProgFunc = do
  pf <- do
    bb <- newName "progBB"
    bh <- newName "progBH"
    bt <- newName "progBT"
    hh <- newName "progHH"
    ht <- newName "progHT"
    tt <- newName "progTT"
    pure
      ( \case
          (Build, Build) -> bb
          (Build, Host) -> bh
          (Build, Target) -> bt
          (Host, Host) -> hh
          (Host, Target) -> ht
          (Target, Target) -> tt
          _ -> error "Invalid dep type"
      )
  fs <-
    concat
      <$> traverse
        ( \dt -> do
            let n = pf dt
            v <- newName "v"
            b <- newName "b"
            h <- newName "h"
            e <- progExpr dt v (b, h) pf
            t <- do
              c <- newName "c"
              a <- newName "a"
              t <- newName "t"
              let vf o =
                    case o of
                      Build -> varT b
                      Host -> varT h
                      Target -> varT t
              ForallT
                (fmap (`PlainTV` SpecifiedSpec) [a, b, h, t, c])
                <$> sequence
                  [ [t|HasPropagatedDep $(varT a) $(varT c)|],
                    [t|SingI $(varT b)|],
                    [t|SingI $(varT h)|],
                    [t|CA Hashable $(varT a)|]
                  ]
                <*> [t|
                  $(varT a) $(varT b) $(vf (fst dt)) $(vf (snd dt)) ->
                  State
                    ( SimpleDeps HS.HashSet $(varT a) $(varT b) $(varT h) $(varT t),
                      SimpleDeps [] $(varT a) $(varT b) $(varT h) $(varT t)
                    )
                    ()
                  |]
            pure
              [ SigD n t,
                FunD n [Clause [VarP v] (NormalB e) []]
              ]
        )
        allDepType
  fd <- do
    vs <- newName "vs"
    t <-
      [t|
        forall a b h t c.
        (SingI b, SingI h, HasPropagatedDep a c, CA Hashable a) =>
        [SimpleDeps c a b h t] ->
        SimpleDeps [] a b h t
        |]
    e <-
      [|
        let rDep =
              snd
                ( execState
                    ( traverse_
                        ( \v -> do
                            traverse_ $(varE (pf (Build, Build))) (depsBuildBuild v)
                            traverse_ $(varE (pf (Build, Host))) (depsBuildHost v)
                            traverse_ $(varE (pf (Build, Target))) (depsBuildTarget v)
                            traverse_ $(varE (pf (Host, Host))) (depsHostHost v)
                            traverse_ $(varE (pf (Host, Target))) (depsHostTarget v)
                            traverse_ $(varE (pf (Target, Target))) (depsTargetTarget v)
                        )
                        $(varE vs)
                    )
                    mempty
                )
         in SimpleDeps
              { depsBuildBuild = reverse (depsBuildBuild rDep),
                depsBuildHost = reverse (depsBuildHost rDep),
                depsBuildTarget = reverse (depsBuildTarget rDep),
                depsHostHost = reverse (depsHostHost rDep),
                depsHostTarget = reverse (depsHostTarget rDep),
                depsTargetTarget = reverse (depsTargetTarget rDep)
              }
        |]
    pure [SigD progFuncName t, FunD progFuncName [Clause [VarP vs] (NormalB e) []]]
  pure (fs ++ fd)
