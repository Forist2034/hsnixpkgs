{-# LANGUAGE TemplateHaskellQuotes #-}

module HsNixPkgs.Build.Phase
  ( Phase,
    phaseExpr,
    phaseDeclAs,
    phaseDecl,
    newPhase,
    runPhases,
  )
where

import HsNixPkgs.Boot.Build.Phase hiding (runPhases)
import qualified HsNixPkgs.Boot.Build.Phase as B
import HsNixPkgs.Build.Main (BIO)
import HsNixPkgs.HsBuilder.Generate
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

phaseExpr :: String -> String -> Code HsQ (BIO ()) -> Code HsQ Phase
phaseExpr n d e =
  unsafeCodeCoerce
    [|
      Phase
        { phaseName = $(lift n),
          phaseDesc = $(lift d),
          phaseFunc = $(unTypeCode e)
        }
      |]

phaseDeclAs ::
  Name ->
  String ->
  String ->
  Code HsQ (BIO ()) ->
  HsQ [Dec] ->
  HsQ [Dec]
phaseDeclAs dn n d e ld = do
  l <- ld
  sequence
    [ sigD dn [t|Phase|],
      funD dn [clause [] (normalB (unTypeCode (phaseExpr n d e))) (pure <$> l)]
    ]

phaseDecl ::
  String ->
  String ->
  Code HsQ (BIO ()) ->
  HsQ [Dec] ->
  HsQ [Dec]
phaseDecl n d e ld = do
  dn <- newName n
  phaseDeclAs dn n d e ld

newPhase ::
  String ->
  String ->
  Code HsQ (BIO ()) ->
  HsQ [Dec] ->
  ModuleM s mt (Code HsQ Phase)
newPhase n d e ld =
  unsafeCodeCoerce . varE
    <$> declFun (phaseDecl n d e ld)

runPhases :: [Code HsQ Phase] -> Code HsQ (BIO ())
runPhases ps =
  unsafeCodeCoerce
    [|
      B.runPhases $(listE (fmap unTypeCode ps))
      |]