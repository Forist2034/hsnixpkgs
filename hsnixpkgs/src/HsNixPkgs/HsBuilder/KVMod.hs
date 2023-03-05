module HsNixPkgs.HsBuilder.KVMod (KVPair (..), mkKVMod) where

import Data.Foldable
import Data.String
import qualified HsNix.DrvStr.Builder as DSB
import HsNixPkgs.HsBuilder.DepStr
import Language.Haskell.GenPackage
import Language.Haskell.TH.Syntax

data KVPair = KVPair
  { kvKey :: String,
    kvType :: DSB.Builder,
    kvValue :: DepStr
  }

mkKVMod ::
  Traversable t =>
  String ->
  [String] ->
  [Extension] ->
  t KVPair ->
  HsModule 'OtherModule (t Name)
mkKVMod modName d es kvs =
  stringModule
    modName
    ( StringModule
        { smText =
            DepStr
              ( DSB.toDrvStr
                  . ( \str ->
                        foldMap
                          (\e -> "{-# LANGUAGE " <> fromString (show e) <> " #-}\n")
                          es
                          <> ("module " <> fromString modName <> " where\n")
                          <> "\n"
                          <> foldl' (<>) mempty str
                    )
                  <$> traverse
                    ( \kvp ->
                        let sn = fromString (kvKey kvp)
                         in fmap
                              ( \s ->
                                  mconcat [sn, " :: ", kvType kvp, "\n"]
                                    <> mconcat [sn, " = \"", DSB.fromDrvStr s, "\"\n"]
                              )
                              (getDepStr (kvValue kvp))
                    )
                    kvs
              ),
          smExtDep = d,
          smValue =
            fmap
              (\KVPair {kvKey = n} -> Name (OccName n) (NameQ (ModName modName)))
              kvs
        }
    )
