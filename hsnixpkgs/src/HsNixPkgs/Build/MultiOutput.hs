{-# LANGUAGE DeriveTraversable #-}

module HsNixPkgs.Build.MultiOutput (
  MultiOutput (..),
  defMultiOutput,
  getMultiOutPath,
) where

import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NEL
import Data.Maybe
import Data.Text (Text)
import HsNix.OutputName
import Language.Haskell.GenPackage
import Language.Haskell.TH

data MultiOutput a = MultiOutput
  { shareDocName :: Text,
    outDev :: a,
    outBin :: a,
    outInclude :: a,
    outLib :: a,
    outDoc :: a,
    outDevDoc :: a,
    outMan :: a,
    outDevMan :: a,
    outInfo :: a
  }
  deriving (Show, Functor, Foldable, Traversable)

defMultiOutput :: Text -> NEL.NonEmpty OutputName -> MultiOutput OutputName
defMultiOutput sdn os =
  let dev = getOrDefault "dev" outON
      bin = getOrDefault "bin" outON
      man = getOrDefault "man" bin
   in MultiOutput
        { outDev = dev,
          outBin = bin,
          outInclude = getOrDefault "include" dev,
          outLib = getOrDefault "lib" outON,
          outDoc = getOrDefault "doc" outON,
          outDevDoc = makeOutputNameThrow "devdoc",
          outMan = man,
          outDevMan = getOrDefault "devman" (getOrDefault "devdoc" man),
          outInfo = getOrDefault "info" bin,
          shareDocName = sdn
        }
  where
    outON = makeOutputNameThrow "out"
    getOrDefault v d =
      let onV = makeOutputNameThrow v
       in if onV `elem` os then onV else d

getMultiOutPath ::
  HM.HashMap OutputName (Code HsQ FilePath) ->
  MultiOutput OutputName ->
  MultiOutput (Code HsQ FilePath)
getMultiOutPath m = fmap (\o -> fromJust (HM.lookup o m))