module HsNixPkgs.Build.MultiOutput
  ( MultiOutput (..),
    defMultiOutput,
    getMultiOutPath,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NEL
import Data.Maybe
import Data.Text (Text)
import HsNixPkgs.HsBuilder.Generate
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
  deriving (Show)

defMultiOutput :: Text -> NEL.NonEmpty Text -> MultiOutput Text
defMultiOutput sdn os =
  let dev = getOrDefault "dev" "out"
      bin = getOrDefault "bin" "out"
      man = getOrDefault "man" bin
   in MultiOutput
        { outDev = dev,
          outBin = bin,
          outInclude = getOrDefault "include" dev,
          outLib = getOrDefault "lib" "out",
          outDoc = getOrDefault "doc" "out",
          outDevDoc = "devdoc",
          outMan = man,
          outDevMan = getOrDefault "devman" (getOrDefault "devdoc" man),
          outInfo = getOrDefault "info" bin,
          shareDocName = sdn
        }
  where
    getOrDefault v d =
      if v `elem` os then v else d

mapOutput :: (a -> b) -> MultiOutput a -> MultiOutput b
mapOutput f m =
  MultiOutput
    { shareDocName = shareDocName m,
      outDev = f (outDev m),
      outBin = f (outBin m),
      outInclude = f (outInclude m),
      outLib = f (outLib m),
      outDoc = f (outDoc m),
      outDevDoc = f (outDevDoc m),
      outMan = f (outMan m),
      outDevMan = f (outDevMan m),
      outInfo = f (outInfo m)
    }

getMultiOutPath ::
  HM.HashMap Text (Code HsQ FilePath) ->
  MultiOutput Text ->
  MultiOutput (Code HsQ FilePath)
getMultiOutPath m = mapOutput (\o -> fromJust (HM.lookup o m))