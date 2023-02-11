module HsNixPkgs.BootTools.StorePath (StorePath (..), parseStorePath) where

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

data StorePath = StorePath
  { storePathRoot :: Text,
    storePathHash :: Text,
    storePathName :: Text
  }
  deriving (Show)

parseStorePath :: Text -> Text -> StorePath
parseStorePath root t =
  let (h, n) =
        T.span
          (`elem` ("0123456789abcdfghijklmnpqrsvwxyz" :: String))
          (fromJust (T.stripPrefix root t >>= T.stripPrefix "/"))
   in StorePath
        { storePathRoot = root,
          storePathHash = h,
          storePathName = fromJust (T.stripPrefix "-" n)
        }