module HsNixPkgs.BootTools.SupportTools (copySupportTool) where

import Crypto.Hash
import Data.Binary
import qualified Data.ByteString as BS
import Data.Functor
import Data.Text (Text)
import qualified Data.Text as T
import HsNixPkgs.BootTools.Types
import Nix.Nar
import System.Directory
import System.FilePath

copySupportTool :: Text -> Bool -> FilePath -> FilePath -> IO SupportTool
copySupportTool name exec src dest =
  let n = T.unpack name
   in do
        copyFile (src </> n) (dest </> n)
        hsh <-
          BS.readFile (src </> n) <&> \c ->
            if exec
              then hashlazy (encode (Nar (Regular Executable c)))
              else hash c
        pure
          ( SupportTool
              { stName = name,
                stExecutable = exec,
                stSha256 = Hash hsh
              }
          )