module HsNixPkgs.Boot.Build.Patch
  ( plainText,
    unGzip,
    unBzip2,
    unXz,
    unLzma,
    applyPatch,
  )
where

import Data.Maybe
import HsNixPkgs.Boot.Build.Util
import System.IO
import System.Process

withUncompressed :: CreateProcess -> FilePath -> (Handle -> IO ()) -> IO ()
withUncompressed ocp f hdl =
  withBinaryFile
    f
    ReadMode
    ( \fh ->
        let cp = ocp {std_in = UseHandle fh, std_out = CreatePipe}
         in withCreateProcess
              cp
              ( \_ o _ ph ->
                  hdl (fromJust o)
                    >> waitForProcSuccess cp ph
              )
    )

plainText :: FilePath -> (Handle -> IO ()) -> IO ()
plainText f = withBinaryFile f ReadMode

unGzip :: FilePath -> (Handle -> IO ()) -> IO ()
unGzip = withUncompressed (proc "gzip" ["-d"])

unBzip2 :: FilePath -> (Handle -> IO ()) -> IO ()
unBzip2 = withUncompressed (proc "bzip2" ["-d"])

unXz :: FilePath -> (Handle -> IO ()) -> IO ()
unXz = withUncompressed (proc "xz" ["-d"])

unLzma :: FilePath -> (Handle -> IO ()) -> IO ()
unLzma = withUncompressed (proc "lzma" ["-d"])

applyPatch :: String -> [String] -> ((Handle -> IO ()) -> IO ()) -> IO ()
applyPatch n f hf =
  echo ["Applying patch ", n]
    >> hf (\h -> callCreateProcess ((proc "patch" f) {std_in = UseHandle h}))