{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}

module HsNixPkgs.Boot.Develop.BuildSystem.Gnu
  ( configureFunc,
    MakeCfg (..),
    runMake,
    installFunc,
    distFunc,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Foldable
import Data.Functor
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import HsNixPkgs.Boot.Build.Main
import HsNixPkgs.Boot.Build.Util
import Language.Haskell.TH.Syntax (Lift)
import System.Directory
import System.FilePath
import System.Posix.Env.ByteString
import System.Posix.Files
import System.Process

fixLibtool :: Text -> IO ()
fixLibtool sp =
  do
    putEnv "lt_cv_deplibs_check_method=pass_all"
    dir <- listDirRec "."
    traverse_
      (\f -> echo ["fixing libtool script ", f] >> fixSearchPath f)
      (filter (\f -> takeFileName f == "ltmain.sh") dir)
    {-replace `/usr/bin/file` with `file` in any `configure`
      scripts with vendored libtool code.  Preserve mtimes to
      prevent some packages (e.g. libidn2) from spontaneously
      autoreconf'ing themselves-}
    mapM
      (\f -> (f,) <$> getSymbolicLinkStatus f)
      (filter (\f -> takeFileName f == "configure") dir)
      >>= traverse_
        ( \(f, fs) ->
            TIO.readFile f >>= \cont ->
              when
                ( "GNU Libtool is free software; you can redistribute it and/or modify"
                    `T.isInfixOf` cont
                )
                $ do
                  TIO.writeFile f (T.replace "/usr/bin/file" "file" cont)
                  setSymbolicLinkTimesHiRes
                    f
                    (accessTimeHiRes fs)
                    (modificationTimeHiRes fs)
        )
        . filter
          ( \(_, fs) ->
              isRegularFile fs
                && ownerExecuteMode
                `intersectFileModes` fileMode fs
                == ownerExecuteMode
          )
  where
    fixSearchPath =
      modifyTextFile
        ( T.unlines
            . fmap
              ( \cont ->
                  let (pref, e) = T.breakOn "eval sys_lib_" cont
                   in if
                          | T.null e -> cont
                          | "eval sys_lib_search_path=" `T.isPrefixOf` e ->
                              pref <> "sys_lib_search_path=" <> sp
                          | "search_path=" `T.isInfixOf` e -> pref
                          | otherwise -> cont
              )
            . T.lines
        )

configureFunc :: FilePath -> [String] -> Maybe Text -> Maybe FilePath -> BIO ()
configureFunc script flags fixLt pref = liftIO $ do
  mapM_ fixLibtool fixLt
  mapM_ (createDirectoryIfMissing True) pref
  echo ("config flag: " : flags)
  callProcess script flags

data MakeCfg = MakeCfg
  { mcMakeFile :: FilePath,
    mcEnableParallel :: Bool
  }
  deriving (Show, Lift)

runMake :: String -> [String] -> MakeCfg -> BIO ()
runMake name fs ma = do
  flags <-
    if mcEnableParallel ma
      then
        asks beBuildCores
          <&> \c -> "-j" : show c : fs
      else pure fs
  liftIO
    ( do
        echo ((name ++ " flags: ") : flags)
        callProcess "make" ("-f" : mcMakeFile ma : flags)
    )

installFunc :: [FilePath] -> [String] -> MakeCfg -> BIO ()
installFunc dest f ma =
  liftIO (traverse_ createDirectory dest)
    >> runMake "install" f ma

distFunc :: [String] -> MakeCfg -> Maybe (FilePath, [FilePath]) -> BIO ()
distFunc f ma mda =
  runMake "dist" f ma
    >> case mda of
      Just (dest, tar) ->
        liftIO
          ( let d = dest </> "tarballs"
             in createDirectory d
                  >> traverse_
                    (\tf -> copyFile tf (d </> tf))
                    tar
          )
      Nothing -> pure ()