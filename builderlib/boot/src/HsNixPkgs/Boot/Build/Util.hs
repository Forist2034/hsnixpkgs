{-# LANGUAGE LambdaCase #-}

module HsNixPkgs.Boot.Build.Util
  ( assertE,
    assertEIO,
    echo,
    waitForProcSuccess,
    callCreateProcess,
    pipeCreateProcess,
    listDirRec,
    changePermissions,
    modifyTextFile,
  )
where

import Control.Exception
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Posix.Files
import System.Process

assertE :: Exception e => Bool -> e -> ()
assertE True _ = ()
assertE False e = throw e

assertEIO :: Exception e => Bool -> e -> IO ()
assertEIO True _ = pure ()
assertEIO False e = throwIO e

echo :: [String] -> IO ()
echo v = putStrLn (concat v)

data NonzeroExit = NonzeroExit CreateProcess Int
  deriving (Show)

instance Exception NonzeroExit

waitForProcSuccess :: CreateProcess -> ProcessHandle -> IO ()
waitForProcSuccess f ph =
  waitForProcess ph >>= \case
    ExitSuccess -> pure ()
    ExitFailure e -> throwIO (NonzeroExit f e)

callCreateProcess :: CreateProcess -> IO ()
callCreateProcess cp =
  withCreateProcess cp (\_ _ _ ph -> waitForProcSuccess cp ph)

pipeCreateProcess :: StdStream -> (Maybe Handle -> IO ()) -> [CreateProcess] -> StdStream -> IO ()
pipeCreateProcess _ _ [] _ = pure ()
pipeCreateProcess hi fhi [cp] ho =
  withCreateProcess
    (cp {std_in = hi, std_out = ho})
    ( \i _ _ h ->
        fhi i >> waitForProcSuccess cp h
    )
pipeCreateProcess hi fhi (cp : cps) ho =
  withCreateProcess
    (cp {std_in = hi, std_out = CreatePipe})
    ( \i o _ h -> do
        fhi i
        iter (fromJust o) cps
        waitForProcSuccess cp h
    )
  where
    iter _ [] = pure ()
    iter i [x] =
      withCreateProcess
        (x {std_in = UseHandle i, std_out = ho})
        (\_ _ _ h -> waitForProcSuccess x h)
    iter i (x : xs) =
      withCreateProcess
        (x {std_in = UseHandle i, std_out = CreatePipe})
        ( \_ o _ h ->
            iter (fromJust o) xs
              >> waitForProcSuccess x h
        )

listDirRec :: FilePath -> IO [FilePath]
listDirRec f = do
  fs <- isDirectory <$> getSymbolicLinkStatus f
  if fs
    then
      (f :) . concat
        <$> ( listDirectory f
                >>= traverse (listDirRec . (f </>))
            )
    else pure [f]

modifyM ::
  Monad m =>
  (t -> m a) ->
  (t -> a -> m ()) ->
  (a -> a) ->
  t ->
  m ()
modifyM r w f v = r v >>= w v . f

changePermissions :: (Permissions -> Permissions) -> FilePath -> IO ()
changePermissions = modifyM getPermissions setPermissions

modifyTextFile :: (Text -> Text) -> FilePath -> IO ()
modifyTextFile = modifyM TIO.readFile TIO.writeFile