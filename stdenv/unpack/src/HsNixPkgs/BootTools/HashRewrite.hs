{-# LANGUAGE CPP #-}

module HsNixPkgs.BootTools.HashRewrite
  ( RewriteSpec (..),
    decodeRewriteSpec,
    rewriteHash,
    rewriteLBSVerbose,
  )
where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Unsafe as BSU
import Data.Foldable
import Data.Functor
import qualified Data.List as L
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Word
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import System.Environment

data RewriteSpec = RewriteSpec
  { rsName :: Text,
    rsPattern :: ByteString,
    rsReplace :: ByteString
  }
  deriving (Show)

#include "store_path.h"

decodeRewriteSpec :: Text -> IO [RewriteSpec]
decodeRewriteSpec txt =
  let (self, other) =
        let ls = T.lines txt
            nSelf = read (T.unpack (head ls))
            (s, o) = L.splitAt (nSelf + 1) ls
         in ( tail s,
              L.take (read (T.unpack (head o))) (tail o)
            )
   in do
        store <- T.pack <$> getEnv "NIX_STORE"
        selfRule <-
          traverse
            ( \l ->
                case T.words l of
                  [o, p] -> do
                    rep <- getEnv (T.unpack o)
                    pure
                      ( RewriteSpec
                          { rsName = "output:" <> o,
                            rsPattern = getHash store p,
                            rsReplace = getHash store (T.pack rep)
                          }
                      )
                  _ -> error ("Invalid output rewrite rule " ++ show l)
            )
            self
        pure
          ( selfRule
              ++ fmap
                ( \l ->
                    case T.words l of
                      [n, p, r] ->
                        RewriteSpec
                          { rsName = n,
                            rsPattern = getHash store p,
                            rsReplace = getHash store r
                          }
                      _ -> error ("Invalid rewrite rule " ++ show l)
                )
                other
          )
  where
    getHash store =
      ( \t ->
          if BS.length t == STORE_PATH_HASH_LEN
            then t
            else error ("Invalid hash " ++ show t)
      )
        . TE.encodeUtf8
        . fst
        . T.span (`elem` ("0123456789abcdfghijklmnpqrsvwxyz" :: String))
        . T.tail -- remove /
        . fromJust
        . T.stripPrefix store

foreign import ccall unsafe "hs_hash_rewrite"
  c_rewrite :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr Word8 -> IO ()

rewriteHash :: Ptr Word8 -> CSize -> RewriteSpec -> IO ()
rewriteHash fp len rs =
  BSU.unsafeUseAsCString
    (rsPattern rs)
    ( \pat ->
        BSU.unsafeUseAsCString
          (rsReplace rs)
          (c_rewrite fp len (castPtr pat) . castPtr)
    )

rewriteLBSVerbose :: [RewriteSpec] -> LBS.ByteString -> IO ByteString
rewriteLBSVerbose rs bs = do
  let len = fromIntegral (LBS.length bs)
  buf <- mallocBytes len
  void
    ( foldlM
        ( \p c ->
            BSU.unsafeUseAsCStringLen
              c
              ( \(ptr, l) ->
                  copyBytes p (castPtr ptr) l
                    $> plusPtr p l
              )
        )
        buf
        (LBS.toChunks bs)
    )
  let siz = fromIntegral (LBS.length bs)
  traverse_
    ( \r ->
        TIO.putStrLn
          ( "Applying rewrite rule "
              <> rsName r
              <> ": "
              <> T.pack (show (rsPattern r))
              <> " -> "
              <> T.pack (show (rsReplace r))
          )
          >> rewriteHash buf siz r
    )
    rs
  BSU.unsafePackMallocCStringLen (castPtr buf, len)