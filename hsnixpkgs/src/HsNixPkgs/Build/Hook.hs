{-# LANGUAGE TemplateHaskellQuotes #-}

module HsNixPkgs.Build.Hook (Hook (..), runHook) where

import Data.Default
import qualified HsNixPkgs.Boot.Build.Hook as B
import HsNixPkgs.Build.Main (BIO)
import HsNixPkgs.HsBuilder.Generate
import Language.Haskell.TH

data Hook t = Hook
  { preHook :: Maybe (Code HsQ (BIO ())),
    postHook :: Maybe (Code HsQ (BIO ()))
  }

instance Default (Hook t) where
  def = Hook {preHook = Nothing, postHook = Nothing}

mergeH ::
  Maybe (Code HsQ (BIO ())) ->
  Maybe (Code HsQ (BIO ())) ->
  Maybe (Code HsQ (BIO ()))
mergeH Nothing Nothing = Nothing
mergeH Nothing r = r
mergeH l Nothing = l
mergeH (Just l) (Just r) =
  Just
    ( unsafeCodeCoerce
        [|$(unTypeCode l) >> $(unTypeCode r)|]
    )

instance Semigroup (Hook t) where
  l <> r =
    Hook
      { preHook = mergeH (preHook l) (preHook r),
        postHook = mergeH (postHook l) (postHook r)
      }

instance Monoid (Hook t) where
  mempty = def

runHook ::
  Hook t ->
  Code HsQ (BIO ()) ->
  Code HsQ (BIO ())
runHook Hook {preHook = Nothing, postHook = Nothing} e = e
runHook h e =
  let pre = maybe [|pure ()|] unTypeCode (preHook h)
      post = maybe [|pure ()|] unTypeCode (postHook h)
   in unsafeCodeCoerce
        [|
          B.runHook
            B.Hook
              { B.preHook = $pre,
                B.postHook = $post
              }
            $(unTypeCode e)
          |]