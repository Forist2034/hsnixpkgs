module HsNixPkgs.Boot.Build.Hook (Hook (..), runHook) where

import HsNixPkgs.Boot.Build.Main

data Hook = Hook
  { preHook :: BIO (),
    postHook :: BIO ()
  }

runHook :: Hook -> BIO () -> BIO ()
runHook h f = preHook h >> (f <* postHook h)