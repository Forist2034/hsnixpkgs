module Utils (
  simplePkgDesc,
  unConditionalSubLib,
) where

import Data.Maybe
import Distribution.CabalSpecVersion
import Distribution.License
import Distribution.PackageDescription
import Distribution.Utils.Path
import Distribution.Version
import HsCabal

simplePkgDesc :: PackageName -> [Int] -> PackageDescription
simplePkgDesc n v =
  emptyPackageDescription
    { specVersion = CabalSpecV3_4,
      package = PackageIdentifier n (mkVersion v),
      licenseRaw = Right MIT,
      licenseFiles = [unsafeMakeSymbolicPath "LICENSE"],
      maintainer = "dariankline@outlook.com",
      author = "Jose Jane",
      buildTypeRaw = Just Simple
    }

unConditionalSubLib :: Library -> (UnqualComponentName, CondTree ConfVar [Dependency] Library)
unConditionalSubLib l =
  ( fromJust (libraryNameString (libName l)),
    unConditional l
  )