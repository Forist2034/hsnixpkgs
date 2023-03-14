{
  hashable =
    { mkDerivation, base, base-orphans, bytestring, containers
    , data-array-byte, deepseq, filepath, ghc-bignum
    , ghc-bignum-orphans, ghc-prim, HUnit, lib, QuickCheck, random
    , test-framework, test-framework-hunit, test-framework-quickcheck2
    , text, unix
    }:
    mkDerivation {
      pname = "hashable";
      version = "1.4.2.0";
      sha256 = "1b4000ea82b81f69d46d0af4152c10c6303873510738e24cfc4767760d30e3f8";
      libraryHaskellDepends = [
        base base-orphans bytestring containers data-array-byte deepseq
        filepath ghc-bignum ghc-prim text
      ];
      testHaskellDepends = [
        base bytestring ghc-prim HUnit QuickCheck random test-framework
        test-framework-hunit test-framework-quickcheck2 text unix
      ];
      homepage = "http://github.com/haskell-unordered-containers/hashable";
      description = "A class for types that can be converted to a hash value";
      license = lib.licenses.bsd3;
    };
  hspec-core =
    { mkDerivation, ansi-terminal, array, base, base-orphans
    , call-stack, deepseq, directory, filepath, haskell-lexer
    , hspec-expectations, hspec-meta, HUnit, lib, process, QuickCheck
    , quickcheck-io, random, setenv, silently, stm, temporary
    , tf-random, time, transformers
    }:
    mkDerivation {
      pname = "hspec-core";
      version = "2.10.9";
      sha256 = "ff7e28bc23c19988aad83d0d1ba700844b43e7d7c10ab5cb590bdf94bb127ea9";
      libraryHaskellDepends = [
        ansi-terminal array base call-stack deepseq directory filepath
        haskell-lexer hspec-expectations HUnit process QuickCheck
        quickcheck-io random setenv stm tf-random time transformers
      ];
      testHaskellDepends = [
        ansi-terminal array base base-orphans call-stack deepseq directory
        filepath haskell-lexer hspec-expectations hspec-meta HUnit process
        QuickCheck quickcheck-io random setenv silently stm temporary
        tf-random time transformers
      ];
      testToolDepends = [ hspec-meta ];
      testTarget = "--test-option=--skip --test-option='Test.Hspec.Core.Runner.hspecResult runs specs in parallel'";
      homepage = "https://hspec.github.io/";
      description = "A Testing Framework for Haskell";
      license = lib.licenses.mit;
    };
  hspec-discover =
    { mkDerivation, base, directory, filepath, hspec-meta, lib, mockery
    , QuickCheck
    }:
    mkDerivation {
      pname = "hspec-discover";
      version = "2.10.9";
      sha256 = "f279368d7b3a1b03cc29e8fc07947cb146b555f9fb6e0f17b8d306f9787c5099";
      isLibrary = true;
      isExecutable = true;
      libraryHaskellDepends = [ base directory filepath ];
      executableHaskellDepends = [ base directory filepath ];
      testHaskellDepends = [
        base directory filepath hspec-meta mockery QuickCheck
      ];
      testToolDepends = [ hspec-meta ];
      homepage = "https://hspec.github.io/";
      description = "Automatically discover and run Hspec tests";
      license = lib.licenses.mit;
      mainProgram = "hspec-discover";
    };
  hspec =
    { mkDerivation, base, hspec-core, hspec-discover
    , hspec-expectations, lib, QuickCheck
    }:
    mkDerivation {
      pname = "hspec";
      version = "2.10.9";
      sha256 = "9431bdbafa152cfab9e203a98d701278c2c34e7c215dda9778db1e77f901716d";
      libraryHaskellDepends = [
        base hspec-core hspec-discover hspec-expectations QuickCheck
      ];
      homepage = "https://hspec.github.io/";
      description = "A Testing Framework for Haskell";
      license = lib.licenses.mit;
    };
}