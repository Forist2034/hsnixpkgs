{ mkDerivation, base, binary, bytestring, containers, directory
, fetchgit, filepath, hashable, hspec, lib, template-haskell, unix
}:
mkDerivation {
  pname = "nix-archive";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/forist2034/nix-core.git";
    sha256 = "0jv4g458ch5zynkj9li28gdzjz7m0w05394brga1gahz3qxyk2j6";
    rev = "0ea47cacbc4cb494211bb051fed24e2fd52baf05";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/nix-archive; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base binary bytestring containers directory filepath hashable
    template-haskell unix
  ];
  testHaskellDepends = [
    base binary bytestring containers filepath hspec
  ];
  license = lib.licenses.mit;
}