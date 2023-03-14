{ mkDerivation, aeson, async, base, binary, bytestring, cryptonite
, directory, filepath, hashable, lib, lzma, mtl, nix-archive, text
, unordered-containers, yaml
}:
mkDerivation {
  pname = "hsnixpkgs-stdenv-pack";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base binary bytestring cryptonite directory filepath
    hashable lzma mtl nix-archive text unordered-containers
  ];
  executableHaskellDepends = [
    base bytestring directory filepath text yaml
  ];
  preBuild = ''
    rm LICENSE
    cp ${../../LICENSE} LICENSE
    '';
  doHaddock = false;
  license = lib.licenses.mit;
  mainProgram = "pack-stdenv-linux";
}
