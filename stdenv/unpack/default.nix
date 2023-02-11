{ mkDerivation, base, binary, bytestring, lib, lzma, nix-archive
, text
}:
mkDerivation {
  pname = "hsnixpkgs-stdenv-unpack";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base bytestring text ];
  executableHaskellDepends = [
    base binary bytestring lzma nix-archive text
  ];
  license = lib.licenses.mit;
  preBuild = ''
    rm LICENSE
    cp ${../../LICENSE} LICENSE
    '';
  mainProgram = "unpack-stdenv-linux";
}
