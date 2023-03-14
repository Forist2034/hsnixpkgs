{ mkDerivation, base, bytestring, deepseq, directory, exceptions
, filepath, lib, mtl, process, template-haskell, text, time, unix
}:
mkDerivation {
  pname = "hsnixpkgs-builderlib-boot";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring deepseq directory exceptions filepath mtl process
    template-haskell text time unix
  ];
  preBuild = ''
    rm LICENSE
    cp ${../../LICENSE} LICENSE
    '';
  license = lib.licenses.mit;
}
