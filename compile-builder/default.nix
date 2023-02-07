{ mkDerivation, base, directory, filepath, lib, process }:
mkDerivation {
  pname = "compile-builder";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  enableSharedExecutables = false;
  executableHaskellDepends = [ base directory filepath process ];
  license = lib.licenses.mit;
  doHaddock = false;
  preBuild = ''
    rm LICENSE
    cp ${../LICENSE} LICENSE
    '';
  mainProgram = "compile-builder";
}
