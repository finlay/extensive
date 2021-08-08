{ mkDerivation, algebra, base, boxes, criterion, lib, mtl, pretty
, QuickCheck, random
}:
mkDerivation {
  pname = "extensive";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    algebra base boxes mtl pretty QuickCheck
  ];
  executableHaskellDepends = [
    algebra base boxes criterion QuickCheck random
  ];
  description = "Linear algebra following Anders Kock";
  license = lib.licenses.bsd3;
}
