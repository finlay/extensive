{ mkDerivation, algebra, base, boxes, criterion, mtl, pretty
, QuickCheck, random, stdenv
}:
mkDerivation {
  pname = "extensive";
  version = "0.1.0.0";
  src = /home/finlay/haskell/extensive;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    algebra base boxes mtl pretty QuickCheck
  ];
  executableHaskellDepends = [
    algebra base boxes criterion QuickCheck random
  ];
  description = "Linear algebra following Anders Kock";
  license = stdenv.lib.licenses.bsd3;
}
