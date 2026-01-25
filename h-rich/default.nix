{ mkDerivation, base, containers, lib, megaparsec, terminal-size
, text
}:
mkDerivation {
  pname = "h-rich";
  version = "0.1.0.0";
  src = ./h-rich;
  libraryHaskellDepends = [
    base containers megaparsec terminal-size text
  ];
  testHaskellDepends = [ base text ];
  license = lib.licenses.bsd3;
}
