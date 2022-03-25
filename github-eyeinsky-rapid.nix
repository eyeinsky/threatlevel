{ mkDerivation, async, base, containers, fetchgit, foreign-store
, lib, stm
}:
mkDerivation {
  pname = "rapid";
  version = "0.1.4";
  src = fetchgit {
    url = "https://github.com/eyeinsky/rapid.git";
    sha256 = "11k7q3ykxw28l2bjc8yc45avpq8l8vn7gpgb9lpfd5dyhpnmvvbd";
    rev = "dcf0b75e306a33a08e4405de4c9f2577331a9bda";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    async base containers foreign-store stm
  ];
  homepage = "https://github.com/esoeylemez/rapid";
  description = "Rapid prototyping with GHCi: hot reloading of running components and reload-surviving values";
  license = lib.licenses.bsd3;
}
