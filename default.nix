{ nixpkgs ? import <nixpkgs> { } }: nixpkgs.stdenv.mkDerivation {
  name = "threatlevel";
  buildInputs = [ nixpkgs.zlib ];
}
