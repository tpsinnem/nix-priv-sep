with import <nixpkgs> {};

{
  slim = callPackage ./slim { libpng = libpng12; };
}
