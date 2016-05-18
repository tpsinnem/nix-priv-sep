let pkgs = (import <nixpkgs> {}); in

{ stack ? pkgs.haskellPackages.stack }:

pkgs.stdenv.mkDerivation {
  name = "xephyr-launcher-0.1.0.0";
  src = ./builddir;
  buildInputs = [pkgs.haskell.compiler.ghc7103];
  outputHashAlgo = "sha512";
  outputHashMode = "recursive";
  outputHash = "39c57aa9d0d69c20b62bf6c2de814255bd7386ab7f11522982cd0b80d1ba3b57" +
               "647d6db1ca4c6ecbfca37b59af64f334ea037de93c1c2d726056251a4805764b";
  buildPhase = ''
    export HOME=`pwd`
    cd src
    ${stack}/bin/stack install'';
  installPhase = ''
    mkdir -p $out/bin
    cp $HOME/.local/bin/xephyr-server-launcher $out/bin
    cp $HOME/.local/bin/xephyr-client-launcher $out/bin''; }
