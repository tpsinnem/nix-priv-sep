let pkgs = (import <nixpkgs> {}); in

{ stack ? pkgs.haskellPackages.stack }:

pkgs.stdenv.mkDerivation {
  name = "xephyr-launcher-0.1.0.0";
  src = ./builddir;
  buildInputs = [pkgs.haskell.compiler.ghc7103];
  buildPhase = ''
    export HOME=`pwd`
    cd src
    ${stack}/bin/stack install'';
  installPhase = ''
    mkdir -p $out/bin
    cp $HOME/.local/bin/xephyr-server-launcher $out/bin
    cp $HOME/.local/bin/xephyr-client-launcher $out/bin''; }
