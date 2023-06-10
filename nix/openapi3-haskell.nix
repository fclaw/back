{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation rec {
    name = "openapi3-code-generator";
    # fetchFromGitHub is a build support function that fetches a GitHub
    # repository and extracts into a directory; so we can use it
    # fetchFromGithub is actually a derivation itself :)
    src = pkgs.fetchFromGitHub {
    owner = "Haskell-OpenAPI-Code-Generator";
    repo = "Haskell-OpenAPI-Client-Code-Generator";
    rev = "3a2b41fb4e26d47ab9ca64711565dafedf961a3e";
    sha256 = "sha256-b7sVdv0NWgC7cqKhduNZOYMroMagzvSu0cqItOJP5yg=";
    };
    buildInputs = [pkgs.stack pkgs.llvm pkgs.glib pkgs.haskell.compiler.ghc90 pkgs.which];
    installPhase = ''
    mkdir -p $out/openapi3
    stack --stack-root $out/openapi3 --verbosity info --system-ghc --local-bin-path $out/bin install --fast -j12
    '';
}