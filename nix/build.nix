let
  sources = import ./sources.nix;
  pkgs = import sources.nixpkgs { };
in
let
  # Wrap Stack to configure Nix integration and target the correct Stack-Nix file
  #
  # - nix: Enable Nix support
  # - no-nix-pure: Pass environment variables, like `NIX_PATH`
  # - nix-shell-file: Specify the Nix file to use (otherwise it uses `shell.nix` by default)
  stack-wrapped = pkgs.symlinkJoin {
    name = "stack";
    paths = [ pkgs.stack ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/stack \
        --add-flags "\
          --nix \
          --no-nix-pure \
          --nix-shell-file=nix/stack-integration.nix \
        "
    '';
  };

  openapi3-code-generator = 
    pkgs.stdenv.mkDerivation {
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
      buildInputs = [pkgs.stack pkgs.llvm pkgs.glib pkgs.haskell.compiler.ghc90];
      installPhase = ''
        mkdir -p $out/openapi3
        stack --stack-root $out/openapi3 --verbosity info --system-ghc --local-bin-path $out/bin install --fast -j12
      '';
    };

in
pkgs.mkShell {
  buildInputs = [
    stack-wrapped 
    openapi3-code-generator
  ];
  # Configure the Nix path to our own `pkgs`, to ensure Stack-with-Nix uses the correct one rather than the global <nixpkgs> when looking for the right `ghc` argument to pass in `nix/stack-integration.nix`
  # See https://nixos.org/nixos/nix-pills/nix-search-paths.html for more information
  NIX_PATH = "nixpkgs=" + pkgs.path;
  shellHook =
  '' echo "Welcome to server shell!!"
  '';
}