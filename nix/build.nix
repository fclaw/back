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
  openapi-generator = import ./openapi3-haskell.nix { inherit pkgs; }; 
in
pkgs.mkShell {
  buildInputs = [
    stack-wrapped
    openapi-generator
    pkgs.glibcLocalesUtf8
    pkgs.glibcLocales
    pkgs.python311
    pkgs.haskell.compiler.ghc927
  ];
  # Configure the Nix path to our own `pkgs`, to ensure Stack-with-Nix uses the correct one rather than the global <nixpkgs> when looking for the right `ghc` argument to pass in `nix/stack-integration.nix`
  # See https://nixos.org/nixos/nix-pills/nix-search-paths.html for more information
  NIX_PATH = "nixpkgs=" + pkgs.path;
  shellHook = ''
     echo "Welcome to server shell!!"
  '';
}