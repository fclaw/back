let
  sources = import ./sources.nix;
  pkgs = import sources.nixpkgs { };

  gmp = pkgs.gmp.overrideAttrs (old: {
      name = "gmp-6.2.1";
      src = fetchurl {
        url = https://gmplib.org/download/gmp/gmp-6.2.1.tar.xz;
        sha256 = "0x2g1jqygyr5wiwg4ma1nd7w4ydpy82z9gkcv8vh2v8dn3y58v5m";
       };
    });
in

# See https://docs.haskellstack.org/en/stable/nix_integration/#using-a-custom-shellnix-file
{ ghc }:

pkgs.haskell.lib.buildStackProject {

  inherit ghc;
  name = "haskell-stack-nix";
  # System dependencies needed at compilation time
  buildInputs = [ pkgs.postgresql pkgs.lzma pkgs.git pkgs.zlib pkgs.imagemagick gmp ];
}
