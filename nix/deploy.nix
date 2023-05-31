let
   pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-23.05.tar.gz") {};
in
pkgs.mkShell { 
  buildInputs = [ pkgs.postgresql pkgs.lzma pkgs.zlib pkgs.imagemagick ];
  }