#!/bin/sh

echo 'launch server..'
. /home/nix/.nix-profile/etc/profile.d/nix.sh && nix-shell --verbose --command "server --cfg_path deploy/config.yaml --path_to_katip deploy --path_to_jwk deploy --cfg_admin_storage_path deploy/admin_storage --print_cfg y"