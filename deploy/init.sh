#!/bin/bash

# export ekg_datadir=server/deploy/ekg
cd server

echo 'launch server..'
nix-shell --command "./bin/server --cfg_path deploy/config.yaml --path_to_katip deploy --path_to_jwk deploy --cfg_admin_storage_path deploy/admin_storage"