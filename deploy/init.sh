#!/bin/sh

echo 'launch server..'
. /home/nix/.nix-profile/etc/profile.d/nix.sh && \
  nix-shell deploy.nix \
    --log-format bar-with-logs \
    --verbose \
    --command \
    "./bin/server \
        --cfg_path deploy/config.yaml \
        --path_to_katip deploy \
        --path_to_jwk deploy \
        --cfg_admin_storage_path deploy/admin_storage \
        --print_cfg y \
        --env_path env.yaml"