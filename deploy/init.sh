#!/bin/sh

[[ -z "${MUTE_500}" ]] && mute500='True' || mute500="${MUTE_500}"

[[ -z "${YAML_ENV}" ]] && env_yaml='env.yaml' || env_yaml="${YAML_ENV}"


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
        --env_path $env_yaml \
        --mute500 $mute500"