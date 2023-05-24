#!/bin/sh

{ # Prevent execution if this script was only partially downloaded
oops() {
    echo "$0:" "$@" >&2
    exit 1
}

require_util() {
    command -v "$1" > /dev/null 2>&1 ||
        oops "you do not have '$1' installed, which I need to $2"
}

tmpDir="$(mktemp -d -t gmplib-unpack.XXXXXXXXXX || \
          oops "Can't create temporary directory for downloading the gmplib binary tarball")"
cleanup() {
    rm -rf "$tmpDir"
}
trap cleanup EXIT INT QUIT TERM

url="https://gmplib.org/download/gmp/gmp-6.2.1.tar.xz"

tarball="$tmpDir/$(basename "$tmpDir/gmp-6.2.1.tar.xz")"

require_util curl "download the binary tarball"
require_util tar "unpack the binary tarball"

echo "downloading gmp-6.2.1 binary tarball for $system from '$url' to '$tmpDir'..."
curl -L "$url" -o "$tarball" || oops "failed to download '$url'"

unpack=$tmpDir/unpack
mkdir -p "$unpack"
tar -xJf "$tarball" -C "$unpack" || oops "failed to unpack '$url'"

script=$(echo "$unpack"/*/./configure && make make prefix=/usr libdir=/usr/lib64 && make install --prefix=/usr --libdir=/usr/lib64 && ldconfig /usr/lib64)

[ -e "$script" ] || oops "script failed to start!"
"$script" "$@"
}
