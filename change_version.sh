#!/bin/bash

MAC_RESOLVER="nightly-2022-01-15"

if [[ -z $1 ]]; then
    echo "Usage: $0 <platform>"
    exit 1
fi

if [[ $1 == "linux" ]]; then
    cp stack_yamls/* .
elif [[ $1 == "mac" ]]; then
    cp stack_yamls/* .
    sed -i '' -e "s/^resolver:.*/resolver: $MAC_RESOLVER/" stack.yaml
    FFI_PATH="$(xcrun --show-sdk-path)/usr/include/ffi"
    echo "extra-include-dirs:" >> stack.yaml
    echo "- $FFI_PATH" >> stack.yaml
else
    echo "Invalid platform. Supported platforms: linux, mac."
    exit 1
fi

exit 0
