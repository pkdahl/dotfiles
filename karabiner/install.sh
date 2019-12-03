#!/usr/bin/env bash

. ../helpers.sh

CURRENT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

DOT_KARABINER="$CURRENT_DIR"
KARABINER_CONFIG_HOME="$XDG_CONFIG_HOME/karabiner"

if ! [ -L "$KARABINER_CONFIG_HOME/karabiner.json" ]; then
    echo_info "Linking karabiner.json"
    mkdir -p "$KARABINER_CONFIG_HOME"
    ln -sf "$DOT_KARABINER/karabiner.json" \
        "$KARABINER_CONFIG_HOME/karabiner.json"
fi
