#!/usr/bin/env bash

. ../helpers.sh

echo_info "Setting up Neomutt"

CURRENT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DOT_NEOMUTT="$CURRENT_DIR"

brew install neomutt

NEOMUTT_CONFIG_HOME="$HOME/.config/neomutt"

if ! [ -d "$NEOMUTT_CONFIG_HOME" ]; then
    echo_info "Creating $NEOMUTT_CONFIG_HOME"
    mkdir -p "$NEOMUTT_CONFIG_HOME"
fi

if ! [ -L "$NEOMUTT_CONFIG_HOME/neomuttrc" ]; then
    echo_info "Linking neomuttrc"
    ln -sf "$DOT_NEOMUTT/neomuttrc" "$NEOMUTT_CONFIG_HOME/neomuttrc"
fi

if ! [ -L "$NEOMUTT_CONFIG_HOME/uio-perkda" ]; then
    ln -sf "$DOT_NEOMUTT/uio-perkda" "$NEOMUTT_CONFIG_HOME/uio-perkda"
fi

if ! [ -L "$NEOMUTT_CACHE_HOME/uio-pkdahl" ]; then
    echo_info "Linking uio-pkdahl settings"
    ln -sf "$DOT_NEOMUTT/uio-pkdahl" "$NEOMUTT_CONFIG_HOME/uio-pkdahl"
fi

NEOMUTT_CACHE_HOME="$HOME/.cache/neomutt"

if ! [ -d "$NEOMUTT_CACHE_HOME" ]; then
    echo_info "Creating $NEOMUTT_CACHE_HOME"
    mkdir -p "$NEOMUTT_CACHE_HOME"
fi

unset NEOMUTT_CONFIG_HOME
unset NEOMUTT_CACHE_HOME
unset DOT_NEOMUTT
unset CURRENT_DIR
