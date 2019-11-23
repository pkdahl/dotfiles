#!/usr/bin/env bash

. ../helpers.sh

echo_info "Setting up mail"

CURRENT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DOT_MAIL="$CURRENT_DIR"

brew install isync

MAIL_DIR="$HOME/Mail"

mkdir -p "$MAIL_DIR/gmail"
mkdir -p "$MAIL_DIR/uio-perkda"
mkdir -p "$MAIL_DIR/uio-pkdahl"

if ! [ -L "$HOME/.mbsyncrc" ]; then
    ln -sf "$DOT_MAIL/mbsyncrc" "$HOME/.mbsyncrc"
fi

unset MAIL_DIR
unset DOT_MAIL
unset CURRENT_DIR

./neomutt/install.sh
