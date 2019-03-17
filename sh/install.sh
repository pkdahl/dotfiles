#!/usr/bin/env bash

DOT_SH=$(pwd)

. ../helpers.sh

echo_info "Linking .profile"
ln -sf "$DOT_SH/profile" "$HOME/.profile"

echo_info "Linking .envir"
ln -sf "$DOT_SH/envir" "$HOME/.envir"

echo_done "Setup sh"
