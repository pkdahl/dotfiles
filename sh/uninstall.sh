#!/usr/bin/env bash

echo_info "Removing .profile"
[ -L "$HOME/.profile" ] && rm -f "$HOME/.profile"

echo_info "Removing .envir"
[ -L "$HOME/.envir ] && rm -f "$HOME/.envir"

echo_done "Removed sh setup"
