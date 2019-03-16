#!/usr/bin/env bash

DOT_SH=$(pwd)

. ../helper.sh

echo_info "Linking .profile"
ln -sf $DOT_SH/profile $HOME/.profile

echo_done "Setup sh"
