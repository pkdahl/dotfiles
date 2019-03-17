#!/usr/bin/env bash

. ../helpers.sh

DOT_SSH=$(pwd)
SSH_CONFIG_HOME="$HOME/.ssh"

if [ ! -d "$SSH_CONFIG_HOME" ]; then
	echo_info "Creating .ssh directory"
	mkdir -p "$SSH_CONFIG_HOME"
fi

echo_info "Linking .ssh/config"
ln -sf "$DOT_SSH/config" "$SSH_CONFIG_HOME/config"

echo_done "Setup SSH"
