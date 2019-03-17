#!/usr/bin/env bash

. ../helpers.sh

DOT_SSH=$(pwd)
SSH_CONFIG_HOME="$HOME/.ssh"

if [ -L "$SSH_CONFIG_HOME/.config" ]; then
	echo_info "Removing .ssh/config"
	rm -f "$SSH_CONFIG_HOME/config"
fi

if [ -d "$SSH_CONFIG_HOME" ]; then
	if [ -z $(ls -A "SSH_CONFIG_HOME") ]; then
		echo_info "Removing $SSH_CONFIG_HOME"
		rmdir "$SSH_CONFIG_HOME"
	fi
fi

echo_done "Removed SSH setup"
