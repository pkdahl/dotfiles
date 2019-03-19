#!/usr/bin/env bash

. ../helpers.sh

DOT_TMUX=$(pwd)

if [ -L "$HOME/.tmux.conf" ]; then
	echo_info "Removing .tmux.conf"
	rm -f "$HOME/.tmux.conf"
fi

echo_done "Removed tmux"
