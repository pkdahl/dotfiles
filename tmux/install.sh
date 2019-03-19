#!/usr/bin/env bash

. ../helpers.sh

DOT_TMUX=$(pwd)

if [ ! -L "$HOME/.tmux.conf" ]; then
	echo_info "Linking .tmux.conf"
	ln -sf "$DOT_TMUX/tmux.conf" "$HOME/.tmux.conf"
fi

echo_done "Setup tmux"
