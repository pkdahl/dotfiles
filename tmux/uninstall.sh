#!/usr/bin/env bash

. ../helpers.sh

CURRENT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
DOT_TMUX="$CURRENT_DIR"

if [ -L "$HOME/.tmux.conf" ]; then
	echo_info "Removing .tmux.conf"
	rm -f "$HOME/.tmux.conf"
fi

if [ -L "$HOME/.tmux" ]; then
	echo_info "Removing ~/.tmux link"
	rm -f "$HOME/.tmux"
fi

echo_done "Removed tmux"
