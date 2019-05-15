#!/usr/bin/env bash

. ../helpers.sh

CURRENT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
DOT_TMUX="$CURRENT_DIR"
DOT_TMUX_PLUGIN_DIR="$DOT_TMUX/plugins"

if [ ! -L "$HOME/.tmux.conf" ]; then
	echo_info "Linking .tmux.conf"
	ln -sf "$DOT_TMUX/tmux.conf" "$HOME/.tmux.conf"
fi

if [ ! -L "$HOME/.tmux" ]; then
	echo_info "Linking .tmux directory"
	ln -sf "$DOT_TMUX" "$HOME/.tmux"
fi

function install_plugin () {
	local src=$1
	local plugin=$2
	if [ ! -d "$DOT_TMUX_PLUGIN_DIR/$plugin" ]; then
		echo_info "Installing $plugin"
		mkdir -p "$DOT_TMUX_PLUGIN_DIR"
		git clone "$src" "$DOT_TMUX_PLUGIN_DIR/$plugin"
	fi
}

install_plugin https://github.com/tmux-plugins/tmux-resurrect.git resurrect
install_plugin https://github.com/tmux-plugins/tmux-continuum.git continuum

if [ ! -d "$DOT_TMUX/themes/nord" ]; then
    echo_info "Installing Nord theme"
    git clone https://github.com/arcticicestudio/nord-tmux "$DOT_TMUX/themes/nord"
fi

echo_done "Setup tmux"
