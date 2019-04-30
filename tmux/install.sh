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

function _ensure_plugin_dir () {
	if [ ! -d "$DOT_TMUX_PLUGIN_DIR"]; then
		echo_info "Creating directory for Tmux plugins"
		mkdir -p "$DOT_TMUX_PLUGIN_DIR"
	fi
}

function install_plugin () {
	_ensure_plugin_dir
	local src=$1
	local plugin=$2
	echo_info "Installing $plugin"
	git clone "$src" "$DOT_TMUX_PLUGIN_DIR/$plugin"
}

install_plugin https://github.com/tmux-plugins/tmux-resurrect.git resurrect
install_plugin https://github.com/tmux-plugins/tmux-continuum.git continuum

echo_done "Setup tmux"
