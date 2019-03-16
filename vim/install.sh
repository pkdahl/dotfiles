#!/usr/bin/env bash

. ../helpers.sh

VIM_CACHE_HOME=$HOME/.cache/vim
DOT_VIM_DIR=$(pwd)

if [[ ! -d $VIM_CACHE_HOME ]]; then
	echo_info "Creating $VIM_CACHE_HOME"
	mkdir -p $VIM_CACHE_HOME
fi

if [[ ! -L $HOME/.vim ]]; then
	echo_info "Symlinking .vim"
	ln -sf $DOT_VIM_DIR $HOME/.vim
fi
