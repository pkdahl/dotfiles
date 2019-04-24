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

# Install packs

VIM_PACK_DIR=$DOT_VIM_DIR/pack

# if [ ! -d $DOT_VIM_DIR/pack/themes/opt/solarized8 ]; then
#	echo_info "Installing solarized8"
#	mkdir -p $DOT_VIM_DIR/pack/themes/opt
#	git clone https://github.com/lifepillar/vim-solarized8.git $DOT_VIM_DIR/pack/themes/opt/solarized8
# fi

if [ ! -d $VIM_PACK_DIR/convenience/start/commentray ]; then
	echo_info "Installing commentary"
	mkdir -p $VIM_PACK_DIR/convenience/start
	git clone https://github.com/tpope/vim-commentary.git \
	          $VIM_PACK_DIR/convenience/start/commentary
	vim -u NONE -c "helptags commentary/doc" -c q
fi

echo_done "Setup vim"
