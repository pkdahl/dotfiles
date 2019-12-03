#!/usr/bin/env bash

. ../helpers.sh

DOT_ZSH_DIR=$(pwd)

# .profile is source by .zshenv

if [ ! -L $HOME/.profile ]; then
	cd ../sh
	[ -f install.sh ] && ./install.sh
	cd -
fi

# Set up spaceship-prompt

ZSH_LIB_HOME=$HOME/.local/lib/zsh
ZSH_DATA_HOME=$HOME/.local/share/zsh
ZSH_CACHE_HOME=$HOME/.cache/zsh

if [ ! -d $ZSH_DATA_HOME/site-functions ]; then
	echo_info "Creating directory for zsh site-functions"
	mkdir -p $ZSH_DATA_HOME/site-functions
fi

if [ ! -d $ZSH_LIB_HOME/spaceship-prompt ]; then
	echo_info "Installing spaceship-prompt"
	mkdir -p $ZSH_LIB_HOME
	git clone https://github.com/denysdovhan/spaceship-prompt.git \
			  $ZSH_LIB_HOME/spaceship-prompt
	ln -sf $ZSH_LIB_HOME/spaceship-prompt/spaceship.zsh \
		   $ZSH_DATA_HOME/site-functions/prompt_spaceship_setup
fi

if ! [ -d "$ZSH_CACHE_HOME" ]; then
    echo_info "Creating cache directory for Zsh"
    mkdir -p "$ZSH_CACHE_HOME"
fi

# Symlink the zsh startup files

function symlink () {
    if ! [ -e "$HOME/.$1" ]; then
	    echo_info "Linking .$1"
	    ln -sf $DOT_ZSH_DIR/$1 $HOME/.$1
    fi
}

symlink "zshenv"
symlink "zprofile"
symlink "zshrc"

echo_done "Setup zsh"
