#!/usr/bin/env bash

. ../helpers.sh

# Remove spaceship-prompt

ZSH_LIB_HOME=$HOME/.local/lib/zsh
ZSH_DATA_HOME=$HOME/.local/share/zsh

if [ -L $ZSH_DATA_HOME/site-functions/prompt_spaceship_setup ]; then
	echo_info "Removing spaceship-prompt site-function"
	rm -f $ZSH_DATA_HOME/site-functions/prompt_spaceship_setup
fi

if [ -d $ZSH_LIB_HOME/spaceship-prompt ]; then
	echo_info "Removing spaceship-prompt library"
	rm -rf $ZSH_LIB_HOME/spaceship-prompt
fi

if [ -d "$ZSH_LIB_HOME" ]; then
	if [ -z $(ls -A "$ZSH_LIB_HOME") ]; then
		echo_info "Removing $ZSH_LIB_HOME"
		rmdir "$ZSH_LIB_HOME"
	fi
fi

# Remove zsh startup files

function rmlink () {
	if [ -L $HOME/.$1 ]; then
		echo_info "Removing .$1"
		rm -f $HOME/.$1
	fi	
}

rmlink "zshenv"
rmlink "zprofile"
rmlink "zshrc"

echo_done "Removed zsh setup"
