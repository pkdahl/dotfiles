#!/usr/bin/env bash

. ../helpers.sh

DOT_BASH_DIR=$(pwd)


# Set up bash-git-prompt

BASH_LIB_HOME=$HOME/.local/lib/bash

if [ ! -d $BASH_LIB_HOME/bash-git-prompt ]; then
	echo_info "Installing bash-git-prompt"
	mkdir -p $BASH_LIB_HOME
	git clone https://github.com/magicmonty/bash-git-prompt.git $BASH_LIB_HOME/bash-git-prompt --depth=1
fi


# .profile is sourced by .bash_profile

if [ ! -L $HOME/.profile ]; then
	cd ../sh
	[ -f install.sh ] && ./install.sh
	cd -
fi

# Symlink the bash startup files

function symlink () {
	echo_info "Linking .$1"
	ln -sf $DOT_BASH_DIR/$1 $HOME/.$1
}

symlink "bash_profile"
symlink "bashrc"
symlink "bash_login"
symlink "bash_logout"


echo_done "Setup bash"
