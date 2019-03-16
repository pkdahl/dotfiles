#!/usr/bin/env bash

. ../helpers.sh

DOT_EMACS_DIR=$(pwd)

if [[ ! -L $HOME/.emacs.d ]]; then
	echo "Symlinking .emacs.d"
	ln -sf $DOT_EMACS_DIR $HOME/.emacs.d
fi
