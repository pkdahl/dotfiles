#!/usr/bin/env bash

. ../helpers.sh

if [[ -L $HOME/.emacs.d ]]; then
	echo_info "Removing .emacs.d"
	rm -f $HOME/.emacs.d
fi
