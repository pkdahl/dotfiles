#!/usr/bin/env bash

. helpers.sh

function _install_dir () {
	dir=$1
	cd $dir || exit
	[ -f install.sh ] && ./install.sh
	cd ..
}

function _install_all () {
	dirs=$(find . -maxdepth 1 -mindepth 1 -type d \
		   \( -not -name '.git' -o -not -name 'private' \) \
		   -print)

	for dir in $dirs; do
		_install_dir $dir
	done
}
