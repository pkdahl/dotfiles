#!/usr/bin/env bash

. ../helpers.sh

GIT_CONFIG_HOME=${HOME}/.config/git

function rm_symlink () {
	if [[ -L ${GIT_CONFIG_HOME}/$1 ]]; then
		echo_info "Removing $1"
		rm -f ${GIT_CONFIG_HOME}/$1
	fi
}

rm_symlink "config"
rm_symlink "ignore"
rm_symlink "private"

if [[ -d $GIT_CONFIG_HOME ]]; then
	echo_info "Removing $GIT_CONFIG_HOME"
	rmdir $GIT_CONFIG_HOME
fi

echo_done "Uninstalled bash"
