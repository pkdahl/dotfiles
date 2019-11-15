#!/usr/bin/env bash

. ../helpers.sh

DOT_GIT_DIR=$(pwd)
GIT_CONFIG_HOME=${HOME}/.config/git

if [[ ! -d ${GIT_CONFIG_HOME} ]]; then
	echo_info "Creating ${GIT_CONFIG_HOME}"
	mkdir -p ${GIT_CONFIG_HOME}
fi

function symlink () {
	echo_info "Symlinking $1"
	ln -sf $DOT_GIT_DIR/$1 $GIT_CONFIG_HOME/$1
}

symlink "attributes"
symlink "config"
symlink "ignore"
symlink "private"
