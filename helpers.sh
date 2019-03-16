#!/usr/bin/env bash

function echo_error() {
  printf '\033[31mERROR:\033[0m %s\n' "$1"
}

function echo_warning() {
  printf '\033[33mWARNING:\033[0m %s\n' "$1"
}

function echo_done() {
  printf '\033[32mDONE:\033[0m %s\n' "$1"
}

function echo_info() {
  printf '\033[36m%s\033[0m\n' "$1"
}

function rm_symlink () {
	link=${HOME}/.$1
	if [[ -L ${link} ]]; then
		echo_info "Removing .$1"
		rm -f ${link}
	else
		echo_warning ".${link} does not exist"
	fi
}

function is_mac () {
	[ "$(uname -s)" = "Darwin" ]
}

function is_linux () {
	[ "$(uname -s)" = "Linux" ]
}
