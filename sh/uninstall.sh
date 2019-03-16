#!/usr/bin/env bash

echo_info "Removing .profile"
[ -L $HOME/.profile ] && rm -f $HOME/.profile

echo_done "Removed sh setup"
