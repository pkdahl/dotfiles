#!/usr/bin/env bash

. ../helpers.sh

if [[ -L $HOME/.vim ]]; then
   echo_info "Removing .vim"
   rm -f $HOME/.vim
fi
