#!/usr/bin/env bash

. ../helpers.sh

CURRENT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DOT_TASK="$CURRENT_DIR"
# Change this to use XGD_DATA_HOME
# Need to ensure XGD_DATA_HOME is set
TASK_DATA_HOME="$HOME/.local/share/task"

echo_info "Setting up Taskwarrior"

# Link settings files
if [ ! -L "$HOME/.taskrc" ]; then
    echo_info "Linking .taskrc"
    ln -sf "$DOT_TASK/taskrc" "$HOME/.taskrc"
fi

# Setup data, i.e. clone git repository with tasks

HOME_RE=^/uio/kant/.*/pkdahl$
GIT_REPO="~/git/task.git"

if [[ ! "$HOME" =~ $HOME_RE ]]; then
    GIT_REPO="ssh://pkdahl@login.uio.no/${GIT_REPO}"
fi

if [ ! -d "$TASK_DATA_HOME" ]; then
    echo info "Cloning Taskwarrior data from $GIT_REPO"
    git clone "$GIT_REPO" "$TASK_DATA_HOME"
fi

unset HOME_RE
unset GIT_REPO
