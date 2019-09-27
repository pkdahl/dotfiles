# ~/.dotfiles/sh/aliases.sh

# Directories

function alias_dir () {
	if [ -d $2 ]; then
		alias $1="cd $2"
	fi
}

alias_dir dotfiles "$DOTFILES"
alias_dir docs "$XDG_DOCUMENTS_DIR"
alias_dir notes "$XDG_DOCUMENTS_DIR/notes"
alias_dir houston "$XDG_DOCUMENTS_DIR/houston"
alias_dir prj "$HOME/Projects"
alias_dir dl "$HOME/Downloads"

# Ask to confirm
alias rm="rm -i"
alias mv="mv -i"

# ls

if [ "$(uname -s)" = "Darwin" ]; then
	alias ls="ls -G"
else
	alias ls="ls --color"
fi

alias ll="ls -l"
alias ll.="ls -lA"
alias l.="ls -A"
alias l="ls -CF"
alias lt="ls -lrt"

# pushd / popd / dirs

alias d='dirs -v'
alias o='popd'
alias p='pushd'

# df / dh use human-readable outptu

alias df="df -h"
alias du="du -h"

# SSH

if [ -L "$HOME/.ssh/config" ]; then
	alias ssh="ssh -F $HOME/.ssh/config"
	export GIT_SSH_COMMAND="ssh -F $HOME/.ssh/config"
fi

# vim

alias vi="vim"

# USIT things

if [ -f /uio/kant/usit-houston-felles/pybofh/bin/houston-person-info.py ]; then
    alias hbofh="/uio/kant/usit-houston-felles/pybofh/bin/houston-person-info.py"
fi
