# ~/.dotfiles/sh/aliases.sh

# Directories

function alias_dir () {
	if [ -d $2 ]; then
		alias $1="cd $2"
	fi
}

alias_dir dotfiles $DOTFILES
alias_dir docs $HOME/Documents
alias_dir prj $HOME/Projects
alias_dir 2100 $HOME/Projects/in2100
alias_dir 4060 $HOME/Projects/in4060

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

# vim

alias vi="vim"
