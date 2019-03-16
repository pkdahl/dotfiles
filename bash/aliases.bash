# ~/.dotfiles/bash/aliases.bash

alias rm="rm -i"
alias mv="mv -i"

# ls
if [[ "$OSTYPE" == darwin* ]]; then
	alias ls="ls -G"
else
	alias ls="ls --color"
fi

alias ll="ls -l"
alias ll.="ls -lA"
alias l.="ls -A"
alias l="ls -CF"
alias lt="ls -lrt"

alias pu="pushd"
alias po="pop"
alias d="dirs -v"

alias vi="vim"
