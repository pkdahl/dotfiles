# ~/.config/bash/aliases.bash

alias rm="rm -i"
alias mv="mv -i"

# ls
if [[ "$OSTYPE" == "linux-gnu" ]]; then
	alias ls="ls --color"
else
	alias ls="ls -G"
fi
alias ll="ls -l"
alias ll.="ls -lA"
alias l.="ls -A"
alias l="ls -CF"
alias lt="ls -lrt"

alias pu="pushd"
alias po="pop"
alias d="dirs -v"

alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."

alias vi="vim"
