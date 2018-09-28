alias h='history -fi'
alias d='dirs -v'
alias o='popd'
alias p='pushd'

# no spelling correction
alias mv='nocorrect mv -i'
alias cp='nocorrect cp -i'
alias rm='nocorrect rm -i'
alias mkdir='nocorrect mkdir'

# ls
if [[ "$OSTYPE" == darwin* ]]; then
	alias ls="ls -G"
else
	alias ls="ls --color"
fi

alias ll='ls -l'
alias l.='ls -A'
alias ll.='ls -Al'
alias lsd='ls -ld *(-/DN)' # list only dirs

if $(which colorls &> /dev/null); then
	alias lc="colorls --gs"
	alias l.c="lc -A"
	alias llc="lc -l"
	alias ll.c="lcl -A"
fi

# misc
alias df='df -h'
alias du='du -h'

# directories
alias prj="cd $HOME/Projects"
alias 4490="cd $HOME/Project/inf4490"
alias 4171="cd $HOME/Projects/inf4171"
