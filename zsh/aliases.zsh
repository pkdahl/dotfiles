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

alias_cd_when_dir () {
	if [[ -d "$2" ]]; then
		alias $1="cd $2"
	fi
}

alias_cd_when_dir prj $HOME/Projects
alias_cd_when_dir 2100 $HOME/Projects/in2100
alias_cd_when_dir 4060 $HOME/Projects/in4060
alias_cd_when_dir docs $HOME/Documents

# This files's absolute path's with trailing component removed twice,
# I.e. zsh/aliases.sh is removed from absolute path.
alias dotfiles="cd ${0:A:h:h}"
