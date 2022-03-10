# ~/.dofiles/zsh/aliases.zsh

# Load common Bourne shell aliases
if [ -e "$SH_CONFIG_HOME/aliases.sh" ]; then
    source "$SH_CONFIG_HOME/aliases.sh"
fi

alias h='history -fi'

# No spelling correction
alias mv='nocorrect mv -i'
alias cp='nocorrect cp -i'
alias rm='nocorrect rm -i'
alias mkdir='nocorrect mkdir'

# ls
alias lsd='ls -ld *(-/DN)' # list only dirs

if $(which colorls &> /dev/null); then
	alias lc="colorls --gs"
	alias l.c="lc -A"
	alias llc="lc -l"
	alias ll.c="lcl -A"
fi
