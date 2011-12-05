#--
# .zshrc is sourced in interactive shells.
# It should contain commands to set up aliases,
# functions, options, key bindings, etc.
#++

#
# Modules {{{
#
autoload -U compinit
compinit

#autolad -U colors
#colors

# }}}
# {{{ Environment variables


DIRSTACKSIZE=100
# }}}
# {{{ Options

# Any directory changing command is treated as pushd
setopt AUTO_PUSHD
# allow tab completion in the middle of a word
setopt COMPLETE_IN_WORD

## keep background processes at full speed
#setopt NOBGNICE
## restart running processes on exit
#setopt HUP

#--
# History options
#++
# Allows appending of new history to HISTFILE instead of overwrite
#setopt APPEND_HISTORY
# Commands are added to history as they are executed
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY
#setopt HIST_IGNORE_ALL_DUPS
# Do not store history or fc commands
setopt HIST_NO_STORE
# Backward search with editor do not show commands more than once
setopt HIST_FIND_NO_DUPS
# Allow editing of command instead of running it immediately
setopt HIST_VERIFY

#--
# History variables
#++
HISTFILE=$ZDOTDIR/.zhistory
HISTSIZE=10240
SAVEHIST=10240

# Never beep
setopt NO_BEEP

# automatically decide when to page a list of completions
#LISTMAX=0

# Disable mail checking
#MAILCHECK=0

#--
# Aliases
#++
alias h='history -fi'
alias ss='svn status'
alias httpdstr='su -c "/etc/init.d/httpd start"'
alias men='encfs $HOME/Documents/encrypted $HOME/Desktop/mnt'
alias umen='fusermount -u $HOME/Desktop/mnt'

# no spelling correction
alias mv='nocorrect mv -i'
alias cp='nocorrect cp -i'
alias rm='nocorrect rm -i'
alias mkdir='nocorrect mkdir'

# ls
# Always colours
alias ls='ls -G'
alias ll='ls -l'
alias l.='ls -A'
alias ll.='ls -Al'
alias lsd='ls -ld *(-/DN)'	# list only dirs

# misc
alias df='df -h'
alias du='du -h'
# }}}
# {{{ Prompt

PROMPT='[%n@%m:%~]%# '
RPROMPT='[%!]'

# }}}
# {{{ Misc

# Emacs keybindings
bindkey -e

# }}}

#function title {
#	if [[ $TERM == "screen" ]]
#	then
#		print -nR $' 33k'$1$' 33'\
#		print -nR $' 33]0; '$2$''
#	elif [[ $TERM == "xterm" || $TERM == "rxvt" ]]
#	then
#		print -nR $' 33]0;'$*$''
#	fi
#}
#function precmd { title zsh "$PWD" }
#function preexec {
#	emulate -L zsh
#	local -a cmd; cmd=(${(z)1})
#	title $cmd[1]:t "$cmd[2,-1]"
#}

# __END__
