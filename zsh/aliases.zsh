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
alias ls='ls -G'           # always colours
alias ll='ls -l'
alias l.='ls -A'
alias ll.='ls -Al'
alias lsd='ls -ld *(-/DN)' # list only dirs

# misc
alias df='df -h'
alias du='du -h'
