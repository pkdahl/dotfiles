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

# directories
alias prj='cd ${HOME}/projects'
alias 1060='cd ${HOME}/projects/uio-inf1060'
alias 1300='cd ${HOME}/projects/uio-inf1300'
alias 2220='cd ${HOME}/projects/uio-inf2220'
alias 3110='cd ${HOME}/projects/uio-inf3110'
alias 3331='cd ${HOME}/projects/uio-inf3331'
alias 4171='cd ${HOME}/projects/uio-inf4171'
