# ~/.config/bash/prompt.bash

set_status() {
    local happy='\[\e[0;32m\]^_^\[\e[0m\]'
    local surprised='\[\e[0;31m\]O_O\[\e[0m\]'
    local host='\[\e[0;33m\]\h\[\e[0m\]'
    local path='\[\e[0;34m\]\w\[\e[0m\]'
    local git_ref=$(git symbolic-ref HEAD 2> /dev/null) || return

    PS1="\n"

    if [[  $UID == 0 ]] ; then
        local user='\[\e[0;31m\]\u\[\e[0m\]'
    else
        local user='\[\e[0;32m\]\u\[\e[0m\]'
    fi

    PS1+="$user at $host in $path"

    if [[ -n $git_ref ]] ; then
        local git_dirty=$(git status --porcelain 2> /dev/null | tail -n1)
        if [[ -n $git_dirty ]] ; then
            PS1+=" on \[\e[0;31m\]${git_ref#refs/heads/}\[\e[0m\]"
        else
            PS1+=" on \[\e[0;32m\]${git_ref#refs/heads/}\[\e[0m\]"
        fi
    fi

    PS1+="\n"

    if [[ $? == 0 ]] ; then
        PS1+="$happy"
    else
        PS1+="$surprised"
    fi

    PS1+=" \$ "


}


PROMPT_COMMAND="set_status"
