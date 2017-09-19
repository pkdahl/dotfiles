set_prompt () {
Last_Command=$? # Must come first!
    Blue='\[\e[01;34m\]'
    White='\[\e[01;37m\]'
    Red='\[\e[01;31m\]'
    Green='\[\e[01;32m\]'
    Reset='\[\e[00m\]'
    FancyX='\342\234\227'
    Checkmark='\342\234\223'

    PS1=""

    # If it was successful, print a green check mark.
    # Otherwise, print a red X.
    if [[ $Last_Command == 0 ]]; then
        PS1+="$Green$Checkmark "
    else
        PS1+="$Red$FancyX "
    fi
    # If root, just print the host in red.
    # Otherwise, print the current user and host in green.
    if [[ $EUID == 0 ]]; then
        PS1+="$Red\\h "
    else
        PS1+="$Green\\u@\\h "
    fi
    # Print the working directory and prompt marker in blue,
    # and reset the text color to the default.
    PS1+="$Blue\\w \\\$$Reset "
}

set_status() {
    local happy='\[\e[0;32m\]^_^\[\e[0m\]'
    local surprised='\[\e[0;31m\]O_O\[\e[0m\]'
    local host='\[\e[0;33m\]\h\[\e[0m\]'
    local path='\[\e[0;34m\]\w\[\e[0m\]'
    local git_ref=$(git symbolic-ref HEAD 2> /dev/null) || return

    if [[  $UID == 0 ]] ; then
        local user='\[\e[0;31m\]\u\[\e[0m\]'
    else
        local user='\[\e[0;32m\]\u\[\e[0m\]'
    fi

    PS1="$user at $host in $path"

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
