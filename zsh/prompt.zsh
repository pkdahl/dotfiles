# http://stevelosh.com/blog/2010/02/my-extravagant-zsh-prompt/
# https://github.com/robbyrussell/oh-my-zsh

autoload -U colors && colors

setopt promptsubst

function git_prompt_info() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo "$ZSH_THEME_GIT_PROMPT_PREFIX$(parse_git_dirty) ${ref#refs/heads/} $ZSH_THEME_GIT_PROMPT_SUFFIX"
}

parse_git_dirty() {
  local STATUS=''
  local FLAGS
  FLAGS=('--porcelain')
  STATUS=$(command git status ${FLAGS} 2> /dev/null | tail -n1)
  if [[ -n $STATUS ]]; then
    echo "$ZSH_THEME_GIT_PROMPT_DIRTY"
  else
    echo "$ZSH_THEME_GIT_PROMPT_CLEAN"
  fi
}

ZSH_THEME_GIT_PROMPT_PREFIX="on "
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[red]%}"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[green]%}"

# function prompt_char() {
#     git branch >/dev/null 2>/dev/null && echo '' && return
#     echo ''
# }

function in_nix_shell() {
    if [[ -n $IN_NIX_SHELL ]]; then
        echo ' ';
    else
        echo '';
    fi
}

PROMPT='
%{$fg[cyan]%}%n%{$reset_color%} at %{$fg[magenta]%}%m%{$reset_color%} in %{$fg[blue]%}%~%{$reset_color%} $(git_prompt_info)
$(in_nix_shell) '
