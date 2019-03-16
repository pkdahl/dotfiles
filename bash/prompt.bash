# ~/.dotfiles/bash/prompt.bash

BASH_LIB_DIR=$HOME/.local/lib/bash

if [ -d $BASH_LIB_DIR/bash-git-prompt ]; then
	GIT_PROMPT_ONLY_IN_REPO=1
	. $BASH_LIB_DIR/bash-git-prompt/gitprompt.sh
fi
