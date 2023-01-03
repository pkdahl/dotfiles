setopt APPEND_HISTORY         # Allows appending of new history to HISTFILE instead of overwrite
setopt INC_APPEND_HISTORY     # Commands are added to history as they are executed
setopt SHARE_HISTORY
setopt HIST_VERIFY            # Allow editing of command instead of running it immediately
setopt HIST_FIND_NO_DUPS      # Backward search with editor do not show commands more than once
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE      # Commands starting with space are not added to history
setopt HIST_EXPIRE_DUPS_FIRST
export HISTFILE=$ZSH_CACHE_HOME/zhistory
export HISTSIZE=10240
export SAVEHIST=$((HISTSIZE - 240))
