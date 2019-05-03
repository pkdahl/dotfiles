# This tmux statusbar config was created by tmuxline.vim
# on Fri, 03 May 2019
# [2019-05-03 Fri] Updated to conform with tmux 2.9 *-style options
set -g status "on"
set -g status-justify "left"
set -g status-style bg="colour236","none"
set -g status-left-length "100"
set -g status-left-style "none"
set -g status-right-length "100"
set -g status-right-style "none"
set -g message-style fg="colour231",bg="colour240"
set -g message-command-style fg="colour231",bg="colour240"
set -g pane-border-style fg="colour240"
set -g pane-active-border-style fg="colour208"
setw -g window-status-separator ""
setw -g window-status-style fg="colour245",bg="colour236","none"
setw -g window-status-activity-style fg="colour208",bg="colour236","none"

set -g status-left "#[fg=colour88,bg=colour208,bold] #S #[fg=colour208,bg=colour236,nobold,nounderscore,noitalics]"
set -g status-right "#[fg=colour240,bg=colour236,nobold,nounderscore,noitalics]#[fg=colour250,bg=colour240] %Y-%m-%d | %H:%M #[fg=colour252,bg=colour240,nobold,nounderscore,noitalics]#[fg=colour241,bg=colour252] #h "
setw -g window-status-format "#[fg=colour245,bg=colour236] #I |#[fg=colour245,bg=colour236] #W "
setw -g window-status-current-format "#[fg=colour236,bg=colour240,nobold,nounderscore,noitalics]#[fg=colour231,bg=colour240] #I |#[fg=colour231,bg=colour240] #W #[fg=colour240,bg=colour236,nobold,nounderscore,noitalics]"
