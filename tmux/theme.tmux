# This tmux statusbar config was created by tmuxline.vim
# on Fri, 03 May 2019

set -g status-justify "left"
set -g status "on"
set -g status-attr "none"
set -g message-command-bg "colour240"
set -g status-left-length "100"
set -g pane-active-border-fg "colour208"
set -g status-bg "colour236"
set -g message-command-fg "colour231"
set -g pane-border-fg "colour240"
set -g message-bg "colour240"
set -g status-left-attr "none"
set -g status-right-attr "none"
set -g status-right-length "100"
set -g message-fg "colour231"
setw -g window-status-fg "colour245"
setw -g window-status-attr "none"
setw -g window-status-activity-bg "colour236"
setw -g window-status-activity-attr "none"
setw -g window-status-activity-fg "colour208"
setw -g window-status-separator ""
setw -g window-status-bg "colour236"
set -g status-left "#[fg=colour88,bg=colour208,bold] #S #[fg=colour208,bg=colour236,nobold,nounderscore,noitalics]"
set -g status-right "#[fg=colour240,bg=colour236,nobold,nounderscore,noitalics]#[fg=colour250,bg=colour240] %Y-%m-%d | %H:%M #[fg=colour252,bg=colour240,nobold,nounderscore,noitalics]#[fg=colour241,bg=colour252] #h "
setw -g window-status-format "#[fg=colour245,bg=colour236] #I |#[fg=colour245,bg=colour236] #W "
setw -g window-status-current-format "#[fg=colour236,bg=colour240,nobold,nounderscore,noitalics]#[fg=colour231,bg=colour240] #I |#[fg=colour231,bg=colour240] #W #[fg=colour240,bg=colour236,nobold,nounderscore,noitalics]"