# Run this with bash, i.e. `bash install.sh`

function is_mac () {
	[ "$(uname -s)" = "Darwin" ]
}

# Abort if we're not on a mac
! is_mac && exit

# Put Dock on the left
defaults write com.apple.dock orientation -string 'left'

# Autohide Dock
defaults write com.apple.dock autohide -bool true

# Restart Dock
killall Dock
