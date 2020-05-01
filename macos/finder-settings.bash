# Run this with bash, i.e. `bash install.sh`

function is_mac () {
	[ "$(uname -s)" = "Darwin" ]
}

# Abort if we're not on a mac
! is_mac && exit

# Show full path in window title
defaults write com.apple.finder _FXShowPosixPathInTitle -bool true

# Restart Dock
killall Finder
