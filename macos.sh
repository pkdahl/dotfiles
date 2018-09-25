#!/usr/bin/env bash

# Put Dock on the left
defaults write com.apple.dock orientation -string 'left'
# Autohide Dock
defaults write com.apple.dock autohide -bool true
# Restart Dock
killall Dock
