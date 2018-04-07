DOTFILES=`pwd`
CACHE_HOME=$(HOME)/.cache
CONFIG_HOME=$(HOME)/.config
DATA_HOME=$(HOME)/.local/share
DOT_BASH=$(DOTFILES)/bash
BASH_CONFIG_HOME=$(CONFIG_HOME)/bash
DOT_ZSH=$(DOTFILES)/zsh
ZSH_CACHE_HOME=$(CACHE_HOME)/zsh
ZSH_CONFIG_HOME=$(CONFIG_HOME)/zsh


.PHONY: bash_clean git_clean irssi_clean

bash_install:
	mkdir -p $(BASH_CONFIG_HOME)
	ln -sf $(DOTFILES)/sh/profile        $(HOME)/.profile
	ln -sf $(DOTFILES)/bash/bash_profile $(HOME)/.bash_profile
	ln -sf $(DOTFILES)/bash/bashrc       $(HOME)/.bashrc
	ln -sf $(DOTFILES)/bash/bash_login   $(HOME)/.bash_login
	ln -sf $(DOTFILES)/bash/bash_logout  $(HOME)/.bash_logout
	ln -sf $(DOTFILES)/bash/aliases.bash $(BASH_CONFIG_HOME)/aliases.bash

bash_clean:
	rm -f $(HOME)/.profile
	rm -f $(HOME)/.bash_profile
	rm -f $(HOME)/.bashrc
	rm -f $(HOME)/.bash_login
	rm -f $(HOME)/.bash_logout
	rm -rf $(BASH_CONFIG_HOME)

emacs_install:
	ln -sf $(DOTFILES)/emacs $(HOME)/.emacs.d

emacs_clean:
	rm -f $(HOME)/.emacs.d

git_install:
	mkdir -p $(CONFIG_HOME)/git
	ln -sf $(DOTFILES)/git/config          $(CONFIG_HOME)/git/config
	ln -sf $(DOTFILES)/git/ignore          $(CONFIG_HOME)/git/ignore
	ln -sf $(DOTFILES)/private/git/private $(CONFIG_HOME)/git/private

git_clean:
	rm -rf $(CONFIG_HOME)/git

hammerspoon_install:
	ln -sf $(DOTFILES)/hammerspoon $(HOME)/.hammerspoon

hammerspoon_clean:
	rm -f $(HOME)/.hammerspoon

irssi_install:
	mkdir -p $(HOME)/.irssi
	ln -sf $(DOTFILES)/private/irssi/config $(HOME)/.irssi/config
	ln -sf $(DOTFILES)/irssi/startup        $(HOME)/.irssi/startup

irssi_clean:
	rm -rf $(HOME)/.irssi

liquidprompt_install:
	mkdir -p $(HOME)/.local/share
	ln -sf $(DOTFILES)/lib/liquidprompt/liquidprompt $(DATA_HOME)/liquidprompt
	ln -sf $(DOTFILES)/liquidprompt/liquidpromptrc   $(CONFIG_HOME)/liquidpromptrc

mail_install:
	mkdir -p $(HOME)/.local/bin
	ln -sf $(DOTFILES)/mail/mailbkup $(HOME)/.local/bin/mailbkup
	ln -sf $(DOTFILES)/mail/mbsynrc  $(HOME)/.mbsyncrc

mail_clean:
	rm -f $(HOME)/.local./bin/mailbkup
	rmdir -p $(HOME)/.local/bin
	rm -f $(HOME)/.mbsyncrc

vim_install:
	mkdir -p $(CACHE_HOME)/vim
	ln -sf $(DOTFILES)/vim/vimrc $(HOME)/.vimrc

vim_clean:
	rm -f $(HOME)/.vimrc
	rm -rf $(CACHE_HOME)/vim

zsh_install:
	mkdir -p $(ZSH_CACHE_HOME)
	mkdir -p $(ZSH_CONFIG_HOME)
	ln -sf $(DOT_ZSH)/zshenv         $(HOME)/.zshenv
	ln -sf $(DOT_ZSH)/zprofile       $(HOME)/.zprofile
	ln -sf $(DOT_ZSH)/zshrc          $(HOME)/.zshrc
	ln -sf $(DOT_ZSH)/aliases.zsh    $(ZSH_CONFIG_HOME)/aliases.zsh
	ln -sf $(DOT_ZSH)/completion.zsh $(ZSH_CONFIG_HOME)/completion.zsh
	ln -sf $(DOT_ZSH)/history.zsh    $(ZSH_CONFIG_HOME)/history.zsh
	ln -sf $(DOT_ZSH)/prompt.zsh     $(ZSH_CONFIG_HOME)/prompt.zsh

zsh_clean:
	rm -rf $(ZSH_CACHE_HOME)
	rm -rf $(ZSH_CONFIG_HOME)
	rm -f $(HOME)/.zshenv
	rm -f $(HOME)/.zprofile
	rm -f $(HOME)/.zshrc
