DOTFILES=`pwd`
DOT_BASH=$(DOTFILES)/bash
BASH_CONFIG_HOME=$(HOME)/.config/bash

.PHONY: bash_clean git_clean

bash_install:
	mkdir -p $(BASH_CONFIG_HOME)
	ln -sf $(DOTFILES)/bash/bash_profile $(HOME)/.bash_profile
	ln -sf $(DOTFILES)/bash/bashrc       $(HOME)/.bashrc
	ln -sf $(DOTFILES)/bash/bash_login   $(HOME)/.bash_login
	ln -sf $(DOTFILES)/bash/bash_logout  $(HOME)/.bash_logout
	ln -sf $(DOTFILES)/bash/env.bash     $(BASH_CONFIG_HOME)/env.bash
	ln -sf $(DOTFILES)/bash/prompt.bash  $(BASH_CONFIG_HOME)/prompt.bash

bash_clean:
	rm -f $(HOME)/.bash_profile
	rm -f $(HOME)/.bashrc
	rm -f $(HOME)/.bash_login
	rm -f $(HOME)/.bash_logout
	rm -rf $(BASH_CONFIG_HOME)

git_install:
	mkdir -p $(HOME)/.config/git
	ln -sf $(DOTFILES)/git/config          $(HOME)/.config/git/config
	ln -sf $(DOTFILES)/git/ignore          $(HOME)/.config/git/ignore
	ln -sf $(DOTFILES)/private/git/private $(HOME)/.config/git/private

git_clean:
	rm -rf $(DOTFILES)/.config/git
