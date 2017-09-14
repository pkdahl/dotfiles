DOTFILES=`pwd`

.PHONY: bash_clean git_clean

bash_install: 
	ln -sf $(DOTFILES)/bash/bash_profile $(HOME)/.bash_profile
	ln -sf $(DOTFILES)/bash/bashrc $(HOME)/.bashrc

bash_clean:
	rm -f $(HOME)/.bash_profile
	rm -f $(HOME)/.bashrc

git_install:
	mkdir -p $(DOTFILES)/.config/git
	ln -sf $(DOTFILES)/git/config $(HOME)/.config/git/config
	ln -sf $(DOTFILES)/git/config $(HOME)/.config/git/ignore

git_clean:
	rm -rf $(DOTFILES)/.config/git

