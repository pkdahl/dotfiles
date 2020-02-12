SHELL := bash

CACHE_HOME  = $(HOME)/.cache
CONFIG_HOME = $(HOME)/.config
DATA_HOME   = $(HOME)/.local/share
LIB_HOME    = $(HOME)/.local/lib

.DEFAULT_TARGET: help

.PHONY: help
help: 
	@echo "Usage: make {zsh | homebrew | git | ssh | neovim}"

#{{{ Common Bourne shell settings

SH_CONFIG_HOME = $(CONFIG_HOME)/sh

$(HOME)/.profile:
	ln -sf $(PWD)/sh/profile $@

$(HOME)/.envir:
	ln -sf $(PWD)/sh/envir $@

$(SH_CONFIG_HOME)/aliases.sh:
	mkdir -p $(@D)
	ln -sf $(PWD)/sh/aliases.sh $@

.PHONY: sh
sh: $(HOME)/.profile $(HOME)/.envir $(SH_CONFIG_HOME)/aliases.sh
#}}}
#{{{ Z shell

ZSH_CACHE_HOME  = $(CACHE_HOME)/zsh
ZSH_CONFIG_HOME = $(CONFIG_HOME)/zsh
ZSH_DATA_HOME   = $(DATA_HOME)/zsh
ZSH_LIB_HOME    = $(LIB_HOME)/zsh

$(ZSH_CACHE_HOME):
	mkdir -p $@
	touch $@

$(ZSH_CONFIG_HOME):
	mkdir -p $@
	touch $@

$(ZSH_DATA_HOME)/site-functions:
	mkdir -p $@
	touch $@ 

$(ZSH_LIB_HOME)/spaceship-prompt: $(ZSH_DATA_HOME)/site-functions
	mkdir -p $(@D)
	git clone https://github.com/denysdovhan/spaceship-prompt.git \
		$(ZSH_LIB_HOME)/spaceship-prompt
	ln -sf $(ZSH_LIB_HOME)/spaceship-prompt/spaceship.zsh \
		$(ZSH_DATA_HOME)/site-functions/prompt_spaceship_setup
	touch $@

$(HOME)/.zshenv:
	ln -sf $(PWD)/zsh/zshenv $@

$(HOME)/.zprofile:
	ln -sf $(PWD)/zsh/zprofile $@

$(HOME)/.zshrc:
	ln -sf $(PWD)/zsh/zshrc $@

$(ZSH_CONFIG_HOME)/aliases.zsh:
	mkdir -p $(@D)
	ln -sf $(PWD)/zsh/aliases.zsh $@

$(ZSH_CONFIG_HOME)/completion.zsh:
	mkdir $(@D)
	ln -sf $(PWD)/zsh/completion.zsh $@

$(ZSH_CONFIG_HOME)/history.zsh:
	mkdir $(@D)
	ln -sf $(PWD)/zsh/history.zsh $@

ZSH_DEPS := $(ZSH_CACHE_HOME) $(ZSH_CONFIG_HOME)
ZSH_DEPS += $(ZSH_DATA_HOME)/site-functions
ZSH_DEPS += $(ZSH_LIB_HOME)/spaceship-prompt
ZSH_DEPS += $(HOME)/.zshenv $(HOME)/.zprofile $(HOME)/.zshrc
ZSH_DEPS += $(ZSH_CONFIG_HOME)/aliases.zsh $(ZSH_CONFIG_HOME)/completion.zsh
ZSH_DEPS += $(ZSH_CONFIG_HOME)/history.zsh
ZSH_DEPS += sh

.PHONY: zsh
zsh: $(ZSH_DEPS)
#}}}
#{{{ Homebrew

BREW_EXE := /usr/local/bin/brew

$(BREW_EXE):
	/usr/bin/ruby -e "$$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

homebrew: $(BREW_EXE)

.PHONY = homebrew
#}}}
#{{{ Git

GIT_CONFIG_HOME = $(CONFIG_HOME)/git

$(GIT_CONFIG_HOME)/attributes:
	mkdir -p $(@D)
	ln -sf $(PWD)/git/attributes $@

$(GIT_CONFIG_HOME)/config:
	mkdir -p $(@D)
	ln -sf $(PWD)/git/config $@

$(GIT_CONFIG_HOME)/ignore:
	mkdir -p $(@D)
	ln -sf $(PWD)/git/ignore $@

GIT_DEPS := $(GIT_CONFIG_HOME)/attributes $(GIT_CONFIG_HOME)/config $(GIT_CONFIG_HOME)/ignore

git: $(GIT_DEPS)

.PHONY = git
#}}}
#{{{ SSH (Secure shell)

SSH_CONFIG_HOME = $(HOME)/.ssh

$(SSH_CONFIG_HOME)/config:
	mkdir -p $(@D)
	ln -sf $(PWD)/ssh/config $@

ssh: $(SSH_CONFIG_HOME)/config

.PHONY = ssh
#}}}
#{{{ Neovim
# Depends on some Python stuff and node stuff for LSP

DOT_NVIM = $(PWD)/nvim
NVIM_CONFIG_HOME = $(CONFIG_HOME)/nvim
NVIM_DATA_HOME = $(DATA_HOME)/nvim
NVIM_EXE = /usr/local/bin/nvim

# Installing Neovim with Homebrew
$(NVIM_EXE): | $(BREW_EXE)
	brew install neovim

$(NVIM_CONFIG_HOME)/init.vim: | $(NVIM_EXE)
	mkdir -p $(@D)
	ln -sf $(DOT_NVIM)/init.vim $@

$(NVIM_DATA_HOME)/site/autoload/plug.vim: | $(NVIM_EXE) $(NVIM_CONFIG_HOME)/init.vim
	mkdir -p $(@D) 
	curl -fLo $@ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	nvim --headless +'PlugInstall --sync' +qa

# install fzf
# missing coc. dependens on nodejs
# neovim python module

NVIM_DEPS := $(NVIM_EXE)
NVIM_DEPS += $(NVIM_CONFIG_HOME)/init.vim
NVIM_DEPS += $(NVIM_DATA_HOME)/site/autoload/plug.vim

.PHONY: neovim
neovim: $(NVIM_DEPS)
#}}}
#{{{ Pass

PASS_EXE := /usr/local/bin/pass

$(PASS_EXE): | $(BREW_EXE)
	brew install pass

$(HOME)/.password-store/.git: | $(PASS_EXE)
	git clone ssh://pkdahl@login.uio.no/~/git/password-store.git $(@D)

.PHONY: pass
pass: | $(PASS_EXE) $(HOME)/.password-store/.git
	@echo "Remember to setup GPG keys"
#}}}
#{{{ macOS settings

$(DATA_HOME)/dotfiles/macos-sentinel:
	bash $(PWD)/macos/dock-settings.bash
	mkdir -p $(@D)
	touch $@

.PHONY: macos
macos: $(DATA_HOME)/dotfiles/macos-sentinel
#}}}
#{{{ macOS applications

# iTerm

/Applications/iTerm.app: | $(BREW_EXE)
	$(BREW_EXE) cask install iterm2

$(HOME)/Downloads/Nord.itermcolors: | /Applications/iTerm.app
	curl -fLo $@ https://raw.githubusercontent.com/arcticicestudio/nord-iterm2/develop/src/xml/Nord.itermcolors
	@echo "Downloaded Nord color theme for iTerm2 to ~/Downloads"
	@echo "To install it:"
	@echo "1. Start iTerm2 and open the Preferences"
	@echo "2. Switch to the Profiles tab located in the topbar"
	@echo "3. Select the Colors tabs on the right side"
	@echo "4. Click on the Load Presets drop-down menu and select the Import entry"
	@echo "5. Import the downloaded Nord.itermcolors"

.PHONY: iterm
iterm: /Applications/iTerm.app $(HOME)/Downloads/Nord.itermcolors

# Firefox

/Applications/Firefox.app: | $(BREW_EXE)
	$(BREW_EXE) cask install firefox

.PHONY: firefox
firefox: /Applications/Firefox.app

#}}}
