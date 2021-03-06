SHELL := bash

CACHE_HOME  = $(HOME)/.cache
CONFIG_HOME = $(HOME)/.config
DATA_HOME   = $(HOME)/.local/share
LIB_HOME    = $(HOME)/.local/lib

USERNAME       := $(shell whoami)
# LOCAL_HOSTNAME := $(shell scutil --get LocalHostName)
DATE           := $(shell date "+%d %b %Y")
DATE_ISO       := $(shell date "+%F")

GIT_REPO_BASE_PATH := ~/git
# If we are not on a UiO machine
ifeq (, $(findstring kant, $(HOME)))
GIT_REPO_BASE_PATH := ssh://pkdahl@login.uio.no/$(GIT_REPO_BASE_PATH)
endif

BREW_BIN := $(brew --prefix)/bin

.DEFAULT_TARGET: help

.PHONY: help
help:
	@echo "Usage: make [RULE]"
	@echo "Available rules:"
	@echo "  - zsh"
	@echo "  - ssh"
	@echo "  - git"
	@echo "  - homebrew"
	@echo "  - neovim"
	@echo "  - pass"
	@echo "  - cheat"
	@echo "  - macos"
	@echo "  - iterm"
	@echo "  - firefox"
	@echo "  - fonts"

.PHONY: clean
clean:
	rm -rf $(CACHE_HOME)/dotfiles

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

$(ZSH_LIB_HOME)/spaceship-prompt: | $(ZSH_DATA_HOME)/site-functions
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
	mkdir -p $(@D)
	ln -sf $(PWD)/zsh/completion.zsh $@

$(ZSH_CONFIG_HOME)/history.zsh:
	mkdir -p $(@D)
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
#{{{ SSH (Secure shell)

SSH_CONFIG_HOME = $(HOME)/.ssh

$(SSH_CONFIG_HOME)/config:
	mkdir -p $(@D)
	ln -sf $(PWD)/ssh/config $@

$(SSH_CONFIG_HOME)/id_rsa_test:
	mkdir -p $(@D)
	ssh-keygen -f $@ -t rsa -b 4096 -q -N "" \
	-C "Created on $(DATE) by $(USER) on $(LOCAL_HOSTNAME)"

.PHONY: ssh
ssh: $(SSH_CONFIG_HOME)/config
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

.PHONY: git
git: $(GIT_DEPS)
#}}}
#{{{ Z
# https://github.com/rupa/z/

Z_VERSION = 1.9
Z_DIR := $(HOME)/.local/src/z/$(Z_VERSION)
Z_FILES = z.sh z.1

$(CACHE_HOME)/dotfiles/z-$(Z_VERSION).tar.gz:
	mkdir -p $(@D)
	curl -fsSLo $@ https://github.com/rupa/z/archive/v$(Z_VERSION).tar.gz

$(Z_DIR)/%: | $(CACHE_HOME)/dotfiles/z-$(Z_VERSION).tar.gz
	mkdir -p $(@D)
	tar -xf  $| -C $(@D) --strip-components 1

$(DATA_HOME)/man/man1/z.1: | $(Z_DIR)/z.1
	mkdir -p $(@D)
	ln -sf $(Z_DIR)/z.1 $@

$(CACHE_HOME)/z/data: | $(Z_DIR)/z.sh
	mkdir -p $(@D)
	touch $@

.PHONY: z
z: | $(Z_FILES:%=$(Z_DIR)/%) $(DATA_HOME)/man/man1/z.1 $(CACHE_HOME)/z/data
#}}}
#{{{ Homebrew

ifeq (, $(findstring kant, $(HOME)))
BREW_PREFIX      := /usr/local
BREW_INSTALL_CMD := /usr/bin/ruby -e "$$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
else
BREW_PREFIX      := $(HOME)/.linuxbrew
BREW_INSTALL_CMD := sh -c "$$(curl -fsSL https://raw.githubusercontent.com/Linuxbrew/install/master/install.sh)"
endif

BREW_EXE := $(BREW_PREFIX)/bin/brew

$(BREW_EXE):
ifeq (kant, $(findstring kant, $(HOME)))
	$(info Press Control-D when asked for sudoer password)
endif
	$(BREW_INSTALL_CMD)

$(BREW_PREFIX)/bin/gcc: $(BREW_EXE)
	$(BREW_EXE) install gcc

BREW_DEPS := $(BREW_EXE)
ifeq (kant, $(findstring kant, $(HOME)))
BREW_DEPS += $(BREW_PREFIX)/bin/gcc
endif

.PHONY: homebrew
homebrew: $(BREW_DEPS)
	$(info You may need to reload shell)
#}}}
#{{{ Pass

PASS_EXE := $(BREW_BIN)/pass

$(PASS_EXE): | $(BREW_EXE)
	brew install pass

$(HOME)/.password-store/.git: | $(PASS_EXE)
	git clone $(GIT_REPO_BASE_PATH)/password-store.git $(@D)

.PHONY: pass
pass: | $(PASS_EXE) $(HOME)/.password-store/.git
	@echo "Remember to setup GPG keys"
#}}}
#{{{ Cheat

CHEAT_BIN         := $(BREW_PREFIX)/bin/cheat
CHEAT_CONFIG_HOME := $(CONFIG_HOME)/cheat
CHEAT_DATA_HOME   := $(DATA_HOME)/cheat
CHEAT_CHEATSHEETS := $(CHEAT_DATA_HOME)/cheatsheets
DOT_CHEAT         := $(PWD)/cheat

$(CHEAT_BIN): | $(BREW_EXE)
	$(BREW_EXE) install cheat

$(CHEAT_CONFIG_HOME)/conf.yml: | $(CHEAT_BIN)
	mkdir -p $(@D)
	ln -sf $(DOT_CHEAT)/conf.yml $@

$(CHEAT_CHEATSHEETS)/community: | $(CHEAT_BIN) $(CHEAT_CONFIG_HOME)/conf.yml
	mkdir -p $(@D)
	git clone https://github.com/cheat/cheatsheets $@

$(CHEAT_CHEATSHEETS)/personal: | $(CHEAT_BIN) $(CHEAT_CONFIG_HOME)/conf.yml
	mkdir -p $(@D)
	git clone git@github.com:pkdahl/cheatsheets.git $@

$(CHEAT_CHEATSHEETS)/work: | $(CHEAT_BIN) $(CHEAT_CONFIG_HOME)/conf.yml
	mkdir -p $(@D)
	git clone $(GIT_REPO_BASE_PATH)/cheatsheets.git $@

CHEAT_OO_DEPS := $(CHEAT_BIN)
CHEAT_OO_DEPS += $(CHEAT_CONFIG_HOME)/conf.yml
CHEAT_OO_DEPS += $(CHEAT_CHEATSHEETS)/community
CHEAT_OO_DEPS += $(CHEAT_CHEATSHEETS)/personal
CHEAT_OO_DEPS += $(CHEAT_CHEATSHEETS)/work

.PHONY: cheat
cheat: | $(CHEAT_OO_DEPS)
#}}}
#{{{ direnv

DIRENV_EXE := $(BREW_PREFIX)/bin/direnv

$(DIRENV_EXE): | $(BREW_EXE)
	$(BREW_EXE) install direnv

.PHONY: direnv
direnv: | $(DIRENV_EXE)
#}}}
#{{{ fzf
# https://github.com/junegunn/fzf

FZF_EXE         := $(BREW_PREFIX)/bin/fzf
FZF_CONFIG_HOME := $(CONFIG_HOME)/fzf

$(FZF_EXE): | $(BREW_EXE)
	$(BREW_EXE) install fzf

$(FZF_CONFIG_HOME)/fzf.zsh: | $(FZF_EXE)
	$(BREW_PREFIX)/opt/fzf/install --xdg --key-bindings --completion --no-update-rc --no-fish

.PHONY: fzf
fzf: | $(FZF_EXE) $(FZF_CONFIG_HOME)/fzf.zsh
#}}}
# ripgrep {{{

RG_EXE := $(BREW_PREFIX)/bin/rg

$(RG_EXE): | $(BREW_EXE)
	$(BREW_EXE) install ripgrep

.PHONY: ripgrep
ripgrep: | $(RG_EXE)
# }}}
#{{{ tmux

TMUX_EXE := $(BREW_PREFIX)/bin/tmux
TMUX_DATA_HOME := $(DATA_HOME)/tmux

$(TMUX_EXE): | $(BREW_EXE)
	$(BREW_EXE) install tmux

$(HOME)/.tmux.conf: | $(TMUX_EXE)
	ln -sf $(PWD)/tmux/tmux.conf $@ 

$(TMUX_DATA_HOME)/plugins/resurrect: | $(HOME)/.tmux.conf
	mkdir -p $(@D)
	git clone https://github.com/tmux-plugins/tmux-resurrect.git $@

$(TMUX_DATA_HOME)/plugins/continuum: | $(HOME)/.tmux.conf
	mkdir -p $(@D)
	git clone https://github.com/tmux-plugins/tmux-continuum.git $@

$(TMUX_DATA_HOME)/themes/nord: | $(HOME)/.tmux.conf
	mkdir -p $(@D)
	git clone https://github.com/arcticicestudio/nord-tmux $@

TMUX_OO_DEPS := $(TMUX_EXE)
TMUX_OO_DEPS += $(HOME)/.tmux.conf
TMUX_OO_DEPS += $(TMUX_DATA_HOME)/plugins/resurrect
TMUX_OO_DEPS += $(TMUX_DATA_HOME)/plugins/continuum
TMUX_OO_DEPS += $(TMUX_DATA_HOME)/themes/nord

.PHONY: tmux
tmux: | $(TMUX_OO_DEPS)
#}}}
# exa {{{

EXA_EXE := $(BREW_PREFIX)/bin/exa

$(EXA_EXE): | $(BREW_EXE)
	$(BREW_EXE) install exa

.PHONY: exa
exa: | $(EXA_EXE)
#}}}
#{{{ macOS settings

$(DATA_HOME)/dotfiles/macos-sentinel:
	bash $(PWD)/macos/dock-settings.bash
	bash $(PWD)/macos/finder-settings.bash
	mkdir -p $(@D)
	touch $@

.PHONY: macos
macos: $(DATA_HOME)/dotfiles/macos-sentinel
#}}}
#{{{ macOS applications

# iTerm

ITERM_COLORS_DIR := $(DATA_HOME)/iterm/colors
ITERM_COLORS := Nord gruvbox-dark gruvbox-light

/Applications/iTerm.app: | $(BREW_EXE)
	$(BREW_EXE) cask install iterm2

.PHONY: iterm
iterm: /Applications/iTerm.app iterm-colors

$(ITERM_COLORS_DIR)/Nord.itermcolors: | /Applications/iTerm.app
	@echo "Downloading Nord colors for iTerm"
	mkdir -p $(@D)
	curl -fLo $@ https://raw.githubusercontent.com/arcticicestudio/nord-iterm2/develop/src/xml/Nord.itermcolors
	open $@

$(ITERM_COLORS_DIR)/gruvbox-%.itermcolors: | /Applications/iTerm.app
	@echo "Downloading gruvbox-$* colors for iTerm"
	mkdir -p $(@D)
	curl -fLo $@ https://raw.githubusercontent.com/morhetz/gruvbox-contrib/master/iterm2/gruvbox-$*.itermcolors
	open $@

.PHONY: iterm-colors
iterm-colors: | $(ITERM_COLORS:%=$(ITERM_COLORS_DIR)/%.itermcolors)
# Firefox

/Applications/Firefox.app: | $(BREW_EXE)
	$(BREW_EXE) cask install firefox

.PHONY: firefox
firefox: /Applications/Firefox.app

# Karabiner-Elements

/Applications/Karabiner-Elements.app: | $(BREW_EXE)
	$(BREW_EXE) cask install karabiner-elements

$(CONFIG_HOME)/karabiner/karabiner.json: | /Applications/Karabiner-Elements.app
	mkdir -p $(@D)
	ln -sf $(PWD)/karabiner/karabiner.json $@

KARABINER_OO_DEPS := /Applications/Karabiner-Elements.app
KARABINER_OO_DEPS += $(CONFIG_HOME)/karabiner/karabiner.json

.PNONY: karabiner
karabiner: | $(KARABINER_OO_DEPS)

# Quick Look {{{

QL_DIR := $(HOME)/Library/QuickLook

$(QL_DIR)/QLColorCode.qlgenerator: | $(BREW_EXE)
	$(BREW_EXE) cask install qlcolorcode
QL_OO_DEPS := $(QL_DIR)/QLColorCode.qlgenerator

$(QL_DIR)/QLStephen.qlgenerator: | $(BREW_EXE)
	$(BREW_EXE) cask install qlstephen
QL_OO_DEPS += $(QL_DIR)/QLStephen.qlgenerator

$(QL_DIR)/QLMarkdown.qlgenerator: | $(BREW_EXE)
	$(BREW_EXE) cask install qlmarkdown
QL_OO_DEPS += $(QL_DIR)/QLMarkdown.qlgenerator

$(QL_DIR)/QuickLookCSV.qlgenerator: | $(BREW_EXE)
	$(BREW_EXE) cask install quicklook-csv
QL_OO_DEPS += $(QL_DIR)/QuickLookCSV.qlgenerator

$(QL_DIR)/QuickLookJSON.qlgenerator: | $(BREW_EXE)
	$(BREW_EXE) cask install quicklook-json
QL_OO_DEPS += $(QL_DIR)/QuickLookJSON.qlgenerator

.PHONY: quicklook
quicklook: | $(QL_OO_DEPS)
	$(info sudo xattr -r -d com.apple.quarantine ~/Library/QuickLook/QuickLookJSON.qlgenerator)

#}}}
#}}}
#{{{ Fonts

BREW_TAP_CASK_FONTS := /usr/local/Homebrew/Library/Taps/homebrew/homebrew-cask-fonts
FONTS_LIB := $(HOME)/Library/Fonts

$(BREW_TAP_CASK_FONTS): | $(BREW_EXE)
	$(BREW_EXE) tap homebrew/cask-fonts

FONT_DEJAVU := $(FONTS_LIB)/DejaVu\ Sans\ Mono\ Nerd\ Font\ Complete.ttf
$(FONT_DEJAVU): | $(BREW_TAP_CASK_FONTS)
	$(BREW_EXE) cask install font-dejavusansmono-nerd-font

FONT_FANTASQUE := $(FONTS_LIB)/Fantasque\ Sans\ Mono\ Regular\ Nerd\ Font\ Complete.ttf
$(FONT_FANTASQUE): | $(BREW_TAP_CASK_FONTS)
	$(BREW_EXE) cask install font-fantasquesansmono-nerd-font

FONT_FIRA_CODE := $(FONTS_LIB)/Fira\ Code\ Regular\ Nerd\ Font\ Complete.otf
$(FONT_FIRA_CODE): | $(BREW_TAP_CASK_FONTS)
	$(BREW_EXE) cask install font-firacode-nerd-font

FONT_IBM_3270 := $(FONTS_LIB)/3270\ Narrow\ Nerd\ Font\ Complete.otf
$(FONT_IBM_3270): | $(BREW_TAP_CASK_FONTS)
	$(BREW_EXE) cask install font-3270-nerd-font

FONT_MONOFUR := $(FONTS_LIB)/monofur\ Nerd\ Font\ Complete.ttf
$(FONT_MONOFUR): | $(BREW_TAP_CASK_FONTS)
	$(BREW_EXE) cask install font-monofur-nerd-font

FONT_SOURCE_CODE_PRO := $(FONTS_LIB)/Sauce\ Code\ Pro\ Nerd\ Font\ Complete.ttf
$(FONT_SOURCE_CODE_PRO): | $(BREW_TAP_CASK_FONTS)
	$(BREW_EXE) cask install font-sourcecodepro-nerd-font

FONTS_OO_DEPS := $(BREW_TAP_CASK_FONTS)
FOTNS_OO_DEPS += $(FONT_DEJAVU)
FONTS_OO_DEPS += $(FONT_FANTASQUE)
FONTS_OO_DEPS += $(FONT_FIRA_CODE)
FONTS_OO_DEPS += $(FONT_IBM_3270)
FONTS_OO_DEPS += $(FONT_MONOFUR)
FONTS_OO_DEPS += $(FONT_SOURCE_CODE_PRO)

.PHONY: fonts
fonts: | $(FONTS_OO_DEPS)
#}}}
#{{{ Maude
# http://maude.cs.illinois.edu/w/index.php/The_Maude_System

MAUDE_VERSION = 3.0

http://maude.cs.illinois.edu/w/images/b/bb/Maude-3.0+yices2-osx.zip
http://maude.cs.illinois.edu/w/images/0/04/Full-Maude-3.0.zip

#}}}
#{{{ Haskell

CABAL_EXE := $(BREW_PREFIX)/bin/cabal

$(CABAL_EXE): | $(BREW_EXE)
	$(BREW_EXE) install cabal-install

STACK_EXE := $(BREW_PREFIX)/bin/stack

$(STACK_EXE): | $(BREW_EXE)
	$(BREW_EXE) install stack

.PHONY: stack
stack: | $(STACK_EXE)

#}}}
#{{{ Node.js

NODE_EXE := $(BREW_PREFIX)/bin/node
NODE_DATA_HOME := $(DATA_HOME)/node
NPM_EXE := $(BREW_PREFIX)/bin/npm
NPM_CONFIG_HOME := $(CONFIG_HOME)/npm
YARN_EXE := $(BREW_PREFIX)/bin/yarn

$(NODE_EXE): | $(BREW_EXE)
	$(BREW_EXE) install node

$(NODE_DATA_HOME): | $(NODE_EXE)
	mkdir -p $@

$(NPM_CONFIG_HOME): | $(NODE_EXE)
	mkdir -p $@

$(YARN_EXE): | $(BREW_EXE) $(NODE_EXE)
	$(BREW_EXE) install yarn

NODE_OO_DEPS := $(NODE_EXE) $(NODE_DATA_HOME)
NODE_OO_DEPS += $(NPM_CONFIG_HOME)
NODE_OO_DEPS += $(YARN_EXE)
ifneq ($(command -v nvim),)
NODE_OO_DEPS += neovim-node-client
endif

.PHONY: node
node: | $(NODE_OO_DEPS)
#}}}
# OCaml {{{
OPAM_EXE := $(BREW_PREFIX)/bin/opam

$(OPAM_EXE): | $(BREW_EXE)
	$(BREW_EXE) install opam

$(HOME)/.ocamlinit: | $(OPAM_EXE)
	ln -sf $(PWD)/ocaml/ocamlinit $@

.PHONY: ocaml
ocaml: | $(OPAM_EXE) $(HOME)/.ocamlinit
# }}}
# Python 3 {{{

PYTHON3_EXE := $(BREW_PREFIX)/bin/python3
PIP3_EXE := $(BREW_PREFIX)/bin/pip3
PYTHON3_VERSION := $(shell python3 -c "import platform; print('.'.join(platform.python_version_tuple()[:2]))")
PYTHON3_SITE_PACKAGES_PATH := $(BREW_PREFIX)/lib/python$(PYTHON3_VERSION)/site-packages

$(PYTHON3_EXE): | $(BREW_EXE)
	$(BREW_EXE) install python3

$(PIP3_EXE): | $(PYTHON3_EXE)

PYTHON3_OO_DEPS := $(PYTHON3_EXE)
ifneq ($(shell command -v nvim),)
PYTHON3_OO_DEPS += neovim-pip3-pynvim
endif

.PHONY: python3
python3: | $(PYTHON3_OO_DEPS)
#}}}
#{{{ Neovim
# Depends on some Python 3 and Node for pynvim and neovim-node.
# Needs to be after because of variable usage, it seems.

DOT_NVIM         = $(PWD)/nvim
NVIM_CONFIG_HOME = $(CONFIG_HOME)/nvim
NVIM_DATA_HOME   = $(DATA_HOME)/nvim
NVIM_EXE         = /usr/local/bin/nvim

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

$(PYTHON3_SITE_PACKAGES_PATH)/pynvim: | $(PIP3_EXE)
	$(PIP3_EXE) install pynvim
.PHONY: neovim-pip3-pynvim
neovim-pip3-pynvim: $(PYTHON3_SITE_PACKAGES_PATH)/pynvim

$(BREW_PREFIX)/bin/neovim-node-host: | $(NPM_EXE)
	$(NPM_EXE) install -g neovim
.PHONY: neovim-node-client
neovim-node-client: | $(BREW_PREFIX)/bin/neovim-node-host

NVIM_OO_DEPS := $(NVIM_EXE)
NVIM_OO_DEPS += $(NVIM_CONFIG_HOME)/init.vim
NVIM_OO_DEPS += $(NVIM_DATA_HOME)/site/autoload/plug.vim
ifeq ($(PIP3_EXE), $(shell command -v pip3))
NVIM_OO_DEPS += neovim-pip3-pynvim
endif
ifeq ($(NPM_EXE), $(shell command -v npm))
NVIM_OO_DEPS += neovim-node-client
endif

.PHONY: nvim neovim
nvim: | $(NVIM_OO_DEPS)
neovim: nvim
#}}}
# Mail {{{

# Clone mail config

DOT_MAIL := $(PWD)/mail
DOT_MAIL_REPO := $(GIT_REPO_BASE_PATH)/config-mail.git
MAILDIR := $(HOME)/Maildir

$(DOT_MAIL): | git
	git clone $(DOT_MAIL_REPO) $@	

$(MAILDIR)/uio-pkdahl:
	mkdir -p $@

$(MAILDIR)/uio-perkda:
	mkdir -p $@

$(MAILDIR)/gmail:
	mkdir -p $@

$(MAILDIR)/neomutt-sidebar-spacer-0:
	touch $@

$(MAILDIR)/neomutt-sidebar-spacer-1:
	touch $@

MAILBOXES_OO_DEPS := $(MAILDIR)/uio-pkdahl
MAILBOXES_OO_DEPS += $(MAILDIR)/uio-perkda
MAILBOXES_OO_DEPS += $(MAILDIR)/gmail
MAILBOXES_OO_DEPS += $(MAILDIR)/neomutt-sidebar-spacer-0
MAILBOXES_OO_DEPS += $(MAILDIR)/neomutt-sidebar-spacer-1

.PHONY: mailboxes
mailboxes: | $(MAILBOXES_OO_DEPS)

# isync/mbsync

MBSYNC_EXE := $(BREW_PREFIX)/bin/mbsync
MBSYNC_CONFIG := $(HOME)/.mbsyncrc

$(MBSYNC_EXE): | $(BREW_EXE) 
	$(BREW_EXE) install isync

$(MBSYNC_CONFIG): | $(DOT_MAIL) $(MBSYNC_EXE)
	ln -sf $(PWD)/mail/mbsyncrc $@

.PHONY: isync
isync: | $(DOT_MAIL) mailboxes $(MBSYNC_EXE) $(MBSYNC_CONFIG)

# imapfilter

IMAPFILER_EXE := $(BREW_PREFIX)/bin/imapfilter
IMAPFILTER_CONFIG := $(CONFIG_HOME)/imapfilter/config.lua

$(IMAPFILTER_EXE): | $(BREW_EXE)
	$(BREW_EXE) install imapfilter

$(IMAPFILTER_CONFIG): mail/imapfilter_config.lua | $(DOT_MAIL) $(IMAPFILTER_EXE)
	mkdir -p $(@D)
	cp mail/imapfilter_config.lua $@

.PHONY: imapfilter
imapfilter: $(IMAPFILTER_CONFIG) | $(IMAPFILTER_EXE)

# mu - maildir-utils

MU_EXE := $(BREW_PREFIX)/bin/mu

$(MU_EXE): | $(BREW_EXE)
	$(BREW_EXE) install mu

.PHONY: mu
mu: $(MU_EXE)

# Neomutt

NEOMUTT_EXE := $(BREW_PREFIX)/bin/neomutt
NEOMUTT_CONFIG_HOME := $(CONFIG_HOME)/neomutt
NEOMUTT_CACHE_HOME := $(CACHE_HOME)/neomutt

$(NEOMUTT_EXE): | $(BREW_EXE)
	$(BREW_EXE) install neomutt

$(NEOMUTT_CONFIG_HOME): $(NEOMUTT_EXE)
	ln -sf $(DOT_MAIL)/neomutt $@

$(NEOMUTT_CACHE_HOME)/tmp : $(NEOMUTT_EXE)
	mkdir -p $@

.PHONY: neomutt
neomutt: $(NEOMUTT_EXE) $(NEOMUTT_CONFIG_HOME) $(NEOMUTT_CACHE_HOME)/tmp

.PHONY: mail
mail: | $(DOT_MAIL) $(MAILDIR) isync neomutt
# }}}
