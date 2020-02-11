SHELL := bash

CACHE_HOME  = $(HOME)/.cache
CONFIG_HOME = $(HOME)/.config
DATA_HOME   = $(HOME)/.local/share
LIB_HOME    = $(HOME)/.local/lib

.DEFAULT_TARGET: help

help: 
	@echo "Usage: make {sh|zsh}"

.PHONY: help

#{{{ Common Bourne settings

$(HOME)/.profile:
	ln -sf $(PWD)/sh/profile $@

$(HOME)/.envir:
	ln -sf $(PWD)/sh/envir $@

sh: $(HOME)/.profile $(HOME)/.envir

.PHONY = sh
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
	mkdir -p $(ZSH_LIB_HOME)
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

$(ZSH_CONFIG_HOME)/aliases.zsh: $(ZSH_CONFIG_HOME)
	ln -sf $(PWD)/zsh/aliases.zsh $@

$(ZSH_CONFIG_HOME)/completion.zsh: $(ZSH_CONFIG_HOME)
	ln -sf $(PWD)/zsh/completion.zsh $@

$(ZSH_CONFIG_HOME)/history.zsh: $(ZSH_CONFIG_HOME)
	ln -sf $(PWD)/zsh/history.zsh $@

ZSH_DEPS := $(ZSH_CACHE_HOME) $(ZSH_CONFIG_HOME)
ZSH_DEPS += $(ZSH_DATA_HOME)/site-functions
ZSH_DEPS += $(ZSH_LIB_HOME)/spaceship-prompt
ZSH_DEPS += $(HOME)/.zshenv $(HOME)/.zprofile $(HOME)/.zshrc
ZSH_DEPS += $(ZSH_CONFIG_HOME)/aliases.zsh $(ZSH_CONFIG_HOME)/completion.zsh
ZSH_DEPS += $(ZSH_CONFIG_HOME)/history.zsh
ZSH_DEPS += sh

zsh: $(ZSH_DEPS)

.PHONY = zsh
#}}}
