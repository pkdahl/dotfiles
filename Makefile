CACHE_HOME = $(HOME)/.cache
DATA_HOME  = $(HOME)/.local/share
LIB_HOME   = $(HOME)/.local/lib

default:
	@echo "this is the default target"
	@echo "we'll do nothing"
	@echo $(CACHE_HOME) $(DATA_HOME) $(LIB_HOME)
	@echo $(CACHE_HOME)
	@echo $(HOME)

#{{{ Sh

$(HOME)/.profile:
	ln -sf $(PWD)/sh/profile $@

$(HOME)/.envir:
	ln -sf $(PWD)/sh/envir $@

.PHONY = sh
sh: $(HOME)/.profile $(HOME)/.envir
#}}}
#{{{ Zsh

ZSH_CACHE_HOME = $(CACHE_HOME)/zsh
ZSH_DATA_HOME  = $(DATA_HOME)/zsh
ZSH_LIB_HOME   = $(LIB_HOME)/zsh

$(ZSH_CACHE_HOME):
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

.PHONY = zsh
ZSH_DEPS := $(ZSH_CACHE_HOME) $(ZSH_DATA_HOME)/site-functions
ZSH_DEPS += $(ZSH_LIB_HOME)/spaceship-prompt
ZSH_DEPS += $(HOME)/.zshenv $(HOME)/.zprofile $(HOME)/.zshrc
ZSH_DEPS += sh

zsh: $(ZSH_DEPS)
#}}}
