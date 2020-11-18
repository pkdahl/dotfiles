include ../Makefile

EMACS_CONFIG_HOME := $(CONFIG_HOME)/emacs
EMACS_VERSION := $(shell emacs --version | head -1 | sed 's/GNU Emacs //')

$(EMACS_CONFIG_HOME)/init.el:
	@echo $(EMACS_CONFIG_HOME)
	mkdir -p $(@D)
	ln -sf $(PWD)/init.el $@

$(HOME)/.emacs.d:
	ln -sf $(EMACS_CONFIG_HOME) $@

OO_DEPS :=  $(EMACS_CONFIG_HOME)/init.el
ifeq (1, $(shell echo "$(EMACS_VERSION) < 27" | bc))
OO_DEPS += $(HOME)/.emacs.d
endif

.PHONY: setup
setup: | $(OO_DEPS)
