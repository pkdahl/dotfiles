include root.mk

CUR_DIR := $(shell dirname $(abspath $(firstword ${MAKEFILE_LIST})))
TASK_CONFIG = $(CONFIG_HOME)/task/taskrc
TASK_DATA_HOME = $(DATA_HOME)/task
TASK_PROFILE = ~/.local/etc/profile.d/task

.PHONY: config data profile

$(TASK_PROFILE):
	mkdir -p $(@D)
	ln -sf ${CUR_DIR}/profile $@
profile: | $(TASK_PROFILE)

$(TASK_CONFIG):
	mkdir -p $(@D)
	ln -sf ${CUR_DIR}/taskrc $@ 
config: | $(TASK_CONFIG)

$(TASK_DATA_HOME):
	mkdir -p $@
data: | $(TASK_DATA_HOME)

