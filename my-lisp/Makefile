DOTEMACS = ~/.emacs.d
include $(DOTEMACS)/Makefile.general

all:
	@$(MAKE) -C tree-mode

.PHONY: clean
clean: cleanelc
	@$(MAKE) -C tree-mode clean
