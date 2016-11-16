DOTEMACS = ~/.emacs.d
include $(DOTEMACS)/Makefile.general

all:
	@$(MAKE) -C lisp
	@$(MAKE) -C my-lisp
	@$(MAKE) -C init

clean:
	@rm -f *.elc
	@$(MAKE) -C lisp clean
	@$(MAKE) -C my-lisp clean
	@$(MAKE) -C init clean
