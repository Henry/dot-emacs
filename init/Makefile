EMACS = emacs
DOTEMACS = ~/.emacs.d

# list of core elisp files
elc_files := $(shell ls *.el | sed 's:\.el:\.elc:g')

# implicit rule for byte-compiling elisp files
%.elc: %.el Makefile
	$(EMACS) \
	--eval "(progn (setq delete-old-versions t) (require 'package) (add-to-list 'package-archives '(\"melpa\" . \"http://melpa.org/packages/\") t) (add-to-list 'package-archives '(\"marmalade\" . \"https://marmalade-repo.org/packages/\") t) (setq package-check-signature nil) (package-initialize) (package-install 'use-package) (setq use-package-verbose t) (setq use-package-always-ensure t))" \
	-batch \
	-L . \
	-L $(DOTEMACS)/lisp \
	-L $(DOTEMACS)/my-lisp \
	-f batch-byte-compile $<

all: $(elc_files)

.PHONY: clean

clean:
	rm -f *.elc
