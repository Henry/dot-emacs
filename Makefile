EMACS = emacs
DOTEMACS = ~/.emacs.d

# list of core elisp files
elc_files := $(shell ls *.el | sed 's:\.el:\.elc:g')

# implicit rule for byte-compiling elisp files
%.elc: %.el Makefile
	$(EMACS) --eval "(setq delete-old-versions t)" -batch -L . \
	-L $(DOTEMACS)/init \
	-L $(DOTEMACS)/init/personal \
	-L $(DOTEMACS)/lisp \
	-L $(DOTEMACS)/my-lisp \
	-f batch-byte-compile $<

all: lisp my-lisp $(elc_files)

.PHONY: lisp my-lisp clean

lisp:
	$(MAKE) -C lisp

my-lisp:
	$(MAKE) -C my-lisp

clean:
	rm -f *.elc
