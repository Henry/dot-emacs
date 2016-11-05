EMACS = emacs

# list of core elisp files
elc_files := $(shell ls *.el | sed 's:\.el:\.elc:g')

# implicit rule for byte-compiling elisp files
%.elc: %.el
	$(EMACS) --batch -L . -L ../../lisp -f batch-byte-compile $<

all: $(elc_files)

.PHONY: clean

clean:
	rm -f *.elc
