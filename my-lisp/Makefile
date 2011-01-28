EMACS = emacs

# list of core elisp files
elc_files := $(shell ls *.el | sed 's:\.el:\.elc:g')

# implicit rule for byte-compiling elisp files
%.elc: %.el
	$(EMACS) --batch \
        -L ./ \
        -L ../lisp \
        -L ../packages/icicles \
        -L ../packages/auctex/auctex \
	-L ../packages/bbdb/lisp \
	-L ../packages/completion-ui \
        -f batch-byte-compile $<

all: $(elc_files)
