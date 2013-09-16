EMACS = emacs

# list of core elisp files
elc_files := $(shell ls *.el | sed 's:\.el:\.elc:g')

# implicit rule for byte-compiling elisp files
%.elc: %.el
	$(EMACS) --batch \
        -L ./ \
        -L ../lisp \
        -L ../packages/icicles \
	-L ../packages/anything-config \
	-L ../packages/anything-config/extensions \
        -L ../packages/auctex/auctex \
	-L ../packages/bbdb/lisp \
	-L ../packages/completion-ui \
        -L ../packages/emacs-window-manager \
        -L ../packages/emacs-window-layout \
        -f batch-byte-compile $<

all: $(elc_files)
