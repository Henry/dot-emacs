EMACS = emacs
DOTEMACS = ~/.emacs.d
PACKAGES = $(DOTEMACS)/packages

# list of core elisp files
elc_files := $(shell ls *.el | sed 's:\.el:\.elc:g')

# implicit rule for byte-compiling elisp files
%.elc: %.el Makefile
	$(EMACS) --eval "(setq delete-old-versions t)" --batch \
        -L ./ \
        -L $(DOTEMACS)/lisp \
        -L $(PACKAGES)/icicles \
	-L $(PACKAGES)/anything-config \
	-L $(PACKAGES)/anything-config/extensions \
        -L $(PACKAGES)/auctex/auctex \
	-L $(PACKAGES)/bbdb/lisp \
	-L $(PACKAGES)/completion-ui \
        -L $(PACKAGES)/emacs-calfw \
        -f batch-byte-compile $<

all: $(elc_files)
