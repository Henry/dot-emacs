EMACS = emacs
DOTEMACS = ~/.emacs.d
PACKAGES = $(DOTEMACS)/packages

# list of core elisp files
elc_files := $(shell ls *.el | sed 's:\.el:\.elc:g')

# implicit rule for byte-compiling elisp files
%.elc: %.el
	$(EMACS) --batch \
	-L ./ \
	-L $(DOTEMACS)/lisp \
	-L tree-mode \
	-L $(PACKAGES)/icicles \
	-L $(PACKAGES)/auctex/auctex \
	-L $(PACKAGES)/bbdb/lisp \
	-L $(PACKAGES)/completion-ui \
	-L $(PACKAGES)/emacs-window-manager \
	-L $(PACKAGES)/emacs-window-layout \
	-f batch-byte-compile $<

all: $(elc_files)
	make -C tree-mode
