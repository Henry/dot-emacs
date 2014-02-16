EMACS = emacs
DOTEMACS = ~/.emacs.d
PACKAGES = $(DOTEMACS)/packages

# list of core elisp files
elc_files := $(shell ls *.el | grep -v ecb | sed 's:\.el:\.elc:g')

# implicit rule for byte-compiling elisp files
%.elc: %.el Makefile
	$(EMACS) --eval "(setq delete-old-versions t)" -batch \
	-L . \
	-L $(DOTEMACS)/lisp \
	-L $(DOTEMACS)/my-lisp \
	-L $(PACKAGES)/anything-config \
	-L $(PACKAGES)/anything-config/extensions \
	-L $(PACKAGES)/apel \
	-L $(PACKAGES)/auctex \
        -L $(PACKAGES)/auto-complete \
        -L $(PACKAGES)/auto-complete-clang \
	-L $(PACKAGES)/babel \
	-L $(PACKAGES)/bbdb/lisp \
	-L $(PACKAGES)/bbdb/bbdbv3-wl \
	-L $(PACKAGES)/cl-lookup \
	-L $(PACKAGES)/completion-ui \
	-L $(PACKAGES)/doremi \
	-L $(PACKAGES)/ebib/src \
	-L $(PACKAGES)/ectags \
        -L $(PACKAGES)/emacs-calfw \
        -L $(PACKAGES)/emacs-clang-complete-async \
        -L $(PACKAGES)/emacs-window-manager \
        -L $(PACKAGES)/emacs-window-layout \
	-L $(PACKAGES)/emsane \
	-L $(PACKAGES)/emms/lisp \
	-L $(PACKAGES)/flim \
        -L $(PACKAGES)/git-modes \
	-L $(PACKAGES)/gnuplot \
	-L $(PACKAGES)/haskell-mode \
	-L $(PACKAGES)/icicles \
	-L $(PACKAGES)/ioccur \
	-L $(PACKAGES)/imaxima \
	-L $(PACKAGES)/magit \
	-L $(PACKAGES)/mo-git-blame \
	-L $(PACKAGES)/org-mode/lisp \
	-L $(PACKAGES)/org-mode/contrib/lisp \
        -L $(PACKAGES)/paredit \
        -L $(PACKAGES)/popup-el \
	-L $(PACKAGES)/semi \
	-L $(PACKAGES)/slime \
        -L $(PACKAGES)/undo-tree \
	-L $(PACKAGES)/w3m \
	-L $(PACKAGES)/wanderlust/wl \
	-L $(PACKAGES)/wanderlust/elmo \
	-L $(PACKAGES)/wanderlust/utils \
	-L $(PACKAGES)/wget \
	-L $(PACKAGES)/yasnippet \
	-f batch-byte-compile $<

all: $(elc_files)
	$(MAKE) -C personal
