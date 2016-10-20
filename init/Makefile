EMACS = emacs
DOTEMACS = ~/.emacs.d
PACKAGES = $(DOTEMACS)/packages

# list of core elisp files
elc_files := $(shell ls *.el | grep -v ecb | sed 's:\.el:\.elc:g')

# implicit rule for byte-compiling elisp files
%.elc: %.el Makefile
	$(EMACS) \
	--eval "(setq delete-old-versions t)" \
	-batch \
	-L . \
	-L $(DOTEMACS)/lisp \
	-L $(DOTEMACS)/my-lisp \
	-L $(PACKAGES)/apel \
	-L $(PACKAGES)/auctex \
	-L $(PACKAGES)/auto-complete \
	-L $(PACKAGES)/babel \
	-L $(PACKAGES)/bbdb/lisp \
	-L $(PACKAGES)/cl-lookup \
	-L $(PACKAGES)/completion-ui \
	-L $(PACKAGES)/company-mode \
	-L $(PACKAGES)/dash \
	-L $(PACKAGES)/doremi \
	-L $(PACKAGES)/ebib/src \
	-L $(PACKAGES)/ectags \
	-L $(PACKAGES)/emacs-calfw \
	-L $(PACKAGES)/emacs-window-manager \
	-L $(PACKAGES)/emacs-window-layout \
	-L $(PACKAGES)/emacs-epc \
	-L $(PACKAGES)/emacs-deferred \
	-L $(PACKAGES)/emacs-ctable \
	-L $(PACKAGES)/emms/lisp \
	-L $(PACKAGES)/flim \
	-L $(PACKAGES)/gnuplot \
	-L $(PACKAGES)/haskell-mode \
	-L $(PACKAGES)/icicles \
	-L $(PACKAGES)/ioccur \
	-L $(PACKAGES)/imaxima \
	-L $(PACKAGES)/magit/lisp \
	-L $(PACKAGES)/nim-mode \
	-L $(PACKAGES)/org-mode/lisp \
	-L $(PACKAGES)/org-mode/contrib/lisp \
	-L $(PACKAGES)/paredit \
	-L $(PACKAGES)/popup-el \
	-L $(PACKAGES)/semi \
	-L $(PACKAGES)/undo-tree \
	-L $(PACKAGES)/wanderlust/wl \
	-L $(PACKAGES)/wanderlust/elmo \
	-L $(PACKAGES)/wanderlust/utils \
	-L $(PACKAGES)/wget \
	-L $(PACKAGES)/with-editor \
	-L $(PACKAGES)/yasnippet \
	-f batch-byte-compile $<

all: $(elc_files)
	$(MAKE) -C personal
