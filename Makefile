EMACS = emacs

# list of core elisp files
elc_files := $(shell ls *.el | grep -v ecb | sed 's:\.el:\.elc:g')

# implicit rule for byte-compiling elisp files
%.elc: %.el
	$(EMACS) -batch -L . \
	-L lisp \
	-L my-lisp \
	-L packages/anything-config \
	-L packages/anything-config/extensions \
	-L packages/apel \
	-L packages/auctex \
        -L packages/auto-complete \
        -L packages/auto-complete-clang \
	-L packages/babel \
	-L packages/bbdb/lisp \
	-L packages/bbdb/bbdbv3-wl \
	-L packages/cl-lookup \
	-L packages/completion-ui \
	-L packages/doremi \
	-L packages/ebib/src \
	-L packages/ectags \
        -L packages/emacs-calfw \
        -L packages/emacs-clang-complete-async \
        -L packages/emacs-window-manager \
        -L packages/emacs-window-layout \
	-L packages/emsane \
	-L packages/emms/lisp \
	-L packages/flim \
        -L packages/git-modes \
	-L packages/gnuplot \
	-L packages/icicles \
	-L packages/ioccur \
	-L packages/imaxima \
	-L packages/magit \
	-L packages/mo-git-blame \
	-L packages/org-mode/lisp \
	-L packages/org-mode/contrib/lisp \
        -L packages/popup-el \
	-L packages/semi \
	-L packages/slime \
        -L packages/undo-tree \
	-L packages/w3m \
	-L packages/wanderlust/wl \
	-L packages/wanderlust/elmo \
	-L packages/wanderlust/utils \
	-L packages/wget \
	-L packages/yasnippet \
	-f batch-byte-compile $<

all: $(elc_files)
