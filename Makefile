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
	-L packages/bbdb/lisp \
	-L packages/ca2 \
	-L packages/cl-lookup \
	-L packages/completion-ui \
	-L packages/doremi \
	-L packages/ebib/src \
	-L packages/ecb \
	-L packages/emms/lisp \
	-L packages/flim \
	-L packages/icicles \
	-L packages/magit \
	-L packages/muse/lisp \
	-L packages/org-mode/lisp \
	-L packages/org-mode/contrib/lisp \
	-L packages/predictive \
	-L packages/semi \
	-L packages/slime \
        -L packages/undo-tree \
	-L packages/w3/lisp \
	-L packages/w3m \
	-L packages/wget \
	-L packages/wanderlust/wl \
	-L packages/wanderlust/elmo \
	-L packages/wanderlust/utils \
	-L packages/yasnippet \
	-f batch-byte-compile $<

all: $(elc_files)
