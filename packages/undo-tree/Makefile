EMACS = emacs

# list of core elisp files
elc_files := $(shell ls *.el | grep -v ecb | sed 's:\.el:\.elc:g')

# implicit rule for byte-compiling elisp files
%.elc: %.el
	$(EMACS) -batch -L . \
	-L lisp \
	-L my-lisp \
	-L packages/apt-el \
	-L packages/bbdb/lisp \
	-L packages/ca2 \
	-L packages/cedet/common/cedet \
	-L packages/cedet/common \
	-L packages/cedet/cogre \
	-L packages/cedet/ede \
	-L packages/cedet/eieio \
	-L packages/cedet/speedbar \
        -L packages/cedet/semantic \
        -L packages/cedet/semantic/bovine \
        -L packages/cedet/semantic/symref \
        -L packages/cedet/contrib \
	-L packages/cl-lookup \
	-L packages/completion-ui \
	-L packages/doremi \
	-L packages/ecb \
	-L packages/ee \
	-L packages/emms/lisp \
	-L packages/magit \
	-L packages/egg \
	-L packages/org-mode/lisp \
	-L packages/org-mode/contrib/lisp \
	-L packages/icicles \
	-L packages/auctex \
	-L packages/ebib/src \
	-L packages/muse/lisp \
	-L packages/predictive \
	-L packages/w3/lisp \
	-L packages/w3m \
	-L packages/wget \
	-L packages/wanderlust/wl \
	-L packages/wanderlust/elmo \
	-L packages/wanderlust/utils \
	-L packages/semi \
	-L packages/flim \
	-L packages/apel \
	-L packages/yasnippet \
	-L packages/slime \
	-L packages/elscreen \
	-f batch-byte-compile $<

all: $(elc_files)
