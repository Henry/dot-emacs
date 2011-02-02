# Copyright 2006 Karl Berry.
# You may freely use, modify and/or distribute this file.
# Makefile for latexrefman.

texi2dvi = texi2dvi --batch --build-dir=$*.t2dvi
texi2pdf = texi2pdf --batch --build-dir=$*.t2pdf
#
texi2docbook = makeinfo --docbook
texi2html = makeinfo --no-split --html
texi2info = makeinfo --no-split
texi2txt = makeinfo --no-split --plaintext -o $@
texi2xml = makeinfo --xml

%.pdf: %.texi
	$(texi2pdf) $<
%.dvi: %.texi
	$(texi2dvi) $<
%.dbk: %.texi
	$(texi2docbook) -o $@ $<
%.html: %.texi
	$(texi2html) $<
%.info: %.texi
	$(texi2info) $<
%.txt: %.texi
	$(texi2txt) $<
%.xml: %.texi
	$(texi2xml) $<

tex_output = latex2e.dvi latex2e.pdf
makeinfo_output = latex2e.info latex2e.html latex2e.txt latex2e.xml latex2e.dbk
all: makeinfo_output tex_output
makeinfo_output: $(makeinfo_output)
tex_output: $(tex_output)

mostlyclean clean:
	rm -rf latex2e.t2*

realclean distclean: clean
	rm -f $(all)
