FUSSYDOC=fussy.tex
LATEX=latex
LATEX2HTML:=latex2html
BIBTEX=bibtex
DVIPS=dvips
SOURCEDIR=../LaTeX

fussy.html: $(SOURCEDIR)/fussy.dvi
	cd $(SOURCEDIR); gmake fussy.html; cp fussy/* ../HTML

$(SOURCEDIR)/fussy.dvi: 
	cd $(SOURCEDIR); gmake fussy.dvi

$(SOURCEDIR)/fussy.aux: 
	cd $(SOURCEDIR); gmake fussy.aux

$(SOURCEDIR)/fussy.bbl: 
	cd $(SOURCEDIR); gmake fussy.bbl

clean: 
	cd $(SOURCEDIR); gmake clean;
