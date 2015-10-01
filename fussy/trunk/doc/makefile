FUSSYDOC=fussy.tex
LATEX=latex
LATEX2HTML:=htlatex
BIBTEX=bibtex
DVIPS=dvips
SOURCEDIR=LaTeX

all: 
	cd $(SOURCEDIR); gmake
	cd HTML; gmake

fussy.ps:
	cd $(SOURCEDIR); gmake

fussy.html: 
	cd HTML; gmake

clean:
	cd HTML;gmake clean;
