FUSSYDOC=fussy.tex
LATEX=latex
LATEX2HTML:=htlatex
BIBTEX=bibtex
DVIPS=dvips
SOURCEDIR=LaTeX

all: 
	cd $(SOURCEDIR); make
	cd HTML; make

fussy.ps:
	cd $(SOURCEDIR); make

fussy.html: 
	cd HTML; make

clean:
	cd HTML;make clean;
