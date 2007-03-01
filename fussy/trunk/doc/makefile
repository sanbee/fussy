FUSSYDOC=fussy.tex
LATEX=latex
LATEX2HTML:=latex2html
BIBTEX=bibtex
DVIPS=dvips

fussy.html: fussy.dvi
	$(LATEX2HTML) fussy
fussy.ps: fussy.dvi
	$(DVIPS) fussy.dvi -o fussy.ps

fussy.dvi: fussy.tex fussy.bbl
	$(LATEX) fussy.tex

fussy.aux: fussy.tex
	$(LATEX) fussy.tex

fussy.bbl: fussy.aux fussy.bib
	$(LATEX) fussy.tex
	$(BIBTEX) fussy
	$(LATEX) fussy.tex
	$(LATEX) fussy.tex

clean: 
	\rm -f *.dvi *.aux *.log *.bbl *.blg *~
