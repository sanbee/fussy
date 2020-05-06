#******************************************************************
# * Copyright (c) 2000-2019, 2020 S.Bhatnagar
# *
#   This file is part of fussy.
#
#   fussy is a free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   fussy is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with fussy.  If not, see <https://www.gnu.org/licenses/>.
# *
#******************************************************************
FUSSYDOC=fussy.tex
LATEX=latex
LATEX2HTML:=htlatex
BIBTEX=bibtex
DVIPS=dvips
SOURCEDIR=.

fussy.ps: fussy.dvi
	$(DVIPS) fussy.dvi -o fussy.ps

fussy.html: fussy.dvi
	$(LATEX2HTML) fussy

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
	\rm -rf *.dvi *.pdf *.out *.aux *.log *.blg *.toc *.ps *~ fussy
