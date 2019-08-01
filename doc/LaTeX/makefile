#******************************************************************
# * Copyright (c) 2000-2018, 2019 S.Bhatnagar
# *
# * This program is free software; you can redistribute it and/or modify
# * it under the terms of the GNU General Public License as published by
# * the Free Software Foundation; either version 2 of the License, or
# * (at your option) any later version.
# *
# * This program is distributed in the hope that it will be useful,
# * but WITHOUT ANY WARRANTY; without even the implied warranty of
# * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# * GNU General Public License for more details.
# *
# * You should have received a copy of the GNU General Public License
# * along with this program; if not, write to the Free Software
# * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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
