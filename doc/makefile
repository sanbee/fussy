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
