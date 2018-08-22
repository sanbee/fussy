bin_PROGRAMS = fussy

AM_CXXFLAGS = -Wno-dangling-else -Wno-parentheses-equality -Wno-constant-logical-operand -Wno-c++11-compat-deprecated-writable-strings

noinst_LIBRARIES = libkum.a libcalc.a
libkum_a_SOURCES = src/sla/djm2e.f src/sla/ep2da.f src/sla/frye.f \
				   src/sla/slacaldj.f src/sla/slacldj.f src/sla/sladcc2s.f \
				   src/sla/sladcs2c.f src/sla/sladjcl.f src/sla/sladmxv.f \
				   src/sla/sladranrm.f src/sla/slagmst.f src/sla/slaprebn.f \
				   src/sla/slapreces.f src/sla/slaprec.f

libcalc_a_SOURCES = src/fussyparse.yy src/calcinit.cc src/tables.cc src/vcode.cc \
				    src/prtFuncs.cc src/ErrorObj.cc src/func.cc src/BitField.cc \
					src/IDResource.cc src/ExitMsg.cc

src/fussyparse.yy: src/lex.yy.c

src/lex.yy.c: src/fussylex.l
	$(LEX) $<

src/vm.a:
	(cd src; sh ./vmMake $(CXX) -g -Wall -I. -Iemath  > vm.a)

src/type.a: src/fussyparse.hh
	(cd src; sh ./typeMake ./fussyparse.hh > type.a)

src/prtFuncs.cc: src/vm.a src/type.a

AM_YFLAGS = -d -t -y -p calc_

libcalc_a_CXXFLAGS = $(AM_CXXFLAGS) -Isrc -Isrc/emath
libcalc_a_CFLAGS = $(AM_CFLAGS) -Isrc -Isrc/emath

fussy_CXXFLAGS = $(AM_CXXFLAGS) -Isrc -Isrc/emath
fussy_CFLAGS = $(AM_CFLAGS) -Isrc -Isrc/emath
fussy_SOURCES = src/fussy.cc src/vcode.cc

fussy_LDADD = libkum.a libcalc.a

doc_DATA = doc/LaTeX/fussy.tex
if HAVE_PDFLATEX
doc_DATA += doc/LaTeX/fussy.pdf
doc/LaTeX/fussy.pdf: doc/LaTeX/fussy.tex
	cd doc/LaTeX && $(PDFLATEX) fussy && $(PDFLATEX) fussy && $(PDFLATEX) fussy
endif
