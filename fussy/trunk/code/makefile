#-*-makefile-*-
# $Id: makefile,v 1.3 2006/03/10 21:38:37 sbhatnag Exp $

# * Copyright (c) 2000-2016, 2017 S.Bhatnagar
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
#
#  History:
#
#    First version:
#
#                 Sanjay Bhatnagar, May 2000
#                 sanjay@ncra.tifr.res.in
#
#----------------------------------------------------------------
#
CCHOME        = /usr/local/compilers/9/gcc_3.2.3
CCHOME        = /usr

LEX           = $(CCHOME)/bin/flex
#YACC          = bison -v -y -p calc_
YACC          = $(CCHOME)/bin/bison -t -y -p calc_
CCINCL        = $(CCHOME)/include
CCPATH        = $(CCHOME)/bin/
CC            = $(CCPATH)g++ $(CFLAGS) $(INCL)
CFLAGS        = -g -Wall
# -DVERBOSE
# -DYYDEBUG
# -DLDBGMODE
# -DVMDBGMODE
FUSSY_Y       = fussy.y 
READLINE      = 
PUB           = $(GLIB)

ROOT          = .
SLA           = $(ROOT)/sla/libkum.a
READLINEINCL  = . 
INCL          = -I. -I$(READLINEINCL) -I$(ROOT)/emath #-I/usr/freeware/include/g++

GENLIBS       = $(READLINE) $(SLA) \
	        -lreadline -lhistory -ltermcap -lm
#-lg2c -lm

OBJS          = calc.o calcinit.o tables.o vcode.o prtFuncs.o ErrorObj.o func.o \
		BitField.o IDResource.o ExitMsg.o
VMMAKE        = sh ./vmMake
TYPEMAKE      = sh ./typeMake
#
#----------------------------------------------------------------
#
fussy: updatecopyright fussy.o libcalc.a $(SLA) *.cc *.c *.h
	$(CC) fussy.o libcalc.a $(INCS) $(GENLIBS) -o fussy

$(SLA): 
	cd $(ROOT)/sla;make

updatecopyright: *.cc *.c *.h
	../scripts/updatecopyright
	\rm -f *.old

libcalc.a: $(OBJS) AngFmt.h
	ar r libcalc.a $(OBJS)
#	if [ -f $(RANLIB) ]; then $(RANLIB) libcalc.a; fi

y.tab.c: $(FUSSY_Y) lex.yy.c yyerror.c calc_lex_bison.h func.h tables.h IDResource.h
	$(YACC) $(FUSSY_Y)

lex.yy.c: fussy.l
	$(LEX) fussy.l

y.tab.h: $(FUSSY_Y) lex.yy.c yyerror.c
	$(YACC) -d $(FUSSY_Y)

ExitMsg.o: ExitMsg.cc
	$(CC) -c ExitMsg.cc

func.o: func.cc calc.h
	$(CC) -c func.cc

ErrorObj.o: ErrorObj.cc ErrorObj.h
	$(CC) -c ErrorObj.cc -o ErrorObj.o

vm.a: calc.h vmMake
	$(VMMAKE) $(CC) > vm.a

type.a: y.tab.h typeMake
	$(TYPEMAKE) y.tab.h > type.a

prtFuncs.o: prtFuncs.cc vm.a y.tab.h type.a
	$(CC) -c prtFuncs.cc -o prtFuncs.o

vcode.o: vcode.cc calc.h
	$(CC) -c vcode.cc -o vcode.o

calc.o: y.tab.c calc.h
	$(CC) -c y.tab.c -o calc.o

calcinit.o:calcinit.cc y.tab.h AngFmt.cc AngFmt.h
	$(CC) -c calcinit.cc

tables.o:tables.cc y.tab.h tables.h
	$(CC) -c tables.cc -o tables.o

BitField.o:BitField.cc BitField.h
	$(CC) -c BitField.cc -o BitField.o

IDResource.o: IDResource.cc IDResource.h BitField.h
	$(CC) -c IDResource.cc -o IDResource.o

fussy.o: fussy.cc y.tab.h ErrorObj.h calc.h defns.h units.h
	$(CC) -c fussy.cc

check:   fussy test/fussytest
	cd test;./fussytest

clean: 
	\rm -rf *.o *~ tst *.a *.tab.h *.tab.c lex.yy.c fussy *.dvi *.log *.aux

allclean: clean
	cd $(ROOT)/sla; make clean

depend:
	makedepend -Y/usr/include/g++-2 -Y/usr/include $(INCL) $(CFLAGS) *.{cc,h,l,y,c}
# DO NOT DELETE
