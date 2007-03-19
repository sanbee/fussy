#-*-makefile-*-
# $Id: makefile,v 1.3 2006/03/10 21:38:37 sbhatnag Exp $

# * Copyright (c) 2000-2006, 2007 S.Bhatnagar
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
LEX           = flex
#YACC          = bison -v -y -p calc_
YACC          = bison -y -p calc_
CCHOME        = /usr/local/compilers/9/gcc_3.2.3
CCHOME        = /usr
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
INCL          = -I. -I$(READLINEINCL) -I$(ROOT)/emath -I/usr/freeware/include/g++

GENLIBS       = $(READLINE) $(SLA) \
	        -lreadline -lhistory -ltermcap -lfl -lg2c -lm

OBJS          = calc.o calcinit.o tables.o vcode.o prtFuncs.o ErrorObj.o func.o \
		BitField.o IDResource.o ExitMsg.o
#
#----------------------------------------------------------------
#
fussy: fussy.o libcalc.a $(SLA)
	$(CC) fussy.o libcalc.a $(INCS) $(GENLIBS) -o fussy

$(SLA): 
	cd $(ROOT)/sla;gmake

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
	./vmMake $(CC) > vm.a

type.a: y.tab.h typeMake
	./typeMake y.tab.h > type.a

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
	cd $(ROOT)/sla; gmake clean

depend:
	makedepend -Y/usr/include/g++-2 -Y/usr/include $(INCL) $(CFLAGS) *.{cc,h,l,y,c}
# DO NOT DELETE

AngFmt.o: ./AngFmt.h ./emath/emath.h /usr/include/math.h
AngFmt.o: /usr/include/features.h /usr/include/sys/cdefs.h
AngFmt.o: /usr/include/gnu/stubs.h /usr/include/bits/huge_val.h
AngFmt.o: /usr/include/bits/mathdef.h /usr/include/bits/mathcalls.h
AngFmt.o: ./emath/Err.h ./units.h /usr/include/string.h /usr/include/ctype.h
AngFmt.o: /usr/include/bits/types.h /usr/include/bits/wordsize.h
AngFmt.o: /usr/include/bits/typesizes.h /usr/include/endian.h
AngFmt.o: /usr/include/bits/endian.h
BitField.o: ./BitField.h /usr/include/stdio.h /usr/include/features.h
BitField.o: /usr/include/sys/cdefs.h /usr/include/gnu/stubs.h
BitField.o: /usr/include/bits/types.h /usr/include/bits/wordsize.h
BitField.o: /usr/include/bits/typesizes.h /usr/include/libio.h
BitField.o: /usr/include/_G_config.h /usr/include/wchar.h
BitField.o: /usr/include/bits/wchar.h /usr/include/gconv.h
BitField.o: /usr/include/bits/stdio_lim.h /usr/include/bits/sys_errlist.h
BitField.o: /usr/include/stdlib.h namespace.h /usr/include/math.h
BitField.o: /usr/include/bits/huge_val.h /usr/include/bits/mathdef.h
BitField.o: /usr/include/bits/mathcalls.h
calcinit.o: /usr/include/stdio.h /usr/include/features.h
calcinit.o: /usr/include/sys/cdefs.h /usr/include/gnu/stubs.h
calcinit.o: /usr/include/bits/types.h /usr/include/bits/wordsize.h
calcinit.o: /usr/include/bits/typesizes.h /usr/include/libio.h
calcinit.o: /usr/include/_G_config.h /usr/include/wchar.h
calcinit.o: /usr/include/bits/wchar.h /usr/include/gconv.h
calcinit.o: /usr/include/bits/stdio_lim.h /usr/include/bits/sys_errlist.h
calcinit.o: ./calc.h /usr/include/math.h /usr/include/bits/huge_val.h
calcinit.o: /usr/include/bits/mathdef.h /usr/include/bits/mathcalls.h
calcinit.o: ./emath/emath.h ./emath/Err.h ./units.h ./defns.h ./BitField.h
calcinit.o: /usr/include/stdlib.h namespace.h ./calcinit.h
calcinit.o: /usr/include/values.h /usr/include/limits.h ./tables.h
calcinit.o: ./IDResource.h ./AngFmt.h AngFmt.cc /usr/include/string.h
calcinit.o: /usr/include/ctype.h /usr/include/endian.h
calcinit.o: /usr/include/bits/endian.h ./ErrorObj.h
ErrorObj.o: ./ErrorObj.h namespace.h
ExitMsg.o: /usr/include/math.h /usr/include/features.h
ExitMsg.o: /usr/include/sys/cdefs.h /usr/include/gnu/stubs.h
ExitMsg.o: /usr/include/bits/huge_val.h /usr/include/bits/mathdef.h
ExitMsg.o: /usr/include/bits/mathcalls.h ./defns.h ./emath/emath.h
ExitMsg.o: ./emath/Err.h ./BitField.h /usr/include/stdio.h
ExitMsg.o: /usr/include/bits/types.h /usr/include/bits/wordsize.h
ExitMsg.o: /usr/include/bits/typesizes.h /usr/include/libio.h
ExitMsg.o: /usr/include/_G_config.h /usr/include/wchar.h
ExitMsg.o: /usr/include/bits/wchar.h /usr/include/gconv.h
ExitMsg.o: /usr/include/bits/stdio_lim.h /usr/include/bits/sys_errlist.h
ExitMsg.o: /usr/include/stdlib.h namespace.h
func.o: /usr/include/stdio.h /usr/include/features.h /usr/include/sys/cdefs.h
func.o: /usr/include/gnu/stubs.h /usr/include/bits/types.h
func.o: /usr/include/bits/wordsize.h /usr/include/bits/typesizes.h
func.o: /usr/include/libio.h /usr/include/_G_config.h /usr/include/wchar.h
func.o: /usr/include/bits/wchar.h /usr/include/gconv.h
func.o: /usr/include/bits/stdio_lim.h /usr/include/bits/sys_errlist.h
func.o: ./calc.h /usr/include/math.h /usr/include/bits/huge_val.h
func.o: /usr/include/bits/mathdef.h /usr/include/bits/mathcalls.h
func.o: ./emath/emath.h ./emath/Err.h ./units.h ./defns.h ./BitField.h
func.o: /usr/include/stdlib.h namespace.h ./func.h ./tables.h
func.o: /usr/include/values.h /usr/include/limits.h
fussy.o: /usr/include/stdio.h /usr/include/features.h
fussy.o: /usr/include/sys/cdefs.h /usr/include/gnu/stubs.h
fussy.o: /usr/include/bits/types.h /usr/include/bits/wordsize.h
fussy.o: /usr/include/bits/typesizes.h /usr/include/libio.h
fussy.o: /usr/include/_G_config.h /usr/include/wchar.h
fussy.o: /usr/include/bits/wchar.h /usr/include/gconv.h
fussy.o: /usr/include/bits/stdio_lim.h /usr/include/bits/sys_errlist.h
fussy.o: ./calc.h /usr/include/math.h /usr/include/bits/huge_val.h
fussy.o: /usr/include/bits/mathdef.h /usr/include/bits/mathcalls.h
fussy.o: ./emath/emath.h ./emath/Err.h ./units.h ./defns.h ./BitField.h
fussy.o: /usr/include/stdlib.h namespace.h ./ErrorObj.h /usr/include/signal.h
fussy.o: /usr/include/bits/sigset.h /usr/include/bits/signum.h
fussy.o: /usr/include/errno.h /usr/include/bits/errno.h
fussy.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
fussy.o: /usr/include/sys/types.h /usr/include/time.h /usr/include/unistd.h
fussy.o: /usr/include/bits/posix_opt.h /usr/include/bits/confname.h
fussy.o: /usr/include/values.h /usr/include/limits.h
IDResource.o: ./IDResource.h /usr/include/stdio.h /usr/include/features.h
IDResource.o: /usr/include/sys/cdefs.h /usr/include/gnu/stubs.h
IDResource.o: /usr/include/bits/types.h /usr/include/bits/wordsize.h
IDResource.o: /usr/include/bits/typesizes.h /usr/include/libio.h
IDResource.o: /usr/include/_G_config.h /usr/include/wchar.h
IDResource.o: /usr/include/bits/wchar.h /usr/include/gconv.h
IDResource.o: /usr/include/bits/stdio_lim.h /usr/include/bits/sys_errlist.h
IDResource.o: /usr/include/stdlib.h ./BitField.h namespace.h
IDResource.o: /usr/include/math.h /usr/include/bits/huge_val.h
IDResource.o: /usr/include/bits/mathdef.h /usr/include/bits/mathcalls.h
prtFuncs.o: ./calc.h /usr/include/math.h /usr/include/features.h
prtFuncs.o: /usr/include/sys/cdefs.h /usr/include/gnu/stubs.h
prtFuncs.o: /usr/include/bits/huge_val.h /usr/include/bits/mathdef.h
prtFuncs.o: /usr/include/bits/mathcalls.h ./emath/emath.h ./emath/Err.h
prtFuncs.o: ./units.h ./defns.h ./BitField.h /usr/include/stdio.h
prtFuncs.o: /usr/include/bits/types.h /usr/include/bits/wordsize.h
prtFuncs.o: /usr/include/bits/typesizes.h /usr/include/libio.h
prtFuncs.o: /usr/include/_G_config.h /usr/include/wchar.h
prtFuncs.o: /usr/include/bits/wchar.h /usr/include/gconv.h
prtFuncs.o: /usr/include/bits/stdio_lim.h /usr/include/bits/sys_errlist.h
prtFuncs.o: /usr/include/stdlib.h namespace.h
tables.o: ./tables.h /usr/include/values.h /usr/include/features.h
tables.o: /usr/include/sys/cdefs.h /usr/include/gnu/stubs.h
tables.o: /usr/include/limits.h /usr/include/math.h
tables.o: /usr/include/bits/huge_val.h /usr/include/bits/mathdef.h
tables.o: /usr/include/bits/mathcalls.h ./defns.h ./emath/emath.h
tables.o: ./emath/Err.h ./BitField.h /usr/include/stdio.h
tables.o: /usr/include/bits/types.h /usr/include/bits/wordsize.h
tables.o: /usr/include/bits/typesizes.h /usr/include/libio.h
tables.o: /usr/include/_G_config.h /usr/include/wchar.h
tables.o: /usr/include/bits/wchar.h /usr/include/gconv.h
tables.o: /usr/include/bits/stdio_lim.h /usr/include/bits/sys_errlist.h
tables.o: /usr/include/stdlib.h namespace.h ./calc.h ./units.h
tables.o: ./IDResource.h
vcode.o: /usr/include/stdio.h /usr/include/features.h
vcode.o: /usr/include/sys/cdefs.h /usr/include/gnu/stubs.h
vcode.o: /usr/include/bits/types.h /usr/include/bits/wordsize.h
vcode.o: /usr/include/bits/typesizes.h /usr/include/libio.h
vcode.o: /usr/include/_G_config.h /usr/include/wchar.h
vcode.o: /usr/include/bits/wchar.h /usr/include/gconv.h
vcode.o: /usr/include/bits/stdio_lim.h /usr/include/bits/sys_errlist.h
vcode.o: ./calc.h /usr/include/math.h /usr/include/bits/huge_val.h
vcode.o: /usr/include/bits/mathdef.h /usr/include/bits/mathcalls.h
vcode.o: ./emath/emath.h ./emath/Err.h ./units.h ./defns.h ./BitField.h
vcode.o: /usr/include/stdlib.h namespace.h ./tables.h
vcode.o: /usr/include/values.h /usr/include/limits.h ./AngFmt.h
vcode.o: AngFmt.cc /usr/include/string.h /usr/include/ctype.h
vcode.o: /usr/include/endian.h /usr/include/bits/endian.h ./ErrorObj.h
vcode.o: /usr/include/sys/time.h /usr/include/time.h
vcode.o: /usr/include/bits/time.h /usr/include/sys/select.h
vcode.o: /usr/include/bits/select.h /usr/include/bits/sigset.h
vcode.o: ./IDResource.h
AngFmt.o: ./emath/emath.h /usr/include/math.h /usr/include/features.h
AngFmt.o: /usr/include/sys/cdefs.h /usr/include/gnu/stubs.h
AngFmt.o: /usr/include/bits/huge_val.h /usr/include/bits/mathdef.h
AngFmt.o: /usr/include/bits/mathcalls.h ./emath/Err.h AngFmt.cc ./AngFmt.h
AngFmt.o: ./units.h /usr/include/string.h /usr/include/ctype.h
AngFmt.o: /usr/include/bits/types.h /usr/include/bits/wordsize.h
AngFmt.o: /usr/include/bits/typesizes.h /usr/include/endian.h
AngFmt.o: /usr/include/bits/endian.h
BitField.o: /usr/include/stdio.h /usr/include/features.h
BitField.o: /usr/include/sys/cdefs.h /usr/include/gnu/stubs.h
BitField.o: /usr/include/bits/types.h /usr/include/bits/wordsize.h
BitField.o: /usr/include/bits/typesizes.h /usr/include/libio.h
BitField.o: /usr/include/_G_config.h /usr/include/wchar.h
BitField.o: /usr/include/bits/wchar.h /usr/include/gconv.h
BitField.o: /usr/include/bits/stdio_lim.h /usr/include/bits/sys_errlist.h
BitField.o: /usr/include/stdlib.h namespace.h
calc.o: /usr/include/math.h /usr/include/features.h /usr/include/sys/cdefs.h
calc.o: /usr/include/gnu/stubs.h /usr/include/bits/huge_val.h
calc.o: /usr/include/bits/mathdef.h /usr/include/bits/mathcalls.h
calc.o: ./emath/emath.h ./emath/Err.h ./units.h ./defns.h ./BitField.h
calc.o: /usr/include/stdio.h /usr/include/bits/types.h
calc.o: /usr/include/bits/wordsize.h /usr/include/bits/typesizes.h
calc.o: /usr/include/libio.h /usr/include/_G_config.h /usr/include/wchar.h
calc.o: /usr/include/bits/wchar.h /usr/include/gconv.h
calc.o: /usr/include/bits/stdio_lim.h /usr/include/bits/sys_errlist.h
calc.o: /usr/include/stdlib.h namespace.h
calcinit.o: /usr/include/values.h /usr/include/features.h
calcinit.o: /usr/include/sys/cdefs.h /usr/include/gnu/stubs.h
calcinit.o: /usr/include/limits.h /usr/include/math.h
calcinit.o: /usr/include/bits/huge_val.h /usr/include/bits/mathdef.h
calcinit.o: /usr/include/bits/mathcalls.h ./calc.h ./emath/emath.h
calcinit.o: ./emath/Err.h ./units.h ./defns.h ./BitField.h
calcinit.o: /usr/include/stdio.h /usr/include/bits/types.h
calcinit.o: /usr/include/bits/wordsize.h /usr/include/bits/typesizes.h
calcinit.o: /usr/include/libio.h /usr/include/_G_config.h
calcinit.o: /usr/include/wchar.h /usr/include/bits/wchar.h
calcinit.o: /usr/include/gconv.h /usr/include/bits/stdio_lim.h
calcinit.o: /usr/include/bits/sys_errlist.h /usr/include/stdlib.h namespace.h
calcinit.o: ./tables.h ./IDResource.h
defns.o: ./emath/emath.h /usr/include/math.h /usr/include/features.h
defns.o: /usr/include/sys/cdefs.h /usr/include/gnu/stubs.h
defns.o: /usr/include/bits/huge_val.h /usr/include/bits/mathdef.h
defns.o: /usr/include/bits/mathcalls.h ./emath/Err.h ./BitField.h
defns.o: /usr/include/stdio.h /usr/include/bits/types.h
defns.o: /usr/include/bits/wordsize.h /usr/include/bits/typesizes.h
defns.o: /usr/include/libio.h /usr/include/_G_config.h /usr/include/wchar.h
defns.o: /usr/include/bits/wchar.h /usr/include/gconv.h
defns.o: /usr/include/bits/stdio_lim.h /usr/include/bits/sys_errlist.h
defns.o: /usr/include/stdlib.h namespace.h
ErrorObj.o: namespace.h
func.o: ./tables.h /usr/include/values.h /usr/include/features.h
func.o: /usr/include/sys/cdefs.h /usr/include/gnu/stubs.h
func.o: /usr/include/limits.h /usr/include/math.h
func.o: /usr/include/bits/huge_val.h /usr/include/bits/mathdef.h
func.o: /usr/include/bits/mathcalls.h ./defns.h ./emath/emath.h ./emath/Err.h
func.o: ./BitField.h /usr/include/stdio.h /usr/include/bits/types.h
func.o: /usr/include/bits/wordsize.h /usr/include/bits/typesizes.h
func.o: /usr/include/libio.h /usr/include/_G_config.h /usr/include/wchar.h
func.o: /usr/include/bits/wchar.h /usr/include/gconv.h
func.o: /usr/include/bits/stdio_lim.h /usr/include/bits/sys_errlist.h
func.o: /usr/include/stdlib.h namespace.h
IDResource.o: /usr/include/stdio.h /usr/include/features.h
IDResource.o: /usr/include/sys/cdefs.h /usr/include/gnu/stubs.h
IDResource.o: /usr/include/bits/types.h /usr/include/bits/wordsize.h
IDResource.o: /usr/include/bits/typesizes.h /usr/include/libio.h
IDResource.o: /usr/include/_G_config.h /usr/include/wchar.h
IDResource.o: /usr/include/bits/wchar.h /usr/include/gconv.h
IDResource.o: /usr/include/bits/stdio_lim.h /usr/include/bits/sys_errlist.h
IDResource.o: /usr/include/stdlib.h ./BitField.h namespace.h
tables.o: /usr/include/values.h /usr/include/features.h
tables.o: /usr/include/sys/cdefs.h /usr/include/gnu/stubs.h
tables.o: /usr/include/limits.h /usr/include/math.h
tables.o: /usr/include/bits/huge_val.h /usr/include/bits/mathdef.h
tables.o: /usr/include/bits/mathcalls.h ./defns.h ./emath/emath.h
tables.o: ./emath/Err.h ./BitField.h /usr/include/stdio.h
tables.o: /usr/include/bits/types.h /usr/include/bits/wordsize.h
tables.o: /usr/include/bits/typesizes.h /usr/include/libio.h
tables.o: /usr/include/_G_config.h /usr/include/wchar.h
tables.o: /usr/include/bits/wchar.h /usr/include/gconv.h
tables.o: /usr/include/bits/stdio_lim.h /usr/include/bits/sys_errlist.h
tables.o: /usr/include/stdlib.h namespace.h
fussy.o: /usr/include/stdio.h /usr/include/features.h
fussy.o: /usr/include/sys/cdefs.h /usr/include/gnu/stubs.h
fussy.o: /usr/include/bits/types.h /usr/include/bits/wordsize.h
fussy.o: /usr/include/bits/typesizes.h /usr/include/libio.h
fussy.o: /usr/include/_G_config.h /usr/include/wchar.h
fussy.o: /usr/include/bits/wchar.h /usr/include/gconv.h
fussy.o: /usr/include/bits/stdio_lim.h /usr/include/bits/sys_errlist.h
fussy.o: /usr/include/string.h ./calc.h /usr/include/math.h
fussy.o: /usr/include/bits/huge_val.h /usr/include/bits/mathdef.h
fussy.o: /usr/include/bits/mathcalls.h ./emath/emath.h ./emath/Err.h
fussy.o: ./units.h ./defns.h ./BitField.h /usr/include/stdlib.h namespace.h
fussy.o: /usr/include/signal.h /usr/include/bits/sigset.h
fussy.o: /usr/include/bits/signum.h /usr/include/readline/readline.h
fussy.o: /usr/include/readline/rlstdc.h /usr/include/readline/rltypedefs.h
fussy.o: /usr/include/readline/keymaps.h /usr/include/readline/chardefs.h
fussy.o: /usr/include/ctype.h /usr/include/endian.h
fussy.o: /usr/include/bits/endian.h /usr/include/readline/tilde.h
fussy.o: ./IDResource.h
fussy.o: ./calc.h /usr/include/math.h /usr/include/features.h
fussy.o: /usr/include/sys/cdefs.h /usr/include/gnu/stubs.h
fussy.o: /usr/include/bits/huge_val.h /usr/include/bits/mathdef.h
fussy.o: /usr/include/bits/mathcalls.h ./emath/emath.h ./emath/Err.h
fussy.o: ./units.h ./defns.h ./BitField.h /usr/include/stdio.h
fussy.o: /usr/include/bits/types.h /usr/include/bits/wordsize.h
fussy.o: /usr/include/bits/typesizes.h /usr/include/libio.h
fussy.o: /usr/include/_G_config.h /usr/include/wchar.h
fussy.o: /usr/include/bits/wchar.h /usr/include/gconv.h
fussy.o: /usr/include/bits/stdio_lim.h
fussy.o: /usr/include/bits/sys_errlist.h /usr/include/stdlib.h
fussy.o: namespace.h ./ErrorObj.h calc_lex_bison.h ./func.h
fussy.o: ./tables.h /usr/include/values.h /usr/include/limits.h
fussy.o: ./IDResource.h ./yyerror.c
yyerror.o: calc_lex_bison.h /usr/include/stdio.h /usr/include/features.h
yyerror.o: /usr/include/sys/cdefs.h /usr/include/gnu/stubs.h
yyerror.o: /usr/include/bits/types.h /usr/include/bits/wordsize.h
yyerror.o: /usr/include/bits/typesizes.h /usr/include/libio.h
yyerror.o: /usr/include/_G_config.h /usr/include/wchar.h
yyerror.o: /usr/include/bits/wchar.h /usr/include/gconv.h
yyerror.o: /usr/include/bits/stdio_lim.h /usr/include/bits/sys_errlist.h
