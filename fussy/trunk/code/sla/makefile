#-*-makefile-*-
#include $(GSRCROOT)/Setup/Setup.$(GOSDIR)
#include $(GSRCROOT)/Setup/Setup.$(GOSDIR).personal
FC=gfortran
FFLAGS=-O3
#
OBJS = djm2e.o     \
	ep2da.o     \
	frye.o      \
	slacaldj.o  \
	slacldj.o   \
	sladcc2s.o  \
	sladcs2c.o  \
	sladjcl.o   \
	sladmxv.o   \
	sladranrm.o \
	slagmst.o   \
	slaprebn.o  \
	slaprec.o   \
	slapreces.o

all: libkum.a

libkum.a: $(OBJS)
	ar r libkum.a *.o

install: all
	mv libkum.a $(GLIB)

#.o.f:
#	g77 -c $*.f

clean: 
	rm -f *.o *~ core *.a
