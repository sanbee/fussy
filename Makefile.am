bin_PROGRAMS = fussy

noinst_LIBRARIES = libkum.a libcalc.a
libkum_a_SOURCES = src/sla/djm2e.f src/sla/ep2da.f src/sla/frye.f \
				   src/sla/slacaldj.f src/sla/slacldj.f src/sla/sladcc2s.f \
				   src/sla/sladcs2c.f src/sla/sladjcl.f src/sla/sladmxv.f \
				   src/sla/sladranrm.f src/sla/slagmst.f src/sla/slaprebn.f \
				   src/sla/slapreces.f src/sla/slaprec.f

libcalc_a_SOURCES = src/y.tab.cc src/calcinit.cc src/tables.cc src/vcode.cc \
				    src/prtFuncs.cc src/ErrorObj.cc src/func.cc src/BitField.cc \
					src/IDResource.cc src/ExitMsg.cc

libcalc_a_CXXFLAGS = $(AM_CXXFLAGS) -Isrc -Isrc/emath
libcalc_a_CFLAGS = $(AM_CFLAGS) -Isrc -Isrc/emath

fussy_CXXFLAGS = $(AM_CXXFLAGS) -Isrc -Isrc/emath
fussy_CFLAGS = $(AM_CFLAGS) -Isrc -Isrc/emath
fussy_SOURCES = src/fussy.cc

fussy_LDADD = libkum.a libcalc.a
