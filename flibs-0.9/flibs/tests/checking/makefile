# Makefile for "checking" tests and examples
#
# $Id: makefile,v 1.1 2007/08/12 17:10:51 arjenmarkus Exp $
#
include ../../config/config.mk
include ../../config/options.mk

PROGRAMS	=	test_w$(EXEEXT) \

all:	$(PROGRAMS)

test_w$(OBJEXT):	test_w.f90
	$(FC) $(FFLAGS) test_w.f90

test_w.f90:	../../src/checking/test_w.f90
	rm -f test_w.f90.org
	cp ../../src/checking/test_w.f90 .
	tclsh ../../src/checking/instrument.tcl

w_$(OBJEXT):	../../src/checking/w_.f90
	$(FC) $(FFLAGS) ../../src/checking/w_.f90

test_w$(EXEEXT):	test_w$(OBJEXT) w_$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_w$(OBJEXT) w_$(OBJEXT)

clean:	
	$(DELETE) *$(OBJEXT)
	$(DELETE) $(PROGRAMS)
	$(DELETE) *$(MODEXT)

