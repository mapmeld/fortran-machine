# Makefile for "controlstructures" tests and examples
#
# $Id: makefile,v 1.2 2008/04/21 17:45:49 arjenmarkus Exp $
#
include ../../config/config.mk
include ../../config/options.mk

INCLUDE	=	-I

PROGRAMS	=	tst_messages$(EXEEXT) \
           tst_finite_state$(EXEEXT) \

all:	$(PROGRAMS)

tst_messages$(OBJEXT):	tst_messages.f90 ../../src/controlstructures/messages.f90
	$(FC) $(FFLAGS) tst_messages.f90 $(INCLUDE)../../src/controlstructures

tst_messages$(EXEEXT):	tst_messages$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) tst_messages$(OBJEXT)

tst_finite_state$(OBJEXT):	tst_finite_state.f90 ../../src/controlstructures/finite_state.f90
	$(FC) $(FFLAGS) tst_finite_state.f90 $(INCLUDE)../../src/controlstructures

tst_finite_state$(EXEEXT):	tst_finite_state$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) tst_finite_state$(OBJEXT)

clean:	
	$(DELETE) *$(OBJEXT)
	$(DELETE) $(PROGRAMS)
	$(DELETE) *$(MODEXT)

