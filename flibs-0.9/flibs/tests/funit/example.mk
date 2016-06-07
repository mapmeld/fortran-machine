# Makefile for example.tbl
#
include ../../config/config.mk
include ../../config/options.mk

PROGRAMS	=	example$(EXEEXT)

#
# Name of the source file containing the code to be tested
# (no extension!)
#
SOURCE	=	streeter

all:	$(PROGRAMS)

$(SOURCE)$(OBJEXT):	$(SOURCE).f90
	$(FC) $(FFLAGS) $(SOURCE).f90

example$(OBJEXT):	example.f90 $(SOURCE)$(OBJEXT)
	$(FC) $(FFLAGS) example.f90

example$(EXEEXT):	example$(OBJEXT) $(SOURCE)$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) example$(OBJEXT) $(SOURCE)$(OBJEXT)
