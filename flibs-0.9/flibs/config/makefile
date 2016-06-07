# Makefile for Ftcl-example "console"
#
# $Id: makefile,v 1.1 2006/08/13 07:04:55 arjenmarkus Exp $
#
include ../../config.mk

PROGRAM	=	interactive$(EXEEXT)
all:	$(PROGRAM)

interactive$(OBJEXT):	interactive.f90
	$(FC) $(FFLAGS) interactive.f90

$(PROGRAM):	interactive$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) interactive$(OBJEXT) $(LIBS)

clean:	
	(DELETE) *$(OBJEXT)
	(DELETE) *$(EXEEXT)
	(DELETE) *$(MODEXT)

