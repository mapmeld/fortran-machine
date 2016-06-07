# Makefile for "os_interact" test programs
#
# $Id: makefile,v 1.1 2008/08/12 13:26:02 arjenmarkus Exp $
#
include ../../config/config.mk
include ../../config/options.mk

PROGRAMS	=	dirsep$(EXEEXT) \
		stopcode$(EXEEXT) \
		reopen_stdin$(EXEEXT) \

all:	$(PROGRAMS)

dirsep$(OBJEXT):	dirsep.f90
	$(FC) $(FFLAGS) dirsep.f90

dirsep$(EXEEXT):	dirsep$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) dirsep$(OBJEXT)

stopcode$(OBJEXT):	stopcode.f90
	$(FC) $(FFLAGS) stopcode.f90

stopcode$(EXEEXT):	stopcode$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) stopcode$(OBJEXT)

reopen_stdin$(OBJEXT):	reopen_stdin.f90
	$(FC) $(FFLAGS) reopen_stdin.f90

reopen_stdin$(EXEEXT):	reopen_stdin$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) reopen_stdin$(OBJEXT)

clean:	
	$(DELETE) *$(OBJEXT)
	$(DELETE) $(PROGRAMS)
	$(DELETE) *$(MODEXT)

