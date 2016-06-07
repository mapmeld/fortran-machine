# Makefile for "computing" tests and examples
#
# $Id: makefile,v 1.1 2008/11/14 18:27:37 arjenmarkus Exp $
#
include ../../config/config.mk
include ../../config/options.mk

PROGRAMS	=	test_cgi$(EXEEXT) \

all:	$(PROGRAMS)

test_cgi$(OBJEXT):	test_cgi.f90 cgi_protocol$(OBJEXT)
	$(FC) $(FFLAGS) test_cgi.f90 -ftrace=full

cgi_protocol$(OBJEXT):	../../src/cgi/cgi_protocol.f90
	$(FC) $(FFLAGS) ../../src/cgi/cgi_protocol.f90 -I../../src/cgi

test_cgi$(EXEEXT):	test_cgi$(OBJEXT) cgi_protocol$(OBJEXT)
	$(LD) $(LDLAGS) $(LDOUTPUT) test_cgi$(OBJEXT) cgi_protocol$(OBJEXT)

clean:	
	$(DELETE) *$(OBJEXT)
	$(DELETE) $(PROGRAMS)
	$(DELETE) *$(MODEXT)

