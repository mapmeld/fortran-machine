# Makefile for "fsqlite" library
#
# NOTE:
# You need to adjust the macro DEFINES in this makefile:
# - To select the calling conventions for Fortran and C
# - To set the directory where the Sqlite3 header file can be found
#
# The calling conventions for the Fortran compiler need to
# match those for the C compiler. To this end the following
# macros have been defined in the source code:
#
# - LOWERCASE:
#   Define this macro if the Fortran compiler translates the names to
#   lower case and adds a single underscore
#   E.g.
#   A subroutine SQLITE3_OPEN in the Fortran code becomes sqlite3_open_
#   in the object file.
#
# - DBL_UNDERSCORE:
#   Define this macro if the Fortran compiler translates the names to
#   lower case but adds two underscores if the name itself contains
#   an underscore.
#   E.g.
#   A subroutine SQLITE3_OPEN in the Fortran code becomes sqlite3_open__
#   in the object file.
#
# - WIN32:
#   Under Windows the decoration is often more complicated. Most
#   compilers under Windows define this macro automatically, so it is
#   used to identify this type of decoration, but there can be
#   confusion: under Cygwin and MingW this macro is also defined, but
#   the decorations are not necessarily those of "__stdcall".
#   (Using either of the two others overrides this).
#
# Note:
# Things are further complicated, under Windows at least, if you
# want to create a DLL. The C and Fortran source code as given here
# is not enough then (you need to export routines explicitly from the
# DLL).
#
# $Id: makefile,v 1.1 2008/05/04 13:23:56 arjenmarkus Exp $
#
include ../../config/config.mk
include ../../config/options.mk

LIBRARIES	=	libfsqlite.a \

INC_SQLITE3	=	$(shell dirname `find /usr -name sqlite3.h -print -quit`)

#
# Use the correct macro for interfacing between Fortran and C and
# add the directory where the Sqlite3 header file can be found
#
#DEFINES	=	-DDBL_UNDERSCORE -I$(INC_SQLITE3)
DEFINES		=	-DLOWERCASE -I$(INC_SQLITE3)
#DEFINES	=	-I$(INC_SQLITE3)


all:	$(LIBRARIES)

libfsqlite.a:	fsqlite.o csqlite.o
	ar r libfsqlite.a fsqlite.o csqlite.o

fsqlite.o:	fsqlite.f90
	$(FC) -c $(FFLAGS) fsqlite.f90

csqlite.o:	csqlite.c
	$(CC) -c $(CFLAGS) csqlite.c $(DEFINES)

clean:	
	$(DELETE) *$(OBJEXT)
	$(DELETE) $(PROGRAMS)
	$(DELETE) *$(MODEXT)

