# Options for gfortran
#
# Maybe define LDOUTPUT as LDOUTPUT=-o $@
#
SPACE	=	\

SEP	=	/

FC	=	gfortran
FFLAGS_NORMAL	=	-c
FFLAGS_DEBUG	=	-c -g
FFLAGS_OPTIMISE	=	-c -O

LD	=	gfortran
LDFLAGS_NORMAL	=	
LDFLAGS_DEBUG	=	-g
LDFLAGS_OPTIMISE	=	
#LDOUTPUT	=	-o$(SPACE)
LDOUTPUT	=	-o $@

LIB	=	ar r

OBJEXT	=	.o
EXEEXT	=	
MODEXT	=	.mod

DELETE	=	rm -f
