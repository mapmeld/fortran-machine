# Options for g95
#
# Maybe define LDOUTPUT as LDOUTPUT=-o $@
#
SPACE	=	\

SEP	=	/

FC	=	g95
FFLAGS_NORMAL	=	-c
FFLAGS_DEBUG	=	-c -g
FFLAGS_OPTIMISE	=	-c -O

LD	=	g95
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
