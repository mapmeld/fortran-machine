# makefile.tst --
#     Makefile for the test programs
#
#     For Compaq Visual Fortran we can use:
#     df x.f90 /link /exe:x.exe
#
#     -ox.exe is supported by g95 and gfortran
#
# For Compaq Visual Fortran:
#FC	=	df
#FCOPTS	=	
#EXEEXT	=	.exe
#OBJEXT	=	.obj
#OUTPUT	=	/link /exe:
#RM	=	del
#
#
#FC	=	g95
FC	=	gfortran
FCOPTS	=	
EXEEXT	=	.exe
OBJEXT	=	.o
OUTPUT	=	-o # Trick: add a trailing blank to the OUTPUT macro
RM	=	rm

all	:	test_file_list$(EXEEXT)

test_file_list$(EXEEXT)	:	filelist.f90
	$(FC) filelist.f90 $(OUTPUT)test_file_list$(EXEEXT)

clean:
	$(RM) test_file_list$(EXEEXT)
