# makefile --
#     Makefile for the test_report and test_logging programs
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
FCOPTS	=	-c
EXEEXT	=	.exe
OBJEXT	=	.o
MODEXT	=	.mod
OUTPUT	=	-o # Trick: add a trailing blank to the OUTPUT macro
RM	=	rm

all	:	test_logging$(EXEEXT)

test_logging$(EXEEXT)	:	test_logging.f90 m_logger$(OBJEXT)
	$(FC) test_logging.f90 m_logger$(OBJEXT) $(OUTPUT)test_logging$(EXEEXT)

m_logger$(OBJEXT)	:	../../src/reporting/m_logger.f90
	$(FC) $(FCOPTS) ../../src/reporting/m_logger.f90

clean:
	$(RM) test_logging$(EXEEXT)
	$(RM) m_logger$(OBJEXT)
	$(RM) m_logger$(MODEXT)
