# makefile --
#     Build the test_ftnunit program
#
F95 = g95
FCOPTS = -c -I../../src/funit -ftrace=full
EXEEXT = .exe
OBJEXT = .o

all:	test_ftnunit$(EXEEXT)

test_ftnunit$(EXEEXT):	ftnunit$(OBJEXT) test_ftnunit$(OBJEXT) 
	$(F95) -o test_ftnunit$(EXEEXT) test_ftnunit$(OBJEXT) ftnunit$(OBJEXT)

test_ftnunit$(OBJEXT):	test_ftnunit.f90 
	$(F95) $(FCOPTS) test_ftnunit.f90

ftnunit$(OBJEXT):	../../src/funit/ftnunit.f90
	$(F95) $(FCOPTS) ../../src/funit/ftnunit.f90
