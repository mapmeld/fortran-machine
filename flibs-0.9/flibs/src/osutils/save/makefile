# makefile --
#     Part of the configuration procedure for OS utils
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

SYSTEM	=	_system_1$(EXEEXT) _system_2$(EXEEXT)

all	:	config_utils$(EXEEXT) $(SYSTEM) gather$(EXEEXT)

config_utils$(EXEEXT)	:	config_utils.f90
	$(FC) config_utils.f90 $(OUTPUT)config_utils$(EXEEXT)
	config_utils$(EXEEXT)
	#$(RM) gather.inp

gather$(EXEEXT)	:	gather.f90
	$(FC) -o gather$(EXEEXT) gather.f90
	gather$(EXEEXT)

#
# Executables for testing the system() routine
#
_system_1$(EXEEXT)	:	_system_1.f90
	$(FC) -o _system_1$(EXEEXT) _system_1.f90
	_system_1$(EXEEXT)

_system_2$(EXEEXT)	:	_system_2.f90
	$(FC) -o _system_2$(EXEEXT) _system_2.f90
	_system_2$(EXEEXT)


#
# Clean up
#
clean	:
	$(RM) _*
	$(RM) gather.inp
	$(RM) config_utils$(EXEEXT)
	$(RM) gather$(EXEEXT)
