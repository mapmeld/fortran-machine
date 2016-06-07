@echo off
rem
rem Run a program under CGI: environment variables written to file
rem
rem This batch file is useful if the Fortran compiler does not
rem support the Fortran 2003 intrinsic GET_ENVIRONMENT_VARIABLE.
rem
rem NOTE:
rem You need to fill in the name of the program yourself

set > cgi.env
runprogram
