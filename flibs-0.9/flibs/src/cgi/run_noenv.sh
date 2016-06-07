#!/usr/bin/sh
#
# Run a program under CGI: environment variables written to file
#
# This shell script is useful if the Fortran compiler does not
# support the Fortran 2003 intrinsic GET_ENVIRONMENT_VARIABLE.
#
# NOTE:
# You need to fill in the name of the program yourself

env > cgi.env
runprogram
