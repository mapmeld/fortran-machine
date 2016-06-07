#!/bin/sh
#
# Small check: can a Fortran program return a value via the stop
# statement?
#
f95=g95

#$f95 -o stopcode stopcode.f90

./stopcode
rc=$?
if [ $rc = 1 ]; then
    echo 'Correct return code found (1)'
else
    echo "Return code was: $rc - should have been 1"
fi
