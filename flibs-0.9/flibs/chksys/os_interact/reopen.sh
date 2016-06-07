#!/bin/sh
# Can a Fortran program reopen standard input?

#f95=gfortran
f95=g95

$f95 -o reopen_stdin reopen_stdin.f90

./reopen_stdin < reopen.inp
