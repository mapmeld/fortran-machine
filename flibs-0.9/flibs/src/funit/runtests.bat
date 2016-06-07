@echo off
rem runtests.bat --
rem     DOS batch file to control a program that uses ftnunit
rem     Name of the program: first argument
rem
rem     $Id: runtests.bat,v 1.2 2008/01/26 11:15:10 arjenmarkus Exp $
rem
if exist runtests.log del runtests.log
echo ALL >ftnunit.run

:run
%1 %2 %3 %4 %5 %6 %7 %8 %9 >runtests.out 2>runtests.err
type runtests.out >>runtests.log
type runtests.err >>runtests.log
if exist ftnunit.lst goto run

del ftnunit.run
del runtests.out
del runtests.err
