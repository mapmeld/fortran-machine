@echo off
rem Small check: can a Fortran program return a value via the stop
rem statement?

set f95=g95

rem %f95% -o stopcode stopcode.f90

.\stopcode
if errorlevel 2 goto toohigh
if errorlevel 1 goto correct

echo "Return code was: 0 - should have been 1"
goto end

:correct
echo Correct return code found (1)
goto end

:toohigh
echo Return code was too high - should have been 1

:end
