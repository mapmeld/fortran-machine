@echo off
echo on
rem configure.bat --
if @%1 == @-help goto help
if @%1 == @/help goto help
if @%1 == @-?    goto help
if @%1 == @/?    goto help
goto analyse

:help
echo Configure Flibs: identify the compiler and set the build options
echo .
echo To pick a specific compiler, use:
echo c:\> configure name options
echo Available compilers:
echo - cvf     - Compaq Visual Fortran
echo - salford - Salford Fortran
echo - f95     - generic Fortran 95 compiler
echo .
echo Available build options:
echo -debug
echo -normal
echo -optimise
echo .
echo Run this batch file from the central directory!
pause
goto veryend

rem
rem Identify the build options ...
rem
:analyse
cd config
if errorlevel 1 goto error
if exist config.mk del config.mk
if exist options.mk del options.mk
copy normal.mk options.mk
if @%1 == @-debug copy debug.mk options.mk
if @%1 == @-optimise copy optimise.mk options.mk
if @%2 == @-debug copy debug.mk options.mk
if @%2 == @-optimise copy optimise.mk options.mk

echo Identifying compiler ... %1
if @%1 == @ goto start
if @%1 == @cvf goto cmp_cvf
if @%1 == @salford goto cmp_salford
if @%1 == @f95     goto cmp_generic
if @%2 == @cvf goto cmp_cvf
if @%2 == @salford goto cmp_salford
if @%2 == @salford goto cmp_generic

echo Unknown compiler: %1 - searching for known compilers instead

:start

rem -----------------------------------------------------------
rem Compaq Visual Fortran
rem -----------------------------------------------------------
rem
:cmp_cvf
df.exe /compile_only idc.f90
if errorlevel 1 goto after_df

echo Compiler: Compaq Visual Fortran
copy cvf.mk config.mk
goto end
:after_df

rem -----------------------------------------------------------
rem Salford Ftn95
rem -----------------------------------------------------------
rem
:cmp_salford
ftn95.exe idc.f90
if errorlevel 1 goto after_salf

echo Compiler: Salford Fortran
copy salford.mk config.mk
goto end
:after_salf

rem -----------------------------------------------------------
rem Generic Fortran 95 compiler
rem -----------------------------------------------------------
rem
:cmp_generic
f95.exe idc.f90
if errorlevel 1 goto after_generic

echo Compiler: Generic Fortran 95 compiler
copy f95.mk config.mk
goto end
:after_generic

rem
rem No suitable compiler found
rem
echo No suitable compiler could be identified
goto end

:error
echo Error:
echo Subdirectory "config" not found. Please start this batch file in
the echo central directory
goto veryend

:end
cd ..
:veryend
