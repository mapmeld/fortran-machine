@echo off
rem makedoc.tcl --
rem     Script for creating HTML-files from the raw documentation files
rem
c:\tcl\bin\tclsh.exe c:\tcl\demos\TclApps\apps\dtp\main.tcl doc -out %1.html html %1.man
copy /n /y %1.html ..\..\site
