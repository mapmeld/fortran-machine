@echo off
rem
rem This does not work, unfortunately!
rem Redirection fails
start "debug\ipc_file.exe < file1.inp"
debug\ipc_file.exe < file2.inp
