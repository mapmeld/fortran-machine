# makefile --
#     Build the csvdata and csvtable programs
#
F95 = g95
FCOPTS = -c -I../../src/sqlite -ftrace=full -fno-second-underscore
EXEEXT = .exe
OBJEXT = .o
CC = gcc
CCOPTS = -c -I../../src/sqlite -I../../../sqlite -UWIN32

all:	csvdata$(EXEEXT) csvtable$(EXEEXT)

csvdata$(EXEEXT):	csvdata$(OBJEXT)
	$(F95) -o csvdata$(EXEEXT) csvdata$(OBJEXT)

csvdata$(OBJEXT):	csvdata.f90
	$(F95) $(FCOPTS) csvdata.f90

csvtable$(EXEEXT):	csvtable$(OBJEXT) fsqlite$(OBJEXT) csqlite$(OBJEXT)
	$(F95) -o csvtable$(EXEEXT) csvtable$(OBJEXT) fsqlite$(OBJEXT) csqlite$(OBJEXT) sqlite3.lib

csvtable$(OBJEXT):	csvtable.f90
	$(F95) $(FCOPTS) csvtable.f90

fsqlite$(OBJEXT):	../../src/sqlite/fsqlite.f90
	$(F95) $(FCOPTS) ../../src/sqlite/fsqlite.f90

csqlite$(OBJEXT):	../../src/sqlite/csqlite.c
	$(CC) $(CCOPTS) ../../src/sqlite/csqlite.c
