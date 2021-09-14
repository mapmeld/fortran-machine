#!/bin/sh
#
# Restart fortran-machine

# Kill server, if running
pkill -f fortran_fcgi

# Recompile server
rm fortran_fcgi
make

# Respawn fcgi
spawn-fcgi -a 127.0.0.1 -p 9000 ./fortran_fcgi
