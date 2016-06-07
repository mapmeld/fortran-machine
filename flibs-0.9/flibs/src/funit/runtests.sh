#!/usr/bin/sh
# runtests.sh --
#    Bourne shell script to control a program that uses funit
#    Name of the program: first argument
#
#    $Id: runtests.sh,v 1.2 2008/01/26 11:15:10 arjenmarkus Exp $
#
if test -f runtests.log ; then
    rm runtests.log
fi
echo ALL >ftnunit.run

chk=1
until test ! -f ftnunit.lst -a $chk -eq 0 ; do
    chk=0
    $1 $2 $3 $4 $5 $6 $7 $8 $9 >>runtests.log 2>&1
done

rm ftnunit.run
