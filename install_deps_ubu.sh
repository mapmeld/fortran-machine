#!/bin/bash

set -o errexit

pkg_status()
{
  state=$(dpkg-query -W -f='${Status}' "$1" 2>/dev/null | awk '{ print $1 }')
  [ "$state" = install ] && return 0 || return 1
}

ensure_install()
{
    local pkg
    for pkg in "$@"
    do
        printf "Checking $pkg"
        if ! pkg_status $pkg >/dev/null
        then
            
            apt-get install -y $pkg
        fi
	echo
    done
}

apt-get update
ensure_install gfortran make sqlite3 libsqlite3-dev nginx libfcgi-dev spawn-fcgi
