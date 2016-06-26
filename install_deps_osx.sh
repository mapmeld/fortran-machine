#!/bin/bash

set -o errexit

# Must have XCode Command Line Tools installed to use bottled gcc from homebrew
# If you don't, you'll end up doing a bootstrap build, which can take hours
install_gcc()
{
    # consult `brew config` for status of command line tools (CLT)
    CLT=`brew config | grep CLT | awk '{ print $2 }'`

    if [ "$CLT" = 'N/A' ]
    then
        xcode-select --install
    fi

    brew install gcc
}

install_pkg()
{
    local pkg=$1
    case $pkg in
        gcc)    install_gcc;;
        *)      brew install $pkg;;
    esac
}

ensure_install()
{
    local pkg
    for pkg in "$@"
    do
        brew list | grep ^$pkg$ >/dev/null || install_pkg $pkg
    done
}

ensure_install gcc sqlite nginx fcgi spawn-fcgi

