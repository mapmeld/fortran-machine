#!/bin/bash

set -o errexit

pacman -Sy
pacman -S --needed gcc-fortran make sqlite  nginx fcgi spawn-fcgi
