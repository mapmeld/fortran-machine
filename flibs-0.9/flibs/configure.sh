#!/usr/bin/sh
#    Configuration for Flibs: find a suitable compiler and set build options
#
if test @$1 = "@-help" -o @$1 = "@-?" ;then
    echo Configure Flibs: identify the compiler and set the build options
    echo .
    echo To pick a specific compiler, use:
    echo % configure name options
    echo Available compilers:
    echo - gfortran - Gnu\'s Fortran 95 compiler
    echo - g95      - Alternative Fortran 95 compiler
    echo - f95      - generic Fortran 95 compiler
    echo ""
    echo Available build options:
    echo -debug
    echo -normal
    echo -optimise
    echo ""
    echo Run this batch file from the central directory!
    exit
fi

cd config

#
# Clean up
#
rm -f config.mk
rm -f options.mk

#
# Build options
#
options=normal

if test @$1 == "@-normal" -o @$2 == "@-normal" ;then
    options=normal
fi
if test @$1 == "@-debug" -o @$2 == "@-debug" ;then
    options=debug
fi
if test @$1 == "@-optimise" -o @$2 == "@-optimise" ;then
    options=optimise
fi

cp $options.mk options.mk

#
# Identify the compiler
#
check=""

if test @$1 == "@gfortran" -o @$2 == "@gfortran" ;then
    check=gfortran
fi
if test @$1 == "@g95" -o @$2 == "@g95" ;then
    check=g95
fi
if test @$1 == "@f95" -o @$2 == "@f95" ;then
    check=f95
fi

found=0

# -------------------------------------------------------------------------
# Compiler: gfortran
# -------------------------------------------------------------------------
if test $found -eq 0 -a \( "$check" == "" -o "$check" == "gfortran" \) ;then
    echo Checking gfortran ...
    gfortran idc.f90
    if test $? -eq 0 ;then
        found=1
        compiler=gfortran
    fi
fi

# -------------------------------------------------------------------------
# Compiler: g95
# -------------------------------------------------------------------------
if test $found -eq 0 -a \( "$check" == "" -o "$check" == "g95" \) ;then
    echo Checking g95 ...
    g95 idc.f90
    if test $? -eq 0 ;then
        found=1
        compiler=g95
    fi
fi

# -------------------------------------------------------------------------
# Compiler: f95
# -------------------------------------------------------------------------
if test $found -eq 0 -a \( "$check" == "" -o "$check" == "f95" \) ;then
    echo Checking f95 ...
    f95 idc.f90
    if test $? -eq 0 ;then
        found=1
        compiler=f95
    fi
fi

#
# Now that we have found a compiler, copy the settings
#
if test $found -eq 1 ;then
    echo Compiler: $compiler
    cp $compiler.mk config.mk
else
    echo No suitable compiler found!
fi

cd ..
exit
