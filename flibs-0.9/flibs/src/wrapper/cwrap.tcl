# cwrap.tcl --
#     Program to generate a set of wrapper functions from C header files
#     so that the functions can be used in a Fortran program
#
#     TODO:
#     typedef, enum, ISO-C, c_ptr
#     prototypes without argument names: int f(int,int);
#

# global variables --
#
set typemap {}   ;# Accumulates translation from user-defined types to basic types

set convert {"unsigned char" "char"      "signed char"            "char"
             "unsigned int"  "int"
             "short int"     "short"     "unsigned short int"     "short"
             "long int"      "long"      "unsigned long int"      "long"
             "long long int" "longlong"  "unsigned long long int" "longlong"
            }

set ignored {WINGDIAPI  "" APIENTRY "" "CONST " "" _cdecl ""
             "CONST84 " "" "CONST84_RETURN " ""} ;# List of ignored keywords (CONST, _cdecl, ...)

# ftype --
#     Translation of C types to corresponding Fortran types
#
array set ftype {short    "integer(c_short)"
                 short*   "integer(c_short), dimension(*)"
                 int      "integer(c_int)"
                 int*     "integer(c_int), dimension(*)"
                 long     "integer(c_long)"
                 long*    "integer(c_long), dimension(*)"
                 __int64  "integer(c_longlong)"
                 __int64* "integer(c_longlong), dimension(*)"
                 longlong "integer(c_longlong)"
                 longlong "integer(c_longlong), dimension(*)"
                 float    "real(c_float)"
                 float*   "real(c_float), dimension(*)"
                 double   "real(c_double)"
                 double*  "real(c_double), dimension(*)"
                 char     "character(len=*)"
                 char*    "character(len=*)"
                 char**   "type(c_ptr)"
                 void*    "type(c_ptr)"
                 void**   "type(c_ptr)"}

array set isotype {short     "integer(c_short), intent(in), value"
                   short*    "integer(c_short), dimension(*), intent(inout)"
                   int       "integer(c_int), intent(in), value"
                   int*      "integer(c_int), dimension(*), intent(inout)"
                   long      "integer(c_long), intent(in), value"
                   long*     "integer(c_long), dimension(*), intent(inout)"
                   longlong  "integer(c_longlong), intent(in)"
                   longlong* "integer(c_longlong), dimension(*), intent(inout)"
                   float     "real(c_float), intent(in), value"
                   float*    "real(c_float), dimension(*), intent(inout)"
                   double    "real(c_double), intent(in), value"
                   double*   "real(c_double), dimension(*), intent(inout)"
                   char      "character(len=*), intent(in), value"
                   char*     "character(len=*), intent(inout)"
                   char**    "type(c_ptr), intent(inout)"
                   void*     "type(c_ptr), intent(in)"
                   void**    "type(c_ptr), intent(inout)"}

# cwrap --
#     Generate the actual C code and the Fortran interface (if possible)
#
# Arguments:
#     type        Return type of the function
#     name        Name of the function
#     arglist     List of arguments (type and name)
#     args        All other arguments (mainly a consequence of the transformation)
#
# Result:
#     None
#
# Note:
#     Unknown types cause the procedure to write an error message
#     C functions whose interface is ambiguous are left out of the
#     Fortran interface module
#
proc cwrap {type name args} {
    global cout
    global ftnout
    global isoout
    global typout
    global error
    global ftype

    set error ""
    set fname [string tolower "${name}_"]

    #
    # Function interface
    #
    if { [lindex $args 0] ne "--" } {
        set arglist [lindex $args 0]
        set args    [lrange $args 1 end]

        puts "Cwrap: $name"
        puts "Cwrap: $arglist"

        set ftnargs [transformArgList $arglist]
        set body    [setUpBody $type $name $arglist]

        puts $cout "
#ifdef FTN_ALLCAPS
#define $fname [string toupper $name]
#endif

$type STDCALL $fname ( \n    [join $ftnargs ,\n\ \ \ \ ] ) {
$body
}"

        if { $error != "" } {
            puts "Function/routine: $name"
            puts "$error"
        }

        set interface [setUpInterface $type [string tolower $name] $arglist]
        puts $ftnout "\n$interface"

        set interface [setUpIsoCInterface $type [string tolower $name] $name $arglist]
        puts $isoout "\n$interface"

    } else {
        #
        # Single variable or field in a structure
        #
        puts $typout "    $ftype($type) :: $name"
    }
}


# transformToTcl --
#     Transform the C code to a set of Tcl commands for easy processing
#
# Arguments:
#     code          Contents of the C header file
#
# Result:
#     Tcl code that can be evaluated directly
#
# Note:
#     There are rather subtle interactions going on with the
#     substitutions
#
proc transformToTcl {code} {

    set code "\n[removeExternC $code]"            ;# start of all lines easier to detect

    regsub -all {/\*.*?\*/} $code "" code           ;# remove comments
    regsub -all {//[^\n]*\n} $code "\n" code        ;# remove C++ comments
    regsub -all {\\ *\n} $code { } code             ;# continuation of macros
    regsub -all {\n[ \t]*#[ \t]+} $code "\n#" code  ;# normalise #keyword
    regsub -all {#([^\n]+)\n} $code ";#\\1;" code   ;# insert semicolon before and after #keyword
    regsub -all {\n} $code " " code                 ;# commands always end with semicolon
    regsub -all {;} $code "\n" code                 ;# break lines at semicolons
    regsub -all {\{} $code " \{ " code              ;# proper lists
    regsub -all {\}} $code " \} " code              ;# proper lists

    regsub -all {defined *\(([a-zA-Z_0-9]+) *\)} $code "\[defined \\1\]" code
    regsub -all {#if([^dn][^\n]+)} $code "IF \{ \\1 \} \{" code
    regsub -all {#elif([^dn][^\n]+)} $code "\} elseif \{ \\1 \} \{" code

#
#   set code [string map {(         " \{"
#                         )         "\} \\ "
#                         \}        "\} "
#                         \{        " \{"
#                         "/*"      ";comment \{"
#                         "*/"      "\}\n"
#                         "const "  " "
#                         "#else"  "\} else \{"
#                         "#endif"  "\}\n"       } $code]
    regsub -all {#ifdef +([a-zA-Z_0-9]+)} $code "IF \{ \[defined \\1\] \} \{ " code
    regsub -all {#ifndef +([a-zA-Z_0-9]+)} $code "IF \{ ! \[defined \\1\] \} \{ " code
    regsub -all {#define ([^\n]+)\n} $code "define \\1\n\n" code
    regsub -all {#undef ([^\n]+)\n} $code "undef \\1\n\n" code
    regsub -all {#else} $code "\} else \{\n" code
    regsub -all {#endif} $code "\}\n" code
    regsub -all {#include} $code "include" code
    regsub -all { *\*} $code "* " code
    regsub -all {\* +\*} $code "**" code
    regsub -all {\(} $code " \{ " code
    regsub -all {\)} $code " \} " code

    puts ">>>$code<<<"
    return $code
}


# removeExternC --
#     Remove the "#ifdef __cplusplus extern "C" #endif" sequence and its
#     counterpart
#
# Arguments:
#     code          Contents of the C header file
#
# Result:
#     Slightly altered code
#
proc removeExternC {code} {

    regsub -all {#ifdef +__cplusplus[ \n]+extern [^#]*#endif} $code {} code
    regsub -all {#ifdef +__cplusplus[ \n]+\}[; \n]*#endif} $code {} code
    regsub -all {#if +defined *\(__cplusplus\)[ \n]+extern [^#]*#endif} $code {} code
    regsub -all {#if +defined *\(__cplusplus\)[ \n]+\}[; \n]*#endif} $code {} code
    return $code
}


# transformArgList --
#     Transform the C argument list for the wrapper
#
# Arguments:
#     arglist       String containing the types and names
#
# Result:
#     Argument list for the wrapper
#
proc transformArgList {arglist} {
    global error
    global typemap
    global ftype

    set wraplist {}
    set end      {}
    set carg     0
    foreach arg [split $arglist ,] {
        set arg  [string map $typemap $arg]
        set name [lindex $arg end]
        set type [lindex $arg end-1]

        if { [llength $arg] == 1 } {
            incr carg
            set name "arg__$carg"
            set type [lindex $arg 0]
        }

        switch -glob -- $type {
            "short"      -
            "int"        -
            "long"       -
            "longlong"   -
            "__int64"    -
            "float"      -
            "double" {
                lappend wraplist "$type* $name"
                if { [string match "*\[\]" $name } {
                    set name [string range $name 0 end-2]
                    set type "$type\[\]"
                }
            }
            "short\*"    -
            "int\*"      -
            "long\*"     -
            "longlong\*" -
            "__int64\*"  -
            "float\*"    -
            "double\*"   -
            "char\*\*"   -
            "void\*"     -
            "void\*\*"   {
                lappend wraplist "$type $name"
            }
            "char"       -
            "char\*"     {
                lappend wraplist "$type $name"
                lappend end      "int len__$name"
            }
            ""        -
            "void"    {
                # Nothing
            }
            default {
                if { [info exists ftype($type)] } {
                    lappend wraplist "$type $name"
                } else {
                    append error "\n    $arg: conversion to/from Fortran not supported"
                }
            }
        }

    }

    return [concat $wraplist $end]
}


# setUpBody --
#     Construct the body of the wrapper
#
# Arguments:
#     type          Type of value to be returned
#     name          Name of the original function
#     arglist       String containing the types and names
#
# Result:
#     Body for the wrapper
#
proc setUpBody {type name arglist} {
    global error
    global typemap

    set type [string map $typemap $type]

    if { $type != "void" } {
        set body   "    $type result__;\n"
        set call   "    result__ = $name ("
        set return "    return result__;"
    } else {
        set body   ""
        set call   "    $name ("
        set return "    return;"
    }
    set wraplist  {}
    set localvars {}
    set prologue  {}
    set epilogue  {}
    set carg      0
    foreach arg [split $arglist ,] {
        set arg  [string map $typemap $arg]
        set name [lindex $arg end]
        set type [lindex $arg end-1]

        if { [llength $arg] == 1 } {
            incr carg
            set name "arg__$carg"
            set type [lindex $arg 0]
        }

        switch -glob -- $type {
            "char"      -
            "short"     -
            "int"       -
            "long"      -
            "longlong"  -
            "__int64"   -
            "float"     -
            "double"    {
                lappend wraplist "*$name"
            }
            "char\\*"   {
                lappend localvars "    fortran_string fort__$name;"
                lappend prologue  "    ftoc_string( &fort__$name, $name, len__$name );"
                lappend epilogue  "    ctof_string( &fort__$name, $name, len__$name );"
                lappend wraplist "fort__$name.pstr"
            }
            "int\\*"     -
            "long\\*"    -
            "float\\*"   -
            "double\\*"  -
            "char\\*\\*" -
            "void\\*"    -
            "void\\*\\*" {
                lappend wraplist "$name"
            }
            "void"    {
                # Nothing
            }
            default {
                # Nothing!
            }
        }

        set body "[join $localvars \n]
[join $prologue \n]
$call [join $wraplist ,\ ] );
[join $epilogue \n]
$return"
    }

    return $body
}


# setUpInterface --
#     Construct the Fortran 90/95 interface
#
# Arguments:
#     type          Type of value to be returned
#     fname         Name as known to Fortran
#     arglist       String containing the types and names
#
# Result:
#     Body for the wrapper
#
proc setUpInterface {type fname arglist} {
    global error
    global ftype
    global typemap

    set type [string map $typemap $type]

    if { $type != "void" } {
        set body   "    $ftype($type) function $fname ("
        set end    "    end function $fname"
    } else {
        set body   "    subroutine $fname ("
        set end    "    end subroutine $fname"
    }
    set wraplist  {}
    set ftnargs   {}
    set ambiguous 0
    set carg      0
    foreach arg [split $arglist ,] {
        set arg  [string map $typemap $arg]
        set name [lindex $arg end]
        set type [lindex $arg end-1]

        if { [llength $arg] == 1 } {
            incr carg
            set name "arg__$carg"
            set type [lindex $arg 0]
        }

        switch -glob -- $type {
            "char"       -  "char\[\]"       -
            "char\\*"    -
            "short"      -  "short\[\]"      -
            "int"        -  "int\[\]"        -
            "long"       -  "long\[\]"       -
            "longlong"   -  "longlong\[\]"   -
            "__int64"    -  "__int64\[\]"    -
            "float"      -  "float\[\]"      -
            "double"     -  "double\[\]"     -
            "char\\*\\*" -
            "void\\*"    -
            "void\\*\\*" {
                lappend wraplist "$ftype($type) :: $name"
                lappend ftnargs  "$name"
            }
            "short\\*"    -
            "int\\*"      -
            "long\\*"     -
            "longlong\\*" -
            "__int64\\*"  -
            "float\\*"    -
            "double\\*"   {
                set ambiguous 1
                lappend wraplist "$ftype($type) :: $name"
                lappend ftnargs  "$name"
            }
            "void" {
                # Nothing
            }
            default {
                if { [info exists ftype($type)] } {
                    lappend wraplist [string map [list @ $name] $ftype($type)]
                    lappend ftnargs  "$name"
                }
            }
        }

    }

    if { $ambiguous } {
        set body "    ! Ambiguous interface: scalars or arrays?\n$body"
    }
    set body "$body [join $ftnargs ,\ ] )\n        [join $wraplist \n\ \ \ \ \ \ \ \ ]\n$end"

    return $body
}


# setUpIsoCInterface --
#     Construct the Fortran 2003 interface, using the standard ISO C bindings
#
# Arguments:
#     type          Type of value to be returned
#     fname         Name as known to Fortran
#     cname         Original C name
#     arglist       String containing the types and names
#
# Result:
#     Body for the wrapper
#
proc setUpIsoCInterface {type fname cname arglist} {
    global error
    global isotype
    global typemap

    set type [string map $typemap $type]

    if { $type != "void" } {
        set body   "    function $fname ("
        set result "result(result__)
        use iso_c_binding
        [lindex [split $isotype($type) ,] 0] :: result__"
        set end    "    end function $fname"
    } else {
        set body   "    subroutine $fname ("
        set result "\n        use iso_c_binding"
        set end    "    end subroutine $fname"
    }
    set wraplist  {}
    set ftnargs   {}
    set ambiguous 0
    set carg      0
    foreach arg [split $arglist ,] {
        set arg  [string map $typemap $arg]
        set name [lindex $arg end]
        set type [lindex $arg end-1]

        if { [llength $arg] == 1 } {
            incr carg
            set name "arg__$carg"
            set type [lindex $arg 0]
        }

        switch -glob -- $type {
            "char"     -
            "char\\*"  -
            "short"    -
            "int"      -
            "long"     -
            "longlong" -
            "__int64"  -
            "float"    -
            "double"   -
            "void\\*"  {
                lappend wraplist "$isotype($type) :: $name"
                lappend ftnargs  "$name"
            }
            "short\\*"     -
            "int\\*"       -
            "long\\*"      -
            "longlong\\*"  -
            "__int64\\*"   -
            "float\\*"     -
            "double\\*"    {
                set ambiguous 1
                lappend wraplist "$isotype($type) :: $name"
                lappend ftnargs  "$name"
            }
            "void" {
            }
            default {
                if { [info exists isotype($type)] } {
                    lappend wraplist [string map [list @ $name] $isotype($type)]
                    lappend ftnargs  "$name"
                }
            }
        }

    }

    if { $ambiguous } {
        set body "    ! Ambiguous interface: scalars or arrays?\n$body"
    }
    set body "$body [join $ftnargs ,\ ] ) &
        ,bind(C,name=\"$cname\") $result
        [join $wraplist \n\ \ \ \ \ \ \ \ ]\n$end"

    return $body
}


# prologue --
#     Write the prologue code for the wrapper
#
# Arguments:
#     module        Name of the module
#     names         Names of the header files
#
# Result:
#     None
#
proc prologue {module names} {
    global cout
    global ftnout
    global isoout

    puts $cout \
"/* Wrapper derived from the header file(s) $names
*/"

    foreach name $names {
        puts $cout "#include \"$name\""
    }

    puts $cout \
"#ifdef WIN32
#define STDCALL stdcall__
#else
#define STDCALL
#endif
#include \"cfstring.h\"
"

    foreach out [list $ftnout $isoout] {
        puts $out \
"! Interfaces for wrapper routines (derived from the header file(s) $names)
!
module $module
    use iso_c_binding
    implicit none

interface"
    }

}


# epilogue --
#     Write the epilogue code for the wrapper
#
# Arguments:
#     None
#
# Result:
#     None
#
proc epilogue {module} {
    global cout
    global ftnout
    global isoout
    global declout

    close $declout
    set infile [open "${module}_decl.f90" r]
    set contents [read $infile]
    close $infile

    file delete "${module}_decl.f90"

    foreach out [list $ftnout $isoout] {
        puts $out \
"end interface

!
! Parameters (macros) from the C header file(s) - if any
!
$contents

end module"
    }
}


# IF --
#     Wrapper around "if" to take care of preprocessor syntax
#
# Arguments:
#     args          Conditions etc. for the #if directive
#
# Result:
#     None
#
# Note:
#     Possible patterns are: if - condition - body - else - body
#     or: if - condition - body - elseif - condition - body ...
#
proc IF {args} {
    set cmd  if
    set cond 1
    foreach string $args {
        if { $cond } {
            set string [string map {\{ ( \} )} $string]
            regsub -all {([(&|! \t]+)([A-Za-z_][A-Za-z_0-9]*)([)&| =><!\t])} $string {\1$::macros(\2)\3} string
            lappend cmd $string
            set cond 0
        } else {
            lappend cmd $string
            if { $string eq "elseif" } {
                set cond 1
            }
        }
    }
    eval $cmd
}


# include --
#     Handle the #include directive
#
# Arguments:
#     header        Name of the header file
#
# Result:
#     None
#
proc include {header} {
    global include_dirs
    #
    # Identify the location of the header file
    #
    if { [string first "<" $header] >= 0 } {
        set header [string range $header 1 end-1]
        foreach dir $include_dirs {
            if { [file exists [file join $dir $header]] } {
                set header [file join $dir $header]
                break
            }
        }
    }
    if { ![file exists $header] } {
        puts "Error: header file not found - $header"
        return
    } else {
        puts "Including: $header"
    }
    #
    # Handle the contents
    #
    set infile [open $header r]
    set contents [read $infile]
    close $infile

    set contents [string map $::convert $contents]
    set contents [string map $::ignored $contents]

    #puts [transformToTcl $contents]
    eval [transformToTcl $contents]
}


# defined --
#     Check if a macro has been defined or not
#
# Arguments:
#     macro         Name of the macro
#
# Result:
#     None
#
proc defined {macro} {
    global macros

    info exists macros($macro)
}


# define --
#     Define a macro
#
# Arguments:
#     macro         Name of the macro
#     args          String of values assigned to it
#
# Result:
#     None
#
proc define {macro args} {
    global macros
    global declout

    set macros($macro) $args

    #
    # Check if the macro represents a value. If so, write to
    # the Fortran module as a parameter
    #
    # TODO: solve the subtleness of #define A "A" - the quotes disappear in Tcl!
    #
    if { [llength $args] == 1 } {
        set value [lindex $args 0]
        if { [string is integer -strict $value] } {
            puts $declout "integer(c_int), parameter :: $macro = [expr {$value+0}] ! $value"
        } elseif { [string is double -strict $value] } {
            puts $declout "real(c_double), parameter :: $macro = [expr {$value+0.0}] ! $value"
        } else {
            puts $declout "character(len=[string length $value]), parameter :: $macro = \"$value\""
        }
    }
}


# undef --
#     Undefine a macro
#
# Arguments:
#     macro         Name of the macro
#
# Result:
#     None
#
# Note:
#     This has no equivalent on the Fortran side.
#     Only do something if the macro has been defined before
#
proc undef {macro} {
    global macros

    if { [info exists macros($macro)] } {
        unset macros($macro)
    }
}


# enum --
#     Transform enumerations
#
# Arguments:
#     list          List of enumerated names (and possibly values)
#     args          Any names for this enumeration
#
# Result:
#     None
#
proc enum {list args} {
    global declout

    set value -1
    foreach e [split $list ,] {
        if { [string first = $e] < 0 } {
            set name [string trim $e]
            incr value
        } else {
            regexp { *([a-zA-Z_0-9]+) *= *([a-zA-Z_0-9]+)} $e ==> name value
        }

        puts $declout "integer, parameter :: $name = $value"
    }
}


# function --
#     Handle the definition of a function type
#
# Arguments:
#     definition    List of all components of the definition
#
# Result:
#     None
#
proc function {definition} {
    global ftype
    global isotype

    #
    # Construct the complete interface for this function type
    # Note: @ will be replaced by the actual argument name
    #
    set return_type [lindex $definition 0]
    set type_name   [lindex $definition 1 1]
    set arglist     [lindex $definition 2]

    if { $return_type == "void" } {
        set ftn_unit   "subroutine"
        set ftn_return ""
        set ftn_type   ""
    } else {
        set ftn_unit   "function"
        set ftn_return "result(result__)"
        set ftn_type   "
                $ftype($return_type) :: result__"    ;# Note: ftype!!
    }
    set f90_interface \
"interface
            $ftn_unit @ (ARGLIST) $ftn_return
                use iso_c_binding
                ARGTYPES$ftn_type
            end $ftn_unit
        end interface"

    set iso_interface \
"interface
            $ftn_unit @ (ARGLIST), bind(c) $ftn_return
                use iso_c_binding
                ARGTYPES$ftn_type
            end $ftn_unit
        end interface"

    set arguments {}
    set f90_argtypes  {}
    set iso_argtypes  {}
    set count     0
    foreach arg [split $arglist ,] {
        incr count
        set  type [lindex $arg 0]
        set  name [lindex $arg 1]
        if { $name == "" } {
            set name "arg__$count"
        }

        lappend arguments    $name
        lappend f90_argtypes "$ftype($type) :: $name"
        lappend iso_argtypes "$isotype($type) :: $name"
    }

    set arguments    [join $arguments    ", "]
    set f90_argtypes [join $f90_argtypes "\n              "]
    set iso_argtypes [join $iso_argtypes "\n              "]

    set ftype($type_name) [string map [list ARGLIST $arguments ARGTYPES $f90_argtypes] $f90_interface]
    set isotype($type_name) [string map [list ARGLIST $arguments ARGTYPES $iso_argtypes] $iso_interface]

    puts "Function: "
    puts "F90:   $ftype($type_name)"
    puts "ISO:   $isotype($type_name)"
}

# struct --
#     Handle the definition of a structure
#
# Arguments:
#     definition    List of all components of the definition
#
# Result:
#     None
#
proc struct {definition} {
    global typout

    #
    # Structures can have explicit names or be anonymous
    #
    if { [llength [lindex $definition 0]] == 1 } {
        set structname [lindex $definition 0]
        set contents   [lindex $definition 1]
        set types      [split [string map {" " ""} [lrange $definition 2 end]] ,]
    } else {
        set contents   [lindex $definition 0]
        set types      [split [string map {" " ""} [lrange $definition 1 end]] ,]
        set structname [lindex $types 0]
    }

    puts $typout "type $structname"
    eval $contents
    puts $typout "endtype $structname"

    #
    # TODO: something smart with the types
    #
}


# typedef --
#     Handle type definitions
#
# Arguments:
#     args          List of all arguments
#
# Result:
#     None
#
# Note:
#     At this moment only simple typedefs are treated!
#
proc typedef {args} {
    global typemap

    #
    # Simple case: two arguments
    #
    puts "Typedef: >>$args<<"
    if { [llength $args] == 2 } {
        set basic   [lindex $args 0]
        set newname [lindex $args 1]

        lappend typemap $newname $basic

        proc $newname {name {arglist {}} args} "cwrap $basic \$name \$arglist"

    } elseif { [lindex $args 0] eq "struct" } {

        struct [lrange $args 1 end]

    } elseif { [lindex $args 1 0] eq "*" } {

        function $args

    } else {
        #
        # More complex cases
        #
        puts "Typedef: [lindex $args 0] ... [lindex $args end] - not treated yet"
    }
}


# handleArgs --
#     Handle the command-line arguments
#
# Arguments:
#     argv          List of arguments
#
# Result:
#     List of parameters (name of the module, names of the header files)
#
proc handleArgs {argv} {

    set module  ""
    set isvalue 0
    set names   {}

    foreach arg $argv {
        if { $isvalue } {
            set isvalue 0
            set $var    $arg
        } else {
            switch -- $arg {
                "-module" {
                    set isvalue 1
                    set var     "module"
                }
                default {
                    set newnames [glob -nocomplain $arg]
                    if { [llength $newnames] > 0 } {
                        set names [concat $names $newnames]
                    } else {
                        puts "No file(s) by that name: $arg"
                    }
                }
            }
        }
    }

    if { $module == "" } {
        set module [file root [file tail [lindex $names 0]]]
    }
    regsub -all {[^a-zA-Z0-9_]} $module "" module

    return [list $module $names]
}


# comment, void, ... --
#     Auxiliary procedures
#
proc extern {args} {
    # No op: definition of external data
}
proc EXTERN {type args} {
    eval cwrap $type $args
}

foreach type {char int long float double void void*} {
    proc $type { name {arglist {--}} } [string map [list TYPE $type] {
        cwrap TYPE $name $arglist
    }]
}

proc unknown {cmdname args} {
    puts "Unknown type: $cmdname"
    puts "Prototype:   $args"
    return
}

# main --
#     Get the program going:
#     Options:
#     -module name   Set the name of the module (otherwise: cwrapper)
#     All others     Names of the header files to be treated
#
foreach {module names} [handleArgs $argv] {break}

set macros(__TCL__) ""

#
# Common places for system header files
#
if { $::tcl_platform(platform) == "windows" } {
    if { [info exists ::env(INCLUDE)] } {
        set include_dirs [split $::env(INCLUDE) \;]
    } else {
        set include_dirs "."
    }
} else {
    set include_dirs {/usr/include}
}

#
# These predefined macros should be handled via a command-line option
# or an inspection of the platform (or both)
#
set macros(_WIN32)   ""
set macros(_MSC_VER) 800
set macros(_M_IX86)  ""
set macros(__STDC__) 1

#
# Open the output files
#
set cout    [open "${module}_wrap.c"  w]
set ftnout  [open "${module}_mod.f90" w]
set isoout  [open "${module}_iso.f90" w]
set declout [open "${module}_decl.f90" w]
set typout  [open "${module}_types.f90" w]

prologue $module $names

foreach name $names {
    set infile [open $name r]
    set contents [read $infile]
    close $infile

    set contents [string map $convert $contents]
    set contents [string map $ignored $contents]

    #puts [transformToTcl $contents]
    eval [transformToTcl $contents]
}

epilogue $module
