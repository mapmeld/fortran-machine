# gentabletest.tcl --
#    Generate a test program based on the table specification
#
#    $Id: gentabletest.tcl,v 1.7 2008/09/30 13:37:02 arjenmarkus Exp $
#
#    TODO:
#    - RANGES (instead of UNCERTAINTIES)
#    - PRELIMINARIES
#

# data --
#     Gather the fixed code fragments here
#
set keywords {DECLARATIONS CODE ERROR RESULT TABLE TOLERANCE RANGES PRELIMINARIES}
set data(prologue) \
{! Test program
program test_table
    implicit none
    integer, parameter :: wp = kind(PRECISION)
    integer, parameter :: niters_ = TRIALS
    logical :: error_recognised_
    integer :: luout_
    integer :: lutbl_
    integer :: error_}

set data(precision) 1.0
set data(trials)    100

set data(expected_result) {}

set data(contains) \
{    error_ = 0
    luout_ = 10
    lutbl_ = 11
    open( luout_, file = 'report.out' )
    open( lutbl_, file = 'table.out' )
    write(*,*) 'Running random tests ...'
    call run_ranges
    write(*,*) 'Running specific tests ...'
    call all_tests
    write(luout_,'(a,i5)') 'All tests completed. Number of errors:',error_
    write(*,*) 'Done'
contains

logical function not_equal_abs( x, y, margin )
    real(kind=wp) :: x, y, margin
    not_equal_abs = abs(x-y) > margin
end function not_equal_abs

logical function not_equal_rel( x, y, margin )
    real(kind=wp) :: x, y, margin
    not_equal_rel = abs(x-y) > 0.5 * margin * (abs(x)+abs(y))
end function not_equal_rel

real function random_uniform( xmin, xmax )
    real(kind=wp) :: xmin, xmax
    real(kind=wp) :: r

    call random_number( r )
    random_uniform = xmin + (xmax-xmin) * r
end function random_uniform

real function random_normal( xmean, xstdev )
    real(kind=wp) :: xmean, xstdev
    real(kind=wp) :: r, phi

    call random_number( r )
    call random_number( phi )
    phi = 8.0_wp * atan(1.0_wp) * phi
    r   = sqrt( -2.0_wp * log(r) )
    random_normal = xmean + xstdev * r * cos(phi)
end function random_normal

subroutine recognised_error
    error_recognised_ = .true.
    write(luout_,'(a)') '    Note: error condition correctly recognised'
end subroutine recognised_error

subroutine unexpected_error
    write(luout_,'(a)') '    Note: unexpected error detected'
    error_ = error_ + 1
end subroutine unexpected_error

subroutine all_tests}

set data(end_all_tests) {end subroutine all_tests}

set data(epilogue) {
end program test_table}

set data(code)         {}
set data(error)        {}
set data(table)        {}
set data(declarations) {}

set data(ranges) {
subroutine run_ranges
    ! Nothing to do
end subroutine run_ranges}


# generateFromTable --
#     Read the file with the table specification and generate the
#     test code from that
#
# Arguments:
#     tblname      Name of the table file
#
# Result:
#     None
#
# Side effects:
#     Writes the test code
#
proc generateFromTable {tblname} {
    global data

    set infile  [open $tblname r]
    set outname "[file root $tblname].f90"

    #
    # Be careful not to override existing files
    #
    if { [file exists $outname] } {
        puts "Output file already exists: $outname"
        exit
    } else {
        puts "Writing test program $outname ..."
    }

    set outfile [open $outname w]

    set cont [gets $infile nextline]
    while { $cont >= 0 } {
        #puts "Line: $nextline"
        switch -re $nextline {
            "^ *!" {
                set cont [gets $infile nextline]
            }
            "DECLARATIONS" {
                set cont [readCodeFragment $infile "declarations" nextline]
            }
            "PRELIMINARIES" {
                set cont [readPreliminaries $infile nextline]
            }
            "CODE" {
                set cont [readCodeFragment $infile "code" nextline]
            }
            "ERROR" {
                set cont [readCodeFragment $infile "error" nextline]
            }
            "RESULT" {
                set cont [readResultParameters $infile nextline]
            }
            "RANGES" {
                set cont [readRanges $infile nextline]
            }
            "TABLE" {
                set cont [readTable $infile nextline]
            }
            default {
                # Ignore unknown keywords
                set cont [gets $infile nextline]
            }
        }
    }

    close $infile

    #
    # Generate the code from the various pieces
    #

    puts $outfile [string map [list PRECISION $data(precision) TRIALS $data(trials)] $data(prologue)]
    puts $outfile $data(expected_result)
    puts $outfile $data(contains)
    puts $outfile $data(declarations)
    puts $outfile "    write(lutbl_,'(100a12)') '[join $data(variables) ',']'"

    set case 0
    foreach {status entry} $data(table) {
        incr case
        puts $outfile "\n    write(luout_,'(a)') 'Test case: $case'"
        puts $outfile $entry
        puts $outfile $data(code)
        puts $outfile "    write(lutbl_,'(100g12.4)') [join $data(variables) ,]"
        if { $status == 2 } {
            puts $outfile "    error_recognised_ = .false."
            puts $outfile $data(error)
            puts $outfile "    if ( .not. error_recognised_ ) then"
            puts $outfile "        write(luout_,'(a)') '    Failure: error not recognised'"
            puts $outfile "    endif"
        }
        if { $status == 0 } {
            puts $outfile $data(report)
        }
    }

    puts $outfile $data(end_all_tests)
    if { ![info exists data(prologue_ranges)] } {
        puts $outfile $data(dummy_ranges)
    } else {
        puts $outfile $data(prologue_ranges)
        puts $outfile $data(code_ranges)
        puts $outfile $data(epilogue_ranges)
        puts $outfile $data(epilogue)
    }
}


# readCodeFragment --
#     Read a code fragment (skip comments)
#
# Arguments:
#     infile       Input file
#     key          Which key to fill
#     var          Variable to store the next line in
#
# Result:
#     Continue or not
#
# Side effects:
#     Stores the code fragment in the variable data($key)
#
proc readCodeFragment {infile key var} {
    upvar 1 $var nextline
    global data
    global keywords

    while { [gets $infile nextline] >= 0 } {
        if { [lsearch $keywords [string trim $nextline]] >= 0 } {
            return 1
        }
        if { ! [regexp {^ *!} $nextline] } {
            append data($key) "$nextline\n"
        }
    }

    #
    # Apparently the end of the file
    #
    return -1
}


# readPreliminaries --
#     Read the parameters for the prologue code
#
# Arguments:
#     infile       Input file
#     var          Variable to store the next line in
#
# Result:
#     Continue or not
#
# Side effects:
#     Stores the parameters in the variables data(precision) and data(trials)
#
proc readPreliminaries {infile var} {
    upvar 1 $var nextline
    global data
    global keywords

    while { [gets $infile nextline] >= 0 } {
        if { [lsearch $keywords [string trim $nextline]] >= 0 } {
            return 1
        }
        if { ! [regexp {^ *!} $nextline] } {
            if { [regexp {([a-z]+) += *([^ ]+)} $nextline ==> key value] } {
                switch -- $key {
                    "precision" {
                        if { $value == "double"} {
                            set value 1.0d0
                        } else {
                            set value 1.0
                        }
                        set data(precision) $value
                    }
                    "trials" {
                        if { [string is integer -strict $value] } {
                            set data(trials) $value
                        }
                    }
                }
            }
        }
    }

    #
    # Apparently the end of the file
    #
    return -1
}


# readResultParameters --
#     Read the information on the result parameters
#
# Arguments:
#     infile       Input file
#     var          Variable to store the next line in
#
# Result:
#     Continue or not
#
# Side effects:
#     Stores the information in the variables data(report),
#     data(result_vars) and data(expected_result)
#
proc readResultParameters {infile var} {
    upvar 1 $var nextline
    global data
    global keywords

    set cont   -1
    set params {}
    while { [gets $infile nextline] >= 0 } {
        if { [lsearch $keywords [string trim $nextline]] >= 0 } {
            set cont 1
            break
        }
        set nextline [lindex [split $nextline !] 0]
        if { [string trim $nextline] != "" } {
            if { [llength $nextline] == 2 } {
                lappend params [string tolower [lindex $nextline 0]] \
                               [string tolower [lindex $nextline 1]]
            } else {
                puts "Error: incorrect result line: $nextline"
            }
        }
    }

    #
    # Transform the list into useable code
    #
    foreach {p margin} $params {
        append data(expected_result) "    real(kind=wp) :: expected_$p, min_$p, max_$p\n"

        if { [string first % $margin] >= 0 } {
            set compare "not_equal_rel"
            set margin  [expr {[string map {% ""} $margin]/100.0}]
        } else {
            set compare "not_equal_abs"
        }
        set margin  "${margin}_wp"
        append data(report) "    if ( $compare\($p,expected_$p,$margin) ) then
        write(luout_,'(a,g12.4,a,g12.4)') &
            '    Error: $p = ',$p, ' - expected: ',expected_$p
        error_ = error_ + 1
    endif\n"
    }

    set data(result_vars) $params

    return $cont
}


# readTable --
#     Read the table with test cases
#
# Arguments:
#     infile       Input file
#     var          Variable to store the next line in
#
# Result:
#     Continue or not
#
# Side effects:
#     Stores the information in the variable data(table)
#
proc readTable {infile var} {
    upvar 1 $var nextline
    global data
    global keywords

    set cont  -1
    set table {}
    while { [gets $infile nextline] >= 0 } {
        if { [lsearch $keywords [string trim $nextline]] >= 0 } {
            set cont 1
            break
        }
        if { ! [regexp {^ *!} $nextline] } {
            lappend table [lindex [split $nextline !] 0]
        }
    }

    #
    # Transform the entries into useable code fragments
    #
    set varnames [string tolower [lindex $table 0]]
    set data(variables) $varnames

    foreach values [lrange $table 1 end] {

        set entry {}
        set status 0 ;# Ordinary

        foreach vn $varnames value $values {
            if { $value == "?" } {
                set value  "0.0  ! Undetermined" ;# Should not matter
                if { $status == 0 } {
                    set status 1   ;# No checks necessary
                }
            }
            if { $value == "ERROR" } {
                set value  "0.0  ! Actually ERROR" ;# Should not matter
                set status 2   ;# Check on flagged error condition necessary
            }

            if { [lsearch $data(result_vars) $vn] >= 0 } {
                append entry "    expected_$vn = $value\n"
            } else {
                append entry "    $vn = $value\n"
            }
        }
        lappend data(table) $status $entry
    }

    return $cont
}


# readRanges --
#     Read the ranges section
#
# Arguments:
#     infile       Input file
#     var          Variable to store the next line in
#
# Result:
#     Continue or not
#
# Side effects:
#     Stores the information in the variable data(ranges)
#
proc readRanges {infile var} {
    upvar 1 $var nextline
    global data
    global keywords

    set cont  -1
    set ranges {}
    while { [gets $infile nextline] >= 0 } {
        if { [lsearch $keywords [string trim $nextline]] >= 0 } {
            set cont 1
            break
        }
        if { ! [regexp {^ *!} $nextline] } {
            lappend ranges [lindex [split $nextline !] 0]
        }
    }

    #
    # Transform the entries into useable code fragments
    #
    set varnames {}
    set prologue "
subroutine run_ranges
$data(declarations)
    integer :: i_
"

    set result_vars {}
    foreach {vn tolerance} $data(result_vars) {
        append prologue "    real(wp) :: min_$vn = huge(1.0_wp), max_$vn = -huge(1.0_wp)\n"
        lappend result_vars $vn
    }

    set header   "    write(lutbl_,'(100a12)') &
"
    set epilogue {}
    set code {
    do i_ = 1,niters_
}
    set varnames {}
    foreach vars $ranges {

        set entry {}
        set status 0 ;# Ordinary

        foreach {vn v1 v2 type} $vars {
            lappend varnames $vn
            if { $type == "" } {
                set type $v2
            }
            append prologue "    real(wp) :: min_$vn = huge(1.0_wp), max_$vn = -huge(1.0_wp)\n"
            append header '$vn',

            switch -- $type {
                "Constant" {
                    append code "        $vn = $v1\n"
                }
                "Uniform" {
                    set v1 [expr {$v1-0.5*$v2}]
                    set v2 [expr {$v1+$v2}]
                    append code "        $vn = random_uniform($v1,$v2)\n"
                }
                "Normal" {
                    append code "        $vn = random_normal($v1,$v2)\n"
                }
            }
            append code "        if ( $vn < min_$vn ) min_$vn = $vn\n"
            append code "        if ( $vn > max_$vn ) max_$vn = $vn\n"
        }
    }

    set    epilogue "        $data(code)"
    append epilogue "        $data(error)"

    foreach vn $result_vars {
        append epilogue "        if ( $vn < min_$vn ) min_$vn = $vn\n"
        append epilogue "        if ( $vn > max_$vn ) max_$vn = $vn\n"
    }

    append epilogue "        $data(code)
        write(lutbl_,'(100g12.4)') &
[join [concat $varnames $result_vars] ,]
    enddo
    write(luout_,'(a)') 'Overview of iterations:'\n"
    foreach vn [concat $varnames $result_vars] {
        append epilogue "    write(luout_,'(a20,2g12.4)') '$vn', min_$vn, max_$vn\n"
    }

    append header ' [join $result_vars ','] '

    set code "$header
$code"

    append epilogue "
end subroutine run_ranges"

    set data(prologue_ranges) $prologue
    set data(code_ranges)     $code
    set data(epilogue_ranges) $epilogue

    return $cont
}


# writeMakefile --
#     Write a simple makefile - if none exists yet
#
# Arguments:
#     tblname      Input file
#
# Result:
#     None
#
# Side effects:
#     Simple makefile "xxx.mk" written
#
proc writeMakefile {tblname} {

    set outname "[file root $tblname].mk"

    #
    # Be careful not to override existing files
    #
    if { [file exists $outname] } {
        puts "Make file $outname already exists - not updated"
        return
    } else {
        puts "Writing make file $outname ..."
    }

    set rootname [file root $tblname]
    set outfile  [open $outname w]

    puts $outfile \
"# Makefile for $tblname
#
include ../../config/config.mk
include ../../config/options.mk

PROGRAMS\t=\t$rootname\$(EXEEXT)

#
# Name of the source file containing the code to be tested
# (no extension!)
#
SOURCE\t=\t... fill in ...

all:\t\$(PROGRAMS)

\$(SOURCE)\$(OBJEXT):\t\$(SOURCE).f90
\t\$(FC) \$(FFLAGS) \$(SOURCE).f90

$rootname\$(OBJEXT):\t$rootname.f90 \$(SOURCE)\$(OBJEXT)
\t\$(FC) \$(FFLAGS) $rootname.f90

$rootname\$(EXEEXT):\t$rootname\$(OBJEXT) \$(SOURCE)\$(OBJEXT)
\t\$(LD) \$(LDFLAGS) \$(LDOUTPUT) $rootname\$(OBJEXT) \$(SOURCE)\$(OBJEXT)"

    close $outfile
}

# main --
#     Run the program
#
if { [llength $argv] != 1 } {
    puts "Usage: [file tail $argv0] <name-of-table-file>"
} else {
    set tblname [lindex $argv 0]
    generateFromTable $tblname
    writeMakefile $tblname
    puts "Done"
}
