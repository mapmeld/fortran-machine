# instrument.tcl --
#     Instrument the Fortran source code - see w_.f90 for more details
#

# scanCode --
#     Scan the Fortran source file and make an instrumented copy
#
# Arguments:
#     filename     Name of the file
#
# Result:
#     None
#
# Side effects:
#     A copy is created of the original file with the extension ".org"
#
proc scanCode {filename} {
    file rename $filename $filename.org

    set infile   [open $filename.org]
    set contents [readSource $infile [typeOfForm $filename]]

    set outfile [open $filename w]

    set return   99999
    set inunit   0

    foreach line $contents {
        foreach {lineno line} $line {break}
        puts "[typeOfLine $line] $line"
        switch -- [typeOfLine $line] {
            "call" {
                set callee [getCallee $line]
                puts $outfile "      call w_call('$filename',$lineno,'$caller','$callee')"
                puts $outfile $line
                puts $outfile "      call w_endcall"
            }
            "ifcall" {
                set callee [getCallee $line]
                set newcode "call w_call('$filename',$lineno,'$caller','$callee')"
                puts $outfile [replaceIfCall $line $newcode]
            }
            "goto" {
                set goto   [getGotoLabel $line]
                set goto   "call w_goto('$filename',$lineno,'$caller', '$goto',*$goto)"
                puts $outfile [replaceKeyword goto [lrange $line 0 end-1] $goto]
            }
            "stop" {
                set stop   "call w_stop('$filename',$lineno,'$caller')"
                puts $outfile [replaceKeyword stop $line $stop]
            }
            "open" {
                set open    "call w_open('$filename',$lineno,'$caller'"
                puts $outfile [replaceOpenClose open $line $open $return]
            }
            "close" {
                set close   "call w_close('$filename',$lineno,'$caller'"
                puts $outfile [replaceOpenClose close $line $close $return]
            }
            "return" {
                set ret    "call w_return('$filename',$lineno,'$caller', *$return)"
                puts $outfile [replaceKeyword return $line $ret]
            }
            "startmodule" {
                set caller ""
                puts $outfile $line
                puts $outfile "      use w_"
                set inunit 1
            }
            "startunit" {
                set caller [getCaller $line]
                puts $outfile $line
                if { $inunit == 0 } {
                    puts $outfile "      use w_"
                    set inunit  1
                }
            }
            "contains" {
                if { $caller != "" } {
                    puts $outfile "99999 continue"
                }
                puts $outfile $line
                incr inunit
            }
            "endmodule" {
                puts $outfile $line
                incr inunit -1
            }
            "endunit" {
                set caller ""
                puts $outfile "99999 continue"
                puts $outfile $line
                incr inunit -1
            }
            "end" {
                if { $caller != "" } {
                    puts $outfile "99999 continue"
                }
                puts $outfile $line
                incr inunit -1
            }
            default {
                puts $outfile $line
            }
        }
     }

     close $infile
     close $outfile
}

# typeOfLine --
#     Determine the type of the line of source code
#
# Arguments:
#     line         Line of source code (complete statement)
#
# Result:
#     One of the keywords used by [scanCode]
#
proc typeOfLine {line} {

    switch -regexp [string tolower $line] {
        "^c " -
        " *!" {
            return comment
        }
        "^ *module +" {
            return startmodule
        }
        "^ *contains" {
            return contains
        }
        "^ *program +" -
        "^ *subroutine +" -
        "function +" {
            return startunit
        }
        "^ *end *program" -
        "^ *end *subroutine" -
        "^ *end *function" {
            return endunit
        }
        "^ *end$" {
            return end
        }
        "if \\(.*\\) *(&\n)? *call " {
            return ifcall
        }
        " *call " - "! *" {
            return call
        }
        " *open *\\(" {
            return open
        }
        " *close *\\(" {
            return close
        }
        " *go *to \[0-9\]" {
            return goto
        }

        " *return\$" {
            return return
        }
        " *stop\$" {
            return stop
        }
        default {
            return any
        }
    }
}

# typeOfForm --
#     Determine if the source file is free-form or fixed-form
#
# Arguments:
#     filename     Name of the file
#
# Result:
#     Free-form (1) or fixed-form (0) code
#
proc typeOfForm {filename} {

    return [string match -nocase "*.f90" $filename]
}

# readSource --
#     Scan the Fortran source file and return the contents as a list of
#     line numbers and complete statements
#
# Arguments:
#     infile       Handle to the file
#     type         Free-form (1) or fixed-form (0) code
#
# Result:
#     List of lists: each containing the line number of the first line
#     making up the statement and then the complete statement
#
proc readSource {infile type} {

    set content {}

    set lineno  1
    set orgline $lineno
    set outline ""
    if { $type } {
        while { [gets $infile line] >= 0 } {
            set line [string trim $line]
            incr lineno

            if { [string range $line end end] == "&" } {
                append outline "$line\n"
            } else {
                append outline "$line"
                lappend content [list $orgline $outline]

                set outline ""
                set orgline $lineno
            }
        }
    } else {
        while { [gets $infile line] >= 0 } {
            set line [string trim $line]
            incr lineno

            if { [string range $line 5 5] == " " } {
                append outline "$line"
                lappend content [list $orgline $outline]

                set outline ""
                set orgline $lineno
            } else {
                append outline "$line\n"
            }
        }
    }



    append outline $line
    lappend content [list $orgline $outline]

    return $content
}

# getGotoLabel --
#     Extract the goto label
#
# Arguments:
#     line         Line of code
#
# Result:
#     Label the statement refers to
#
proc getGotoLabel {line} {

    regexp {go *to +([0-9]+)} $line => goto
    return $goto
}

# getCallee --
#     Extract the name of the called subroutine
#
# Arguments:
#     line         Line of code
#
# Result:
#     Name of the subroutine
#
proc getCallee {line} {

    regexp -nocase {call +([a-z0-9_]+)} $line => callee
    return $callee
}

# getCaller --
#     Extract the name of the (calling) subroutine
#
# Arguments:
#     line         Line of code
#
# Result:
#     Name of the subroutine
#
proc getCaller {line} {

    regexp -nocase {(function|subroutine|program) +([a-z0-9_]+)} $line => what caller
    return $caller
}

# replaceKeyword --
#     Replace a keyword in the line of code
#
# Arguments:
#     keyword      Keyword to be replaced
#     line         Line of code containing the keyword
#     newcode      Code to replace it by
#
# Result:
#     New line of code
#
proc replaceKeyword {keyword line newcode} {

    set index [string last $keyword $line]
    if { $index < 0 } {
        set index 0
    }
    regsub -nocase -start $index $keyword $line $newcode line
    return $line
}

# replaceIfCall --
#     Instrument the combined if/call statement
#
# Arguments:
#     line         Line of code containing the if/call
#     newcode      Code to replace it by
#
# Result:
#     New line of code
#
proc replaceIfCall {line newcode} {

    return "[replaceKeyword call $line "then\n      $newcode\n      call"]\
\n      call w_endcall\
\n      endif"
}

# replaceOpen --
#     Replace the open statement
#
# Arguments:
#     keyword      Keyword to be replaced
#     line         Line of code containing the keyword
#     newcode      Code to replace "open(" (including the opening brace) by
#     label        Default label to jump to
#
# Result:
#     New line of code
#
proc replaceOpenClose {keyword line newcode label} {
    set index [string last $keyword $line]

    if { [regexp -nocase {err *= *([0-9]+)} $line => newlabel] } {
        set label $newlabel
    }

    regsub -nocase -start $index "$keyword *\\("  $line "$newcode,*$label," line
    return $line
}


# main --
#     Get the transformation going by examining all Fortran files
#     (or reading the names from the file "instrument.names")
#
set names {}
if { ! [file exists "instrument.names"] } {
    foreach ext {f for f90} {
        set names [concat $names [glob -nocomplain *.$ext]]
    }
} else {
    set infile [open "instrument.names"]
    while { [gets $infile line] >= 0 } {
        if { [file exists $line] } {
            lappend names $line
        }
    }
}

set hastk 0
catch {
    console show
    set hastk 1
}

puts $names
foreach file $names {
    puts "$file ..."
    scanCode $file
}

if { $hastk } {
    puts "Done - type \"exit\" to finish"
}
