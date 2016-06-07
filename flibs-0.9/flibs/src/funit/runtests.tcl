# runtests.tcl --
#    Tcl script to control a program that uses ftnunit
#    Name of the program: first argument
#
#    $Id: runtests.tcl,v 1.2 2008/01/26 11:15:10 arjenmarkus Exp $
#
proc createMainWindow {} {
    menu .m -type menubar
    menu .m.file -tearoff 0
    . configure -menu .m
    .m add cascade -label File -menu .m.file
    .m.file add command -label Exit -command exitTests -underline 1

    frame .f
    text .f.t -wrap none \
            -xscrollcommand {.f.x set} \
            -yscrollcommand {.f.y set}
    scrollbar .f.x -orient horizontal -command {.f.t xview}
    scrollbar .f.y -orient vertical   -command {.f.t yview}

    grid .f.t .f.y -sticky news
    grid .f.x -    -sticky news
    grid rowconfigure . 0 -weight 1
    grid columnconfigure . 0 -weight 1
    grid rowconfigure .f 0 -weight 1
    grid columnconfigure .f 0 -weight 1
    grid .f -sticky news

    wm protocol . WM_DELETE_WINDOW exitTests
}

proc exitTests {} {
    file delete -force "ftnunit.run"
    exit
}

proc runProgram {cmd} {
    puts "Running: $cmd ...."
    set infile [open "|$cmd" r]
    fileevent $infile readable [list getOutput $infile]
    fconfigure $infile -buffering line
}

proc getOutput {infile} {
    if { [gets $infile line] >= 0 } {
        .f.t insert end "$line\n"
        .f.t see end
        update idletasks
    } else {
        catch {
            close $infile  ;# End of file
        } msg
        .f.t insert end "$msg\n"
        .f.t see end
        set ::finished 1
    }
}

createMainWindow

#
# Run the program, repeatedly
#
file delete -force "runtests.log"
file delete -force "ftnunit.lst"

set outfile [open "ftnunit.run" w]
puts $outfile ALL
close $outfile

wm title . [string map {\{ "" \} ""} "Ftnunit: $argv"]

set go 1
while { [file exists "ftnunit.lst"] || $go } {
    set go 0
    runProgram $argv
    vwait ::finished
}
