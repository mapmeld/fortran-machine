# dependencies.tcl
#     Examine the available Fortran sources and set up a
#     list of dependencies for use in make for instance
#
#     Limitations:
#     - include files are not examined
#     - no directory information about where to find the include file
#       is taken into account (that is: no -I options!)
#     - include files with a Fortran-type extensions are examined
#       as if they were ordinary source files, thus leading to
#       too many source files in the dependency list
#

# getDependencyItems --
#     Analyse the file and return a list of items
# Arguments:
#     filename       Name of the file to examine
# Result:
#     A list of lists:
#     - Element 1 is a list of modules defined in the file
#     - Element 2 is a list of external modules referred to in the file
#     - Element 3 is a list of include files
#
proc getDependencyItems {filename} {
    set infile [open $filename r]

    set provided {}
    set required {}
    set include  {}

    while { [gets $infile line] >= 0 } {

        switch -re -- [string tolower $line] {
            { *include +['"]} {
                lappend include [GetInclude $line]
            }
            { *use +[a-z]} {
                lappend required [GetUse $line]
            }
            { *module +[a-z]} {
                lappend provided [GetModule $line]
            }
        }
    }

    return [list $provided $required $include]
}

# GetInclude, GetUse, GetModule --
#     Extract the relevant information from the list
# Arguments:
#     line           Line of code
# Result:
#     Name of a file or a module
#
proc GetInclude {line} {
    return [lindex [split $line {'"}] 1]
}
proc GetUse {line} {
    return [lindex $line 1]
}
proc GetModule {line} {
    return [lindex $line 1]
}

# ObjectFile, ModuleFile --
#     Transform the file name
# Arguments:
#     name           Source file/module name
# Result:
#     Name of a file or a module
#
proc ObjectFile {name} {
    return "[file root $name].o"
}
proc ModuleFile {name} {
    return "[string toupper $name].mod"
}

# analyseAllFiles --
#     Analyse all files in the current directory
# Arguments:
#     filename       Name of the file for dependencies
#     type           Type of output ("make" or "ordered")
# Result:
#     None
#
proc analyseAllFiles {filename type} {

    set outfile [open $filename w]

    if { $::tcl_platform(platform) == "windows" } {
        set mask "*.f *.for *.f90"
    } else {
        set mask "*.f *.for *.f90 *.F *.FOR *.F90"
    }
    set files {}
    foreach f [eval glob -type f $mask] {
        foreach {provided required include} [getDependencyItems $f] {break}

        set deps($f) {}

        foreach i $include {
            lappend deps($f) $i
        }

        foreach m $provided {
            set mod [ModuleFile $m]
            lappend files $mod
            set deps($mod) $f
        }
        foreach m $required {
            set mod [ModuleFile $m]
            set deps($f) $mod
        }
        lappend files $f
    }

    switch -- $type {
        "make" {
            foreach f $files {
                if { [lsearch {.f .for .f90 .F .FOR .F90} [file extension $f]] >= 0 } {
                    puts $outfile [ObjectFile $f]\t:\t$f\n\t[join $deps($f) \\\n\t]
                } else {
                    puts $outfile $f\t:\t[join $deps($f) \\\n\t]
                }
            }
        }
        "ordered" {
            puts "TO BE DONE!"
        }
    }

    close $outfile
}

# main --
#     Get the thing going
#

analyseAllFiles test.mk make
