#
# A script to generate the html documentation from the 
# doctools man files.
#

package require doctools

#
# formatfile --
#   Process the given manfile and generate the associated
#   html file.
#
proc formatfile {manfile} {
    ::doctools::new mdt
    mdt configure -file $manfile
    mdt configure -format html
    set handle [open $manfile r]
    set content [read -nonewline $handle]
    close $handle
    set htmlcontent [mdt format $content]
    set htmlfile [computefilename $manfile .html]
    set handle [open $htmlfile w]
    puts -nonewline $handle $htmlcontent
    close $handle
    mdt destroy
    return $htmlfile
}
#
# computefilename --
#   Replace the extension in the given filename
#   with the given new extension.
#
proc computefilename {filename newextension} {
    set lastdot [string last "." $filename]
    incr lastdot -1
    set newname [string range $filename 0 $lastdot]
    append newname $newextension
    return $newname
}

#
# processall --
#   Process all man files in the current directory.
#
proc processall {} {
    set manfiles [glob "*/*.man"]
    foreach filename $manfiles {
        set isuptodate [isuptodate $filename]
        if {$isuptodate==0} then {
            puts "> Updating $filename..."
            formatfile $filename
        } else {
            puts "> Up-to-date: $filename "
        }
    }
    return ""
}
#
# isuptodate --
#   Returns 1 if the given man file is up-to-date,
#   with respect to the associated htmlfile.
#
proc isuptodate {manfile} {
    set htmlfile [computefilename $manfile .html]
    set fexists [file exists $htmlfile]
    if {$fexists==0} then {
        set result 0
    } else {
        set time1 [file mtime $manfile]
        set time2 [file mtime $htmlfile]
        set result [expr {$time1< $time2}]
    }
    return $result
}
#
# Executable part of the script.
#
processall

