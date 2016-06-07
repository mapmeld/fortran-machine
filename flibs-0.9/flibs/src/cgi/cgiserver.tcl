# cgiserver.tcl --
#     A tiny CGI/HTTP server useful for simple applications
#     Derived from the tiny CGI server - http://wiki.tcl.tk/16867
#     This was derived from "DustMotePlus"
#     The server supports a small fraction of the CGI/HTTP standard only
#
#     Purpose:
#     - Present HTML files
#     - Run programs via a simple protocol (no need to know
#       much about CGI)
#
#     Usage:
#         tclsh cgiserver.tcl ?-cfg configuration-file?
#
#     Features:
#     - You can show static HTML pages (requested URL ends in .html)
#     - You can run Tcl scripts (requested URL ends in .tcl) that
#       print the result on standard output and then finish
#     - You can run programs (URL ends in .exe) that use the simple
#       protocol exemplified in "cgi.f90" (see also below)
#
#     TODO: use cgi-bin/* instead of the extension
#
#     The configuration file can specify:
#     - root      The root directory of the application (default: startup directory)
#     - default   The start-up page (default: index.html)
#     - port      Port to be used (default: 80)
#     - encoding  The encoding of the data sent to the client (default: iso8859-1)
#
#     What configuration file:
#     - If the configuration file is not specified on the command-line
#       via "-cfg name", then a file cgiserver.cfg is sought in the
#       current directory.
#     - If it does not exist, the internal defaults are used
#
#     Protocol:
#     The protocol implemented for executable programs (URL: *.exe)
#     is very simple:
#     - The program is started and fed the key-value pairs from the
#       request (on standard input). To dinstinguish requests, lines
#       with %BEGIN% and %END%
#       are added.
#     - The program must write its results to a file "cgiout" and on
#       completion of that file, create a file "cgiready".
#     - The server program takes care of all the rest.
#     The reason for using an external file and not standard output
#     is that there is no buffering problem then and the end of the
#     processing is very clear-cut. That way the program can run in
#     a loop waiting for new requests.
#
#     Known limitations:
#     - The protocol is very simple, it does not cope well with text
#       from a textarea control
#     - No support for multiple instances of the same program at the
#       moment (this requires directory and process management)
#       This means among other things, that the output may get
#       confused if more than process is running at the same time
#     - No checking of the incoming connections, you can do this
#       however by redefining the "answer" procedure in the
#       configuration file
#     - On Windows, if you start this script with wish or a similar
#       shell, the server socket will not be closed properly when
#       terminating the server.
#
#     Ideas for improvements:
#     - Run Tcl scripts in their own interpreter, instead of a separate
#       process?
#     - Add some logging facilities?
#     - Present a graphical screen if Tk is available?
#
#     Note:
#     This server is not intended to become a full-fledged HTTP/CGI
#     server system a la Apache or AOLserver. Only the simplest
#     processing options are implemented.
#

# defaults --
#
set root      [pwd]
set default   index.html
set port      8015
set encoding  iso8859-1

# bgerror --
#     Handle background errors (echo to the screen and to the client)
# Arguments:
#     msg       Error message
# Result:
#     None
#
proc bgerror msg {
    global clientSock
    puts stdout "bgerror: $msg\n$::errorInfo"
    puts $clientSock "HTTP/1.0 200 OK"
    puts $clientSock "Content-Type: text/plain;charset=$::encoding\n"
    puts $clientSock "Processing error: $msg\n$::errorInfo"
    close $clientSock
}

# answer --
#     Accept incoming connections
# Arguments:
#     socketChannel     Channel for the incoming connection
#     host2             Host of the connection
#     port2             Port of the connection
# Result:
#     None
# Note:
#     Right now any connection is accepted
#
proc answer {socketChannel host2 port2} {
  fileevent $socketChannel readable [list serve $socketChannel]
}

# serve --
#     Handle the incoming request
# Arguments:
#     sock      Channel for the socket
# Result:
#     None
# Side effects:
#     Sends data to the client, may start external programs
# Note:
#     Currently, the external programs are not kept alive
#
proc serve sock {
    global clientSock
    set clientSock $sock ;# For background errors
    fconfigure $sock -blocking 0

    #
    # Get the input from the GET request
    #
    gets $sock line
    puts "Received: $line"

    if { [fblocked $sock] } {
        puts "Socket appears to be blocked - no processing"
        return
    }

    #
    # No more input needed, so construct the proper URL
    # and identify the action to be taken
    #
    fileevent $sock readable ""
    set tail /
    regexp {(/[^ ?]*)(\?[^ ]*)?} $line -> tail args
    if {[string match */ $tail]} {append tail $::default}
    set name [string map {%20 " "} $::root$tail]
    set args [string range $args 1 end]

    #
    # The URL must be an existing file:
    # - A Tcl script
    # - An executable (hm, how to deal with platforms that do not use
    #   the extension .exe?)
    # - An HTML file (anything else, actually)
    #
    set exe 0
    set perm r
    if {[file readable $name]} {
        puts $sock "HTTP/1.0 200 OK"
        if {[file extension $name] eq ".tcl"} {
            set ::env(QUERY_STRING) [string range $args 1 end]
            set name [list |tclsh $name]
        } elseif {[file extension $name] eq ".exe"} {
            set exe  1
            set perm w+
            set name [list |$name]
        } else {
           puts $sock "Content-Type: text/html;charset=$::encoding\n"
        }
        set inchan [open $name $perm]
        if { $exe } {
            puts "Executing program $name ..."
            sendHttpData $inchan $args
            waitForProgram
            close $inchan
            set inchan [open "cgiout"]
        }

        #
        # Copy the output to the client
        #
        fconfigure $inchan -translation binary
        fconfigure $sock   -translation binary
        fcopy $inchan $sock -command [list done $inchan $sock]
    } else {
        puts "URL/File $name not found!"
        puts $sock "HTTP/1.0 404 Not found\n"
        puts $sock "<html><head><title>No such URL</title></head>"
        puts $sock "<body><center>"
        puts $sock "The URL you requested does not exist on this site."
        puts $sock "</center></body></html>"
        close $sock
    }
}

# done --
#     Callback for the fcopy command - close the channels
# Arguments:
#     file         Incoming channel
#     sock         Outgoing channel (the client)
#     bytes        Number of bytes that was sent
#     msg          Any message from the fcopy command
proc done {file sock bytes {msg {}}} {
    close $file
    close $sock
}

# sendHttpData --
#     Send key/value pairs to the CGI process
# Arguments:
#     inchan       Channel of incoming process
#     httpdata     HTTP data sent by the client
# Result:
#     None
# Notes:
#     The CGI process receives the data on stdin
#     The data start with the line %BEGIN% and
#     and end with %END%, so that there can be
#     no ambiguity over them
#
proc sendHttpData {inchan httpdata} {
    file delete -force cgiout
    file delete -force cgiready

    puts $inchan %BEGIN%
    foreach keyvalue [split $httpdata "&"] {
        puts $inchan $keyvalue
    }
    puts $inchan %END%
    flush $inchan
}

# waitForProgram --
#     Wait for the program to finish its job
# Arguments:
#     None
# Result:
#     None
#
proc waitForProgram {} {
    checkFile cgiready
    vwait continue
}
proc checkFile {name} {
    if { [file exists $name] } {
        set ::continue 1
    } else {
        after 20 [list checkFile $name]
    }
}

# main --
#     Get the thing started
#

set cfg [lsearch $argv "-cfg"]
if { $cfg >= 0 } {
    incr cfg
    set cfgfile [lindex $argv $cfg]
} else {
    set cfgfile "cgiserver.cfg"
}

if { [file readable $cfgfile] } {
    puts "Using configuration file $cgfile"
    source $cgifile
}   else {
    puts "Using default configuration"
}

#
# Check the default URL and start the server
#
if { ! [file readable [file join $root $default]] } {
    puts "Error: could not find startup file/URL -  [file join $root $default]"
    puts "Stopping server"
} else {
    socket -server answer $port
    puts "Server ready..."
    vwait forever
}
