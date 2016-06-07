# mimick_server.tcl --
#     Mimick a CGI server using the GET method
#     Used to drive the test_cgi program
#
#

set query "function=sin&output=html&minimum=0.0&maximum=10.0&steps=20"

set env(QUERY_STRING) $query
catch {
set rc [exec test_cgi.exe]
puts $rc
} msg
puts $msg

#
# Force an error
#
set query "function=J0&output=html&minimum=0.0&maximum=10.0&steps=20"

set env(QUERY_STRING) $query
catch {
set rc [exec test_cgi.exe]
puts $rc
} msg
puts $msg
