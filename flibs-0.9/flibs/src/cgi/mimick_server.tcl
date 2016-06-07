# mimick_server.tcl --
#     Mimick a CGI server (various modes)
#     Intended for testing purposes only
#
#

set query "value1=1&value2=2&text=x+y%41+z"

#
# Classic CGI: GET method
#
set env(QUERY_STRING) $query
catch {
set rc [exec get_data.exe]
puts $rc
} msg
puts $msg

#
# Classic CGI: POST method
#
unset env(QUERY_STRING)
set env(CONTENT_LENGTH) [string length $query]
catch {
set rc [exec get_data.exe <<$query]
puts $rc
} msg
puts $msg

#
# Simple file protocol
#
unset env(CONTENT_LENGTH)
set query "%BEGIN%\n[string map [list & \n] $query]\n%END%"
catch {
set rc [exec get_data.exe <<$query]
puts $rc
} msg
puts $msg


#
# Simple CGI
#
#unset env(CONTENT_LENGTH)
#set outfile TODO
#set rc [exec get_data.exe]
#puts $rc

