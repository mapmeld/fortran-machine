program configtest
!!! system() needs no interface  
character(len=20) :: cmd         
logical :: exists                
inquire( file = "_system_", exist = exists)
if ( exists ) stop
cmd = "echo test"                
call system( cmd )               
open( 10, file = "gather.inp", position="append")
write( 10, "(a)" ) "_system_1"
close( 10 )
open( 10, file = "_system_")
close( 10 )
end program
