program configtest
!!interface system               
!!    module procedure system_fnc
!!end interface                  
character(len=20) :: cmd         
logical :: exists                
inquire( file = "_system_", exist = exists)
if ( exists ) stop
cmd = "echo test"                
call system_fnc( cmd )           
open( 10, file = "gather.inp", position="append")
write( 10, "(a)" ) "_system_2"
close( 10 )
open( 10, file = "_system_")
close( 10 )
contains                         
subroutine system_fnc( cmd )     
    character(len=*) :: cmd      
    integer          :: rc       
    rc = system( cmd )           
end subroutine                   
end program
