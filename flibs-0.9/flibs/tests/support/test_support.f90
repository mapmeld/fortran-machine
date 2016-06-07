!
! logmsg --
!   Write a message into the log file
!
subroutine logmsg ( message )
  implicit none
  character(len=*), intent(in) :: message
  write(6,"(A)") trim(message)
  write(log_unit,"(A)") trim(message)
end subroutine logmsg
!
! log_startup --
!   Startup logger
!
subroutine log_startup ( filename )
  implicit none
  character(len=*), intent(in) :: filename
  open ( log_unit , file=filename, action = "write")
end subroutine log_startup
!
! log_shutdown --
!   Shutdown logger
!
subroutine log_shutdown ( )
  implicit none
  close ( log_unit )
end subroutine log_shutdown
!
! assert_startup --
!   Startup assertion system.
!
subroutine assert_startup ( )
  implicit none
  assertTotalTestFail = 0
  assertTotalTestSuccess = 0
  assertTestIndex = 0
end subroutine assert_startup
!
! assert_shutdown --
!   Shutdown assertion system.
!
subroutine assert_shutdown ( )
  implicit none
  character (len= 200 ) :: msg
  call logmsg ( "**********************" )
  call logmsg ( "End of tests." )
  write ( msg , * ) "Total number of tests : ", assertTotalTestSuccess + assertTotalTestFail
  call logmsg ( msg )
  write ( msg , * ) "Total number of success tests : ", assertTotalTestSuccess
  call logmsg ( msg )
  write ( msg , * ) "Total number of failing tests : ", assertTotalTestFail
  call logmsg ( msg )
end subroutine assert_shutdown
!
! assert --
!   Check that the given test is true and updates the assertion system.
!
subroutine assert (test, message)
  implicit none
  logical         , intent(in) :: test
  character(len=*), intent(in) :: message
  integer, parameter :: MSG_LEN = 200
  character (len= MSG_LEN ) :: msg
  assertTestIndex = assertTestIndex + 1
  if (.NOT.test) then
     write(msg,*) "-> Test #", assertTestIndex , " FAIL"
     call logmsg ( msg )
     write(msg,*) "Error: ", trim(message)
     call logmsg ( msg )
     assertTotalTestFail = assertTotalTestFail + 1
  else
     write(msg,*) "-> Test #", assertTestIndex , " PASS"
     call logmsg ( msg )
     assertTotalTestSuccess = assertTotalTestSuccess + 1
  endif
end subroutine assert

