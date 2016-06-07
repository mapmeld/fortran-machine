! test_m_exception.f90 --
!     Small test program for the m_exception component.
!
!     $Id: test_m_exception.f90,v 1.2 2008/06/18 10:36:11 relaxmike Exp $
!
program test_m_exception
  use m_exception, only : &
       exception_catch, &
       EXCEPTION_ERROR , &
       EXCEPTION_FAILURE , &
       EXCEPTION_FATAL_ERROR , &
       exception_getcounter , &
       exception_getlogunit, &
       EXCEPTION_INFORMATION , &
       exception_initcounter , &
       exception_islogactive ,&
       exception_logactive , &
       exception_raiseError, &
       exception_raiseFailure, &
       exception_raiseFatalError, &
       exception_raiseInformation, &
       exception_raiseWarning, &
       exception_setlogunit , &
       exception_setstoponerror , &
       EXCEPTION_SIZE , &
       EXCEPTION_WARNING
       implicit none
  integer :: assertTotalTestSuccess
  integer :: assertTotalTestFail
  integer :: assertTestIndex
  integer , parameter :: log_unit = 12

  call test_m_exception_all ()
contains
  !
  ! test_m_exception_all --
  !   Test m_exception services
  !
  subroutine test_m_exception_all ()
    integer, dimension ( 1 : EXCEPTION_SIZE ) :: counter
    logical :: islogactive
    integer :: current_logunit
    integer :: status
    external :: callback1
    external :: callback2
    real :: root
    interface 
       function compute_sqrt ( value ) result ( root )
         implicit none
         real, intent(in) :: value
         real :: root
       end function compute_sqrt
    end interface
    call log_startup ( "test_m_exception.log" )
    call assert_startup ( )
    !
    ! Configure the exception so that it do not break the tests.
    !
    call exception_setstoponerror ( .false. )
    !
    ! Simplest use-case
    !
    root = compute_sqrt ( -1. )
    call exception_getcounter ( counter )
    call assert ( counter ( EXCEPTION_ERROR ) == 1 , "Wrong error counter" )
    ! Clean-up
    call exception_initcounter ()
    !
    ! Generate all levels of exceptions
    !
    call exception_raiseFatalError (  "This is my message" )
    call exception_raiseError (  "This is my message" )
    call exception_raiseWarning (  "This is my message" )
    call exception_raiseInformation (  "This is my message" )
    call exception_raiseFailure (  "This is my message" )
    !
    ! Check counters
    !
    call exception_getcounter ( counter )
    call assert ( counter ( EXCEPTION_INFORMATION ) == 1 , "Wrong information counter" )
    call assert ( counter ( EXCEPTION_WARNING ) == 1 , "Wrong warning counter" )
    call assert ( counter ( EXCEPTION_ERROR ) == 1 , "Wrong error counter" )
    call assert ( counter ( EXCEPTION_FATAL_ERROR ) == 1 , "Wrong fatal error counter" )
    call assert ( counter ( EXCEPTION_FAILURE ) == 1 , "Wrong failure counter" )
    !
    ! Reset counters and check them
    !
    call exception_initcounter ()
    call exception_getcounter ( counter )
    call assert ( counter ( EXCEPTION_INFORMATION ) == 0 , "Wrong information counter" )
    call assert ( counter ( EXCEPTION_WARNING ) == 0 , "Wrong warning counter" )
    call assert ( counter ( EXCEPTION_ERROR ) == 0 , "Wrong error counter" )
    call assert ( counter ( EXCEPTION_FATAL_ERROR ) == 0 , "Wrong fatal error counter" )
    call assert ( counter ( EXCEPTION_FAILURE ) == 0 , "Wrong failure counter" )
    !
    ! Set the exception unit to the logfile and generate an error.
    !
    call exception_setlogunit ( log_unit )
    current_logunit = exception_getlogunit ()
    call assert ( current_logunit == log_unit , "Wrong log unit" )
    call exception_raiseFailure (  "This is my message" )
    !
    ! Set the exception unit back to the screen and generate an error.
    !
    call exception_setlogunit ( 0 )
    current_logunit = exception_getlogunit ()
    call assert ( current_logunit == 0 , "Wrong log unit" )
    call exception_raiseFailure (  "This is my message" )
    !
    ! Inactivate the exception logger and generate an error.
    !
    call exception_logactive ( .false. )
    islogactive = exception_islogactive ()
    call assert ( .NOT.islogactive , "Wrong log active flag" )
    call exception_raiseFailure (  "This is my message" )
    call exception_logactive ( .true. )
    islogactive = exception_islogactive ()
    call assert ( islogactive , "Wrong log active flag" )
    call exception_raiseFailure (  "This is my message" )
    !
    ! Test the pseudo-catch
    !
    call exception_catch ( callback1 , status )
    call assert ( status == 0 , "Wrong status" )
    call exception_catch ( callback2 , status )
    !
    ! Sample client code :
    !
    select case ( status )
    case ( EXCEPTION_INFORMATION )
       write(6,*) "Information"
    case ( EXCEPTION_WARNING )
       write(6,*) "Warning"
    case ( EXCEPTION_ERROR , EXCEPTION_FATAL_ERROR , EXCEPTION_FAILURE )
       write(6,*) "Fatal error"
    case default
       write(6,*) "No problem, continue."
    end select
    call assert ( status == EXCEPTION_FATAL_ERROR , "Wrong status" )
    !
    ! Shutdown the tests
    !
    call logmsg ( "Test suite completed" )
    call assert_shutdown ()
    call log_shutdown ()

  end subroutine test_m_exception_all
  !
  ! assert --
  !   Check that the given test is true and updates the assertion system.
  !
  subroutine assert (test, message)
    implicit none
    logical         , intent(in) :: test
    character(len=*), intent(in) :: message
    character(len=50) :: origin
    integer, parameter :: MSG_LEN = 200
    character (len= MSG_LEN ) :: msg
    origin = "test_m_vstring.f90"
    assertTestIndex = assertTestIndex + 1
    if (.NOT.test) then
       write(msg,*) "-> Test #", assertTestIndex , " FAIL"
       call logmsg ( msg )
       write(msg,*) "Origin:", origin
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
  !
  ! logmsg --
  !   Write a message into the log file
  !
  subroutine logmsg ( message )
    implicit none
    character(len=*), intent(in) :: message
    write(6,*) trim(message)
    write(log_unit,*) trim(message)
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
    write ( msg , * ) "Total number of success tests : ", assertTotalTestSuccess
    call logmsg ( msg )
    write ( msg , * ) "Total number of failing tests : ", assertTotalTestFail
    call logmsg ( msg )
  end subroutine assert_shutdown

end program test_m_exception

subroutine callback1 ()
end subroutine callback1

subroutine callback2 ()
  use m_exception, only : &
       exception_raiseFatalError
  implicit none
  call exception_raiseFatalError ( "Wrong blabla !" )
end subroutine callback2

function compute_sqrt ( value ) result ( root )
  use m_exception
  implicit none
  real, intent(in) :: value
  real :: root
  if ( value < 0. ) then
     call exception_raiseError ( "Value is negative in compute_sqrt" )
  else
     root = sqrt ( value )
  endif
end function compute_sqrt

