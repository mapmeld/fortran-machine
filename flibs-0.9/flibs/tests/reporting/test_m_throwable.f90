! test_m_throwable.f90 --
!     Small test program for the m_throwable component.
!
!     $Id: test_m_throwable.f90,v 1.1 2008/04/09 07:29:56 relaxmike Exp $
!
program test_m_throwable
  use m_throwable, only : &
       throwable_new , &
       throwable_free , &
       T_THROWABLE ,&
       throwable_iscause ,&
       throwable_getcause , &
       throwable_getmessage , &
       throwable_write , &
       throwable_printStackTrace , &
       throwable_exists
       implicit none
  integer :: assertTotalTestSuccess
  integer :: assertTotalTestFail
  integer :: assertTestIndex
  integer , parameter :: log_unit = 12

  call test_m_throwable_all ()
contains
  !
  ! test_m_throwable_all --
  !   Test m_throwable services
  !
  subroutine test_m_throwable_all ()
    type ( T_THROWABLE ), pointer :: mythrow1
    type ( T_THROWABLE ), pointer :: mythrow2
    type ( T_THROWABLE ), pointer :: mythrow3
    type ( T_THROWABLE ), pointer :: mythrow4
    character (len=200) :: message
    logical :: iscause
    logical :: catch
    call log_startup ( "test_m_throwable.log" )
    call assert_startup ( )
    !
    ! Create / destroy an empty throwable
    !
    call throwable_new ( mythrow1 )
    iscause = throwable_iscause ( mythrow1 )
    call assert ( .not.iscause , "Wrong cause." )
    call throwable_free ( mythrow1 )
    !
    ! Create / destroy a throwable with message
    !
    call throwable_new ( mythrow1 , "Message 1" )
    iscause = throwable_iscause ( mythrow1 )
    call assert ( .not.iscause , "Wrong cause." )
    call throwable_free ( mythrow1 )
    !
    ! Create / destroy a throwable with cause
    !
    call throwable_new ( mythrow2 )
    call throwable_new ( mythrow1 , mythrow2 )
    iscause = throwable_iscause ( mythrow1 )
    call assert ( iscause , "Wrong cause." )
    call throwable_free ( mythrow1 )
    !
    ! Create / destroy a throwable with cause and message
    !
    call throwable_new ( mythrow2 , "My throw 2")
    call throwable_new ( mythrow1 , mythrow2  , "My throw 1" )
    iscause = throwable_iscause ( mythrow1 )
    call assert ( iscause , "Wrong cause." )
    call throwable_free ( mythrow1 )
    !
    ! Get the cause
    !
    call throwable_new ( mythrow2 , "My throw 2")
    call throwable_new ( mythrow1 , mythrow2  , "My throw 1" )
    ! mythrow3 equals mythrow2
    call throwable_getcause ( mythrow1 , mythrow3 )
    call throwable_getmessage ( mythrow3 , message )
    call assert ( trim(message) == "My throw 2" , "Wrong message." )
    call throwable_free ( mythrow1 )
    !
    ! throwable_write
    !
    call throwable_new ( mythrow1 , "My throw 1" )
    call throwable_write ( mythrow1 , 6 )
    call throwable_write ( mythrow1 , log_unit )
    call throwable_free ( mythrow1 )
    !
    ! throwable_printStackTrace on unit
    !
    call throwable_new ( mythrow1 , "My throw 1" )
    call throwable_new ( mythrow2 , mythrow1 , "My throw 2" )
    call throwable_new ( mythrow3 , mythrow2 , "My throw 3" )
    call throwable_printStackTrace ( mythrow3 , 6 )
    call throwable_printStackTrace ( mythrow3 , log_unit )
    call throwable_free ( mythrow3 )
    !
    ! throwable_printStackTrace with callback
    !
    call throwable_new ( mythrow1 , "My throw 1" )
    call throwable_new ( mythrow2 , mythrow1 , "My throw 2" )
    call throwable_new ( mythrow3 , mythrow2 , "My throw 3" )
    call throwable_printStackTrace ( mythrow3 , logmsg )
    call throwable_free ( mythrow3 )
    !
    ! Sample use of throwable.
    !
    call samplesub_high1 ( mythrow1 )
    catch = throwable_exists ( mythrow1 )
    call assert ( .not.catch , "Wrong catch." )
    if ( catch ) then
       !
       ! Process the specific type of throwable object.
       !
    endif
    !
    ! Sample use of throwable with never allocated throw.
    !
    call samplesub_high1 ( mythrow4 )
    catch = throwable_exists ( mythrow4 )
    call assert ( .not.catch , "Wrong catch." )
    if ( catch ) then
       !
       ! Process the specific type of throwable object.
       !
    endif
    !
    ! Use throwable with a throwable object created
    !
    call samplesub_high2 ( mythrow4 )
    catch = throwable_exists ( mythrow4 )
    call assert ( catch , "Wrong catch." )
    if ( catch ) then
       !
       ! Process the specific type of throwable object.
       !
       call throwable_printStackTrace ( mythrow4 )
       write ( * , * ) "We should stop."
    endif
    !
    ! Use throwable with a full stack of cause
    !
    call samplesub_high3 ( mythrow4 )
    catch = throwable_exists ( mythrow4 )
    call assert ( catch , "Wrong catch." )
    if ( catch ) then
       !
       ! Process the specific type of throwable object.
       !
       call throwable_printStackTrace ( mythrow4 )
       write ( * , * ) "We should stop."
    endif
    !
    ! Shutdown the tests
    !
    call logmsg ( "Test suite completed" )
    call assert_shutdown ()
    call log_shutdown ()

  end subroutine test_m_throwable_all
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
    origin = "test_m_throwable.f90"
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
  !
  ! Sample subroutine which does not generate a throwable object.
  !
  subroutine samplesub_high1 ( throwable )
    type ( T_THROWABLE ), pointer :: throwable
    !
    ! No problem therefore nothing to do on throwable.
    !
  end subroutine samplesub_high1
  !
  ! Sample subroutine which generates a throwable object.
  !
  subroutine samplesub_high2 ( throwable )
    type ( T_THROWABLE ), pointer :: throwable
    !
    ! Problem therefore create the trowable
    !
    call throwable_new ( throwable , "Problem in samplesub_high2" )
  end subroutine samplesub_high2
  !
  ! Sample subroutine which generates a throwable object,
  ! depending on a lower-level cause.
  !
  subroutine samplesub_high3 ( throwable )
    type ( T_THROWABLE ), pointer :: throwable
    type ( T_THROWABLE ), pointer :: throwablelow
    call samplesub_low3 ( throwablelow )
    if ( throwable_exists ( throwablelow ) ) then
       !
       ! Problem therefore create the trowable
       !
       call throwable_new ( throwable , throwablelow , "Problem in samplesub_high3" )
    endif
  end subroutine samplesub_high3
  !
  ! Sample low-level subroutine which does  generate a throwable object.
  !
  subroutine samplesub_low3 ( throwable )
    type ( T_THROWABLE ), pointer :: throwable
    !
    ! Problem therefore create the trowable
    !
    call throwable_new ( throwable , "Problem in samplesub_low3" )
  end subroutine samplesub_low3
end program test_m_throwable


