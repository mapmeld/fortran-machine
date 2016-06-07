! test_m_assert.f90 --
!     Small test program for the m_assert component.
!
!     $Id: test_m_assert.f90,v 1.1 2008/04/08 09:06:53 relaxmike Exp $
!
program test_m_assert
  use m_assert, only : &
       assert_assert , &
       assert_setenabled , &
       assert_getenabled ,&
       ASSERT_INDEX_NUMBERTRUE , &
       ASSERT_INDEX_NUMBERFALSE , &
       ASSERT_INDEX_NUMBERTOTAL , &
       ASSERT_INDEX_MAX , &
       assert_getcounters ,&
       assert_initcounters, &
       assert_fatalError , &
       assert_error ,&
       assert_warning ,&
       assert_information ,&
       assert_failure
  use m_exception, only : &
       exception_setstoponerror
  implicit none
  integer :: assertTotalTestSuccess
  integer :: assertTotalTestFail
  integer :: assertTestIndex
  integer , parameter :: log_unit = 12

  call test_m_assert_all ()
contains
  !
  ! test_m_assert_all --
  !   Test m_exception services
  !
  subroutine test_m_assert_all ()
    logical :: assertenabled
    integer, dimension(1:ASSERT_INDEX_MAX) :: counters
      integer :: x , y

    call log_startup ( "test_m_assert.log" )
    call assert_startup ( )
    !
    ! Configure the exception so that it do not break the tests.
    !
    call exception_setstoponerror ( .false. )
    !
    ! Check the default enabled valued
    !
    assertenabled = assert_getenabled ()
    call assert ( assertenabled , "Wrong default enabled." )
    !
    ! Check that counters are correctly initialized
    !
    call assert_getcounters ( counters )
    call assert ( counters ( ASSERT_INDEX_NUMBERTRUE ) == 0 , "Wrong default counter." )
    call assert ( counters ( ASSERT_INDEX_NUMBERFALSE ) == 0 , "Wrong default counter." )
    call assert ( counters ( ASSERT_INDEX_NUMBERTOTAL ) == 0 , "Wrong default counter." )
    !
    ! Assert a true.
    ! Check that counters are correctly updated
    !
    call assert_assert ( .true. , "This is true." )
    call assert_getcounters ( counters )
    call assert ( counters ( ASSERT_INDEX_NUMBERTRUE ) == 1 , "Wrong counter." )
    call assert ( counters ( ASSERT_INDEX_NUMBERFALSE ) == 0 , "Wrong counter." )
    call assert ( counters ( ASSERT_INDEX_NUMBERTOTAL ) == 1 , "Wrong counter." )
    !
    ! Assert a false
    ! Check that counters are correctly updated
    !
    call assert_assert ( .false. , "This is false." )
    call assert_getcounters ( counters )
    call assert ( counters ( ASSERT_INDEX_NUMBERTRUE ) == 1 , "Wrong counter." )
    call assert ( counters ( ASSERT_INDEX_NUMBERFALSE ) == 1 , "Wrong counter." )
    call assert ( counters ( ASSERT_INDEX_NUMBERTOTAL ) == 2 , "Wrong counter." )
    !
    ! Template use
    !
    x = 2
    y = x**2
    call assert_assert ( y == 4 , "Wrong power value." )
    call assert_getcounters ( counters )
    call assert ( counters ( ASSERT_INDEX_NUMBERTRUE ) == 2 , "Wrong counter." )
    call assert ( counters ( ASSERT_INDEX_NUMBERFALSE ) == 1 , "Wrong counter." )
    call assert ( counters ( ASSERT_INDEX_NUMBERTOTAL ) == 3 , "Wrong counter." )
    !
    ! Reset counters
    !
    call assert_initcounters ()
    call assert_getcounters ( counters )
    call assert ( counters ( ASSERT_INDEX_NUMBERTRUE ) == 0 , "Wrong counter." )
    call assert ( counters ( ASSERT_INDEX_NUMBERFALSE ) == 0 , "Wrong counter." )
    call assert ( counters ( ASSERT_INDEX_NUMBERTOTAL ) == 0 , "Wrong counter." )
    !
    ! Assert all exceptions levels
    !
    call assert_fatalError (.false. , "Message")
    call assert_error (.false. , "Message")
    call assert_warning (.false. , "Message")
    call assert_information (.false. , "Message")
    call assert_failure (.false. , "Message")
    call assert_getcounters ( counters )
    call assert ( counters ( ASSERT_INDEX_NUMBERTRUE ) == 0 , "Wrong counter." )
    call assert ( counters ( ASSERT_INDEX_NUMBERFALSE ) == 5 , "Wrong counter." )
    call assert ( counters ( ASSERT_INDEX_NUMBERTOTAL ) == 5 , "Wrong counter." )
    !
    ! Shutdown the tests
    !
    call logmsg ( "Test suite completed" )
    call assert_shutdown ()
    call log_shutdown ()

  end subroutine test_m_assert_all
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
    origin = "test_m_assert.f90"
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

end program test_m_assert

