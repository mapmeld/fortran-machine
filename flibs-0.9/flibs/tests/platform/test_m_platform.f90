!
! This program is a testing component for the module platform
!
program test_m_platform
  use m_platform, only : &
       platform_get_os, &
       platform_get_platform, &
       PLATFORM_OS_WINDOWS_NT, &
       platform_system, &
       PLATFORM_PLATFORM_WINDOWS, &
       PLATFORM_PLATFORM_UNIX , &
       platform_cd, &
       platform_stat , &
       PLATFORM_OS_UNKNOWN , &
       PLATFORM_PLATFORM_UNKNOWN , &
       platform_osstring , &
       platform_platformstring
  implicit none
  integer :: assertTotalTestSuccess
  integer :: assertTotalTestFail
  integer :: assertTestIndex
  integer , parameter :: log_unit = 12
  call test_m_platform_all ()
contains
  subroutine test_m_platform_all ()
    integer :: os
    integer :: currentplatform
    
    integer, parameter :: MSG_LEN = 500
    character (len= MSG_LEN ) :: msg
    character (len= MSG_LEN ) :: command
    integer :: status
    character ( len = 200 ) :: dirname
    character ( len = 200 ) :: filename
    integer, dimension (1:13) :: statarray
    character ( len = 200 ) :: ostring
    character ( len = 200 ) :: platformstring
    integer :: lengthtrim
    !
    ! Initialize the test system
    !
    call log_startup ( "test_m_platform.log" )
    call assert_startup ()
    !
    ! test #1 : operating system
    !
    call logmsg ( "Test #1 : get_os" )
    ! Just test that it works
    os = platform_get_os ()
    write ( msg ,*) "Current os:" , os
    call logmsg ( msg )
    ! TODO : How to get another source of information for the current OS ?
    call assert ( os/=PLATFORM_OS_UNKNOWN , "Wrong OS.")
    !
    ! test #2 : general platform
    !
    call logmsg ( "Test #2 : get_platform" )
    ! Just test that it works
    currentplatform = platform_get_platform ()
    write ( msg ,*) "Current platform:" , currentplatform
    call logmsg ( msg )
    ! TODO : How to get another source of information for the current platform ?
    call assert ( currentplatform/=PLATFORM_PLATFORM_UNKNOWN , "Wrong platform.")
    !
    ! Test #3 : execute a command
    !
    call logmsg ( "Test #3 : platform_system" )
    select case (currentplatform)
    case (PLATFORM_PLATFORM_WINDOWS)
       write ( command , *) "dir"
    case (PLATFORM_PLATFORM_UNIX)
       write ( command , *) "ls"
    case default
       write(6,*) "Unknown command to execute for platform :", currentplatform
    end select
    call platform_system ( command , status )
    write ( msg ,*) "Status:" , status
    call logmsg ( msg )
    call assert ( status==0 , "Wrong command status for files list.")
    !
    ! Change directory
    !
    dirname = ".."
    call platform_cd ( dirname , status )
    call assert ( status==0 , "Wrong command status to change directory.")
    dirname = "platform"
    call platform_cd ( dirname , status )
    call assert ( status==0 , "Wrong command status to change directory.")
    !
    ! Get file status
    !
    filename = "declaration.txt"
    call platform_stat ( filename , statarray , status )
    call assert ( status==0 , "Wrong command status to change directory.")    
    call assert ( statarray(8)==5141 , "Wrong file size.")
    !
    ! Get the string from the OS
    !
    call platform_osstring ( ostring )
    call logmsg ( ostring )
    ! TODO : how to test ?
    lengthtrim = len_trim ( ostring )
    call assert ( lengthtrim>0 , "Wrong os string.")    
    !
    ! Get the string from the platform
    !
    call platform_platformstring ( platformstring )
    call logmsg ( platformstring )
    ! TODO : how to test ?
    lengthtrim = len_trim ( platformstring )
    call assert ( lengthtrim>0 , "Wrong platform string.")    
    !
    ! Shutdown the tests
    !
    call logmsg ( "Test suite completed" )
    call assert_shutdown ()
    call log_shutdown ()
    
  end subroutine test_m_platform_all

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
    origin = "test_m_filedir.f90"
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
end program test_m_platform

