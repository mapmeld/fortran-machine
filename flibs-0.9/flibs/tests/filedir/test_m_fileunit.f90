!
! This program is a testing component for the module m_fileunit
!
program test_fileunit
  use m_fileunit, only : &
       fileunit_set_stoponerror , &
       fileunit_closeallopen, &
       fileunit_report, &
       fileunit_displayopen , &
       fileunit_getallopen , &
       fileunit_getfreeunit
  use m_platform, only : &
       platform_get_platform, &
       PLATFORM_PLATFORM_WINDOWS, &
       PLATFORM_PLATFORM_UNIX, &
       PLATFORM_PLATFORM_MAC, &
       platform_cd
  implicit none
  integer :: assertTotalTestSuccess
  integer :: assertTotalTestFail
  integer :: assertTestIndex
  integer , parameter :: log_unit = 12
  !
  ! Run the tests
  !
  call test_m_vfile_all ()
contains
  !
  ! Include support for unit tests.
  !
  include "test_support.f90"
  !
  ! Test all
  !
  subroutine test_m_vfile_all ()
    external :: file_error
    integer, parameter :: filelength = 200

    !
    ! Initialize the test system
    !
    call log_startup ( "test_m_fileunit.log" )
    call assert_startup ()
    !
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! Test the file unit processing commands
    !
    call test_unitprocessing ()
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! Shutdown the tests
    !
    call logmsg ( "Test suite completed" )
    call assert_shutdown ()
    call log_shutdown ()
  end subroutine test_m_vfile_all
  !
  ! Test the file unit processing commands
  !
  subroutine test_unitprocessing ()
    integer :: file_unit
    integer :: file_unit1
    integer :: file_unit2
    integer, parameter :: MSG_LEN = 500
    character (len= MSG_LEN ) :: msg
    integer :: nbunits
    integer :: other_file_unit
    integer , dimension(:) , pointer :: units
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! test #2 : get an unused file unit
    !
    call logmsg ( "Test #2 : get_unit" )
    file_unit = fileunit_getfreeunit (  )
    write ( msg ,*)  "Unused unit:", file_unit
    call logmsg ( msg )
    call assert (file_unit==1, "Wrong file unit. (1)")
    other_file_unit = fileunit_getfreeunit (  )
    call assert (other_file_unit==1, "Wrong file unit. (2)")
    open ( file_unit , file = "declaration.txt" )
    other_file_unit = fileunit_getfreeunit (  )
    write ( msg ,*)  "Unused unit:", other_file_unit
    call logmsg ( msg )
    call assert (other_file_unit==2, "Wrong file unit. (3)")
    close ( file_unit )
    other_file_unit = fileunit_getfreeunit (  )
    call assert (other_file_unit==1, "Wrong file unit. (4)")
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! Open several units and close all
    !
    file_unit1 = fileunit_getfreeunit (  )
    open ( unit = file_unit1 , file = "declaration.txt" )
    file_unit2 = fileunit_getfreeunit (  )
    open ( unit = file_unit2 , file = "declaration2.txt" )
    call fileunit_getallopen ( nbunits , units )
    call assert ( nbunits == 3 , "Wrong number of units")
    ! Unit #1 is the logger for the tests itself
    call assert ( units ( 1 ) == file_unit1 , "Wrong unit #1")
    call assert ( units ( 2 ) == file_unit2 , "Wrong unit #2")
    deallocate ( units )
    call fileunit_report ( log_unit , file_unit1 )
    call fileunit_report ( log_unit , file_unit2 )
    call fileunit_displayopen ( log_unit )
    ! We cannot do that here because of the logger for the tests !!!
    ! But, yes, it works.
    ! call fileunit_closeallopen ()
    close ( file_unit1 )
    close ( file_unit2 )
    call fileunit_getallopen ( nbunits , units )
    call assert ( nbunits == 1 , "Wrong number of units")
    deallocate ( units )
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
  end subroutine test_unitprocessing
  !
  ! check_fileunitsnb --
  !   Check that the number of opened units is OK
  !
  subroutine check_fileunitsnb ()
    integer :: nbunits
    integer , dimension(:) , pointer :: units
    call fileunit_getallopen ( nbunits , units )
    if ( nbunits /= 1 ) then
       call fileunit_displayopen ( log_unit )
    endif
    call assert ( nbunits == 1 , "Wrong number of units")
    deallocate ( units )
  end subroutine check_fileunitsnb
end program test_fileunit

