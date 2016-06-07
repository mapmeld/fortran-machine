!
! This program is a testing component for the module platform
!
program test_m_vstrplatform
  use m_vstrplatform, only : &
       vstrplatform_system, &
       vstrplatform_cd, &
       vstrplatform_stat , &
       vstrplatform_osstring , &
       vstrplatform_platformstring
  implicit none
  integer :: assertTotalTestSuccess
  integer :: assertTotalTestFail
  integer :: assertTestIndex
  integer , parameter :: log_unit = 12
  call test_m_vstrplatform_all ()
contains
  !
  ! Include support for unit tests.
  !
  include "test_support.f90"
  !
  ! Test all
  !
  subroutine test_m_vstrplatform_all ()
    !
    ! Initialize the test system
    !
    call log_startup ( "test_m_vstrplatform.log" )
    call assert_startup ()
    !
    ! Shutdown the tests
    !
    call logmsg ( "Test suite completed" )
    call assert_shutdown ()
    call log_shutdown ()
    
  end subroutine test_m_vstrplatform_all
end program test_m_vstrplatform

