!
! m_assert --
!   This module provides an interface to check for assertions and 
!   generate corresponding errors.
!
!   This component is based on the m_exception module.
!   The main service of the component is the subroutine "assert_assert"
!   which check that the given logical variable is true. The following
!   is a sample use. If the condition is not .true., then an exception
!   of error type is raised by the m_exception module.
!   
!      use m_assert, only : assert_assert
!      integer :: x , y
!      x = 2
!      y = x**2
!      call assert_assert ( y == 4 , "Wrong power value." )
!
!   The user can choose a more specific type of exception raised
!   by choosing the correspondant assert_failure, assert_fatalError, 
!   assert_error, assert_warning or assert_information service.
!
!   The component can influence performances because of the 
!   internal computation performed to check the assertions, which may be 
!   undesirable in optimized mode. 
!   Therefore, the assertions are checked only if the pre-processing
!   macro _ASSERT_ENABLE is defined. If it is undefined, there is no 
!   impact on performances.
!
!   The component can be dynamically enabled or disabled with  
!   assert_setenabled ; one can see if the component is currently 
!   enabled with assert_getenabled.
!
!   The user can check the number of assertions that were successful
!   and the number of assertions which failed with assert_getcounters,
!   and reset the internal counters with assert_initcounters.
!
!   To control more precisely the behaviour of the m_assert component
!   when an exception occurs, the user can directly configure m_exception.
!
! Copyright (c) 2008 Michael Baudin
!
! $Id: m_assert.f90,v 1.2 2008/05/06 08:39:47 relaxmike Exp $
!
module m_assert
  use m_exception, only : &
       exception_raiseError , &
       exception_raiseFailure , &
       exception_raiseFatalError , &
       exception_raiseInformation, &
       exception_raiseWarning
  implicit none
  private
  !
  ! Public methods
  !
  public :: assert_fatalError
  public :: assert_error
  public :: assert_warning
  public :: assert_information
  public :: assert_failure
  public :: assert_getenabled
  public :: assert_setenabled
  public :: assert_assert
  public :: assert_getcounters
  public :: assert_initcounters
  !
  ! Set to true to enable assertion checking
  !
  logical :: assert_enabled = .true.
  !
  ! Counter of the number of assertions that were true, false, total
  !
  integer :: assert_numbertrue = 0
  integer :: assert_numberfalse = 0
  integer :: assert_numbertotal = 0
  !
  ! Flag for the counters
  !
  integer, parameter, public :: ASSERT_INDEX_NUMBERTRUE = 1
  integer, parameter, public :: ASSERT_INDEX_NUMBERFALSE = 2
  integer, parameter, public :: ASSERT_INDEX_NUMBERTOTAL = 3
  integer, parameter, public :: ASSERT_INDEX_MAX = 3

contains
  !
  ! assert_assert --
  !   Assert that the given test is true.
  !   If not .true., raise an assert error.
  !
  subroutine assert_assert ( test , message )
    implicit none
    logical         , intent(in) :: test
    character(len=*), intent(in) :: message
    call assert_error ( test , message )
  end subroutine assert_assert
  !
  ! assert_getenabled --
  !   Returns .true. if the assertions are enabled
  !
  logical function assert_getenabled ( )
    assert_getenabled = assert_enabled
  end function assert_getenabled
  !
  ! assert_setenabled --
  !   Enable (or disable) the assertion checking if the given boolean is true,
  !   (or .false.).
  !
  subroutine assert_setenabled ( bool )
    logical , intent(in) :: bool
    assert_enabled = bool
  end subroutine assert_setenabled
  !
  ! assert_fatalError --
  !   Assert that the given test is true.
  !   If not .true., raise a fatal error.
  !
  subroutine assert_fatalError ( test , message )
    implicit none
    logical         , intent(in) :: test
    character(len=*), intent(in) :: message
#ifdef _ASSERT_ENABLE
    logical :: istrue
    istrue = assert_istrue ( test )
    if (.not. istrue) then
       call exception_raiseFatalError ( message )
    endif
#else
    continue
#endif
  end subroutine assert_fatalError
  !
  ! assert_error --
  !   Assert that the given test is true.
  !   If not .true., raise an error.
  !
  subroutine assert_error ( test , message )
    implicit none
    logical         , intent(in) :: test
    character(len=*), intent(in) :: message
#ifdef _ASSERT_ENABLE
    logical :: istrue
    istrue = assert_istrue ( test )
    if (.not. istrue) then
       call exception_raiseError ( message )
    endif
#else
    continue
#endif
  end subroutine assert_error 
  !
  ! assert_warning --
  !   Assert that the given test is true.
  !   If not .true., raise a warning.
  !
  subroutine assert_warning ( test , message )
    implicit none
    logical         , intent(in) :: test
    character(len=*), intent(in) :: message
#ifdef _ASSERT_ENABLE
    logical :: istrue
    istrue = assert_istrue ( test )
    if (.not. istrue) then
       call exception_raiseWarning ( message )
    endif
#else
    continue
#endif
  end subroutine assert_warning
  !
  ! assert_information --
  !   Assert that the given test is true.
  !   If not .true., raise an information.
  !
  subroutine assert_information ( test , message )
    implicit none
    logical         , intent(in) :: test
    character(len=*), intent(in) :: message
#ifdef _ASSERT_ENABLE
    logical :: istrue
    istrue = assert_istrue ( test )
    if (.not. istrue) then
       call exception_raiseInformation ( message )
    endif
#else
    continue
#endif
  end subroutine assert_information
  !
  ! assert_failure --
  !   Assert that the given test is true.
  !   If not .true., raise a failure.
  !
  subroutine assert_failure ( test , message )
    implicit none
    logical         , intent(in) :: test
    character(len=*), intent(in) :: message
#ifdef _ASSERT_ENABLE
    logical :: istrue
    istrue = assert_istrue ( test )
    if (.not. istrue) then
       call exception_raiseFailure ( message )
    endif
#else
    continue
#endif
  end subroutine assert_failure
  !
  ! assert_countersupdate --
  !   Check if the condition is true or false and update the counters
  !
  subroutine assert_countersupdate ( test )
    logical         , intent(in) :: test
    assert_numbertotal = assert_numbertotal + 1
    if ( test ) then
       assert_numbertrue = assert_numbertrue + 1
    else
       assert_numberfalse = assert_numberfalse + 1
    endif
  end subroutine assert_countersupdate
  !
  ! assert_istrue --
  !   Returns .true. if the assert component is disabled.
  !   If the assert component is enabled, return .true. or .false.
  !   depending on the value of test.
  ! Side effects:
  !   Updates the assert counters with a call to assert_countersupdate.
  !
  logical function assert_istrue ( test )
    logical         , intent(in) :: test
    assert_istrue = .true.
#ifdef _ASSERT_ENABLE
    if ( assert_enabled ) then
       call assert_countersupdate ( test )
       if ( .not.test ) then
          assert_istrue = .false.
       endif
    endif
#endif
  end function assert_istrue
  !
  ! assert_getcounters --
  !   Fill an array of size 3 with the current values of the counters :
  !     counter ( ASSERT_INDEX_NUMBERTRUE ) : number of true assertions
  !     counter ( ASSERT_INDEX_NUMBERFALSE ) : number of false assertions
  !     counter ( ASSERT_INDEX_NUMBERTOTAL ) : number of total assertions
  !
  subroutine assert_getcounters ( counters )
    integer , dimension(1:ASSERT_INDEX_MAX) , intent(out) :: counters
    counters ( ASSERT_INDEX_NUMBERTRUE ) = assert_numbertrue
    counters ( ASSERT_INDEX_NUMBERFALSE ) = assert_numberfalse
    counters ( ASSERT_INDEX_NUMBERTOTAL ) = assert_numbertotal
  end subroutine assert_getcounters
  !
  ! assert_initcounters --
  !   Initializes the assertions counters
  !
  subroutine assert_initcounters ( )
    assert_numbertrue = 0
    assert_numberfalse = 0
    assert_numbertotal = 0
  end subroutine assert_initcounters
end module m_assert

