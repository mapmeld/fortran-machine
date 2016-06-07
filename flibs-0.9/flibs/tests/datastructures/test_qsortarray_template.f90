module m_specialdata
  type MY_SPECIAL_DATA
     integer key
     integer index
  end type MY_SPECIAL_DATA
contains
  integer function QSORT_COMPARE (a,b)
    type(MY_SPECIAL_DATA), intent(in) :: a,b
    if ( a%key < b%key ) then
       QSORT_COMPARE = -1
    elseif ( a%key == b%key ) then
       QSORT_COMPARE = 0
    else
       QSORT_COMPARE = 1
    endif
  end function QSORT_COMPARE

end module m_specialdata

module m_testsorting
  use m_specialdata
contains

#define _QSORTARRAY_TYPE MY_SPECIAL_DATA
#define _QSORTARRAY_MODULE m_specialdata
#include "qsortarray_template.f90"

end module m_testsorting

! test_qsortarray_template --
!     Test program for sorting.f90.
!
!     See: sorting.f90 for more details
!
!     $Id: test_qsortarray_template.f90,v 1.1 2008/04/17 14:43:56 relaxmike Exp $
!
program test_qsortarray_template
  use m_specialdata, only : MY_SPECIAL_DATA , QSORT_COMPARE
  use m_testsorting, only : qsort_array
  implicit none
  integer :: assertTotalTestSuccess
  integer :: assertTotalTestFail
  integer :: assertTestIndex
  integer , parameter :: log_unit = 12
  call test_main ()
contains
  !
  ! Include support for unit tests.
  !
  include "test_support.f90"
  !
  ! test_main --
  !   The main subroutine for the tests.
  !
  subroutine test_main ()
    
    call log_startup ( "test_qsortarray_template.log" )
    call assert_startup ( )
    
    call test_sort_size2 ()
    call test_sort_size3 ()
    call test_sort_size4 ()
    call test_sort_size5 ()
    call test_sort_size6 ()
    call test_sort_perf ()
    
    !
    ! Shutdown the tests
    !
    call logmsg ( "Test suite completed" )
    call assert_shutdown ()
    call log_shutdown ()
    
  end subroutine test_main
  !
  ! test_sort_size6 --
  !   Sort an array of size 6.
  !
  subroutine test_sort_size6
    type(MY_SPECIAL_DATA), pointer :: array(:)
    integer, parameter :: array_size = 6
    integer i
    
    allocate(array(array_size))
    i = 0
    ! Compute a constant array to sort
    ! #1
    i = i + 1
    array(i)%key = 5
    array(i)%index = i
    ! #2
    i = i + 1
    array(i)%key = 4
    array(i)%index = i
    ! #3
    i = i + 1
    array(i)%key = 7
    array(i)%index = i
    ! #4
    i = i + 1
    array(i)%key = 1
    array(i)%index = i
    ! #5
    i = i + 1
    array(i)%key = 8
    array(i)%index = i
    ! #6
    i = i + 1
    array(i)%key = 12
    array(i)%index = i
    
    call qsort_array ( array, QSORT_COMPARE )
    !
    ! Check array content
    !
    call logmsg ( "Checking array of size 6" )
    call assert ( array(1)%key == 1 , "Wrong array value." )
    call assert ( array(2)%key == 4 , "Wrong array value." )
    call assert ( array(3)%key == 5 , "Wrong array value." )
    call assert ( array(4)%key == 7 , "Wrong array value." )
    call assert ( array(5)%key == 8 , "Wrong array value." )
    call assert ( array(6)%key == 12 , "Wrong array value." )
    
  end subroutine test_sort_size6
  !
  ! test_sort_size5 --
  !   Sort an array of size 5.
  !
  subroutine test_sort_size5
    type(MY_SPECIAL_DATA), pointer :: array(:)
    integer, parameter :: array_size = 5
    integer i
    
    allocate(array(array_size))
    i = 0
    ! Compute a constant array to sort
    ! #1
    i = i + 1
    array(i)%key = 5
    array(i)%index = i
    ! #2
    i = i + 1
    array(i)%key = 4
    array(i)%index = i
    ! #3
    i = i + 1
    array(i)%key = 7
    array(i)%index = i
    ! #4
    i = i + 1
    array(i)%key = 1
    array(i)%index = i
    ! #5
    i = i + 1
    array(i)%key = 8
    array(i)%index = i
    
    call qsort_array ( array, QSORT_COMPARE )
    !
    ! Check array content
    !
    call logmsg ( "Checking array of size 5" )
    call assert ( array(1)%key == 1 , "Wrong array value." )
    call assert ( array(2)%key == 4 , "Wrong array value." )
    call assert ( array(3)%key == 5 , "Wrong array value." )
    call assert ( array(4)%key == 7 , "Wrong array value." )
    call assert ( array(5)%key == 8 , "Wrong array value." )
    
  end subroutine test_sort_size5
  !
  ! test_sort_size4 --
  !   Sort an array of size 4.
  !
  subroutine test_sort_size4
    type(MY_SPECIAL_DATA), pointer :: array(:)
    integer, parameter :: array_size = 4
    integer i
    
    allocate(array(array_size))
    i = 0
    ! Compute a constant array to sort
    ! #1
    i = i + 1
    array(i)%key = 5
    array(i)%index = i
    ! #2
    i = i + 1
    array(i)%key = 4
    array(i)%index = i
    ! #3
    i = i + 1
    array(i)%key = 7
    array(i)%index = i
    ! #4
    i = i + 1
    array(i)%key = 1
    array(i)%index = i
    
    call qsort_array ( array, QSORT_COMPARE )
    !
    ! Check array content
    !
    call logmsg ( "Checking array of size 4" )
    call assert ( array(1)%key == 1 , "Wrong array value." )
    call assert ( array(2)%key == 4 , "Wrong array value." )
    call assert ( array(3)%key == 5 , "Wrong array value." )
    call assert ( array(4)%key == 7 , "Wrong array value." )
    
  end subroutine test_sort_size4
  !
  ! test_sort_size3 --
  !   Sort an array of size 3.
  !
  subroutine test_sort_size3
    type(MY_SPECIAL_DATA), pointer :: array(:)
    integer, parameter :: array_size = 3
    integer i
    
    allocate(array(array_size))
    i = 0
    ! Compute a constant array to sort
    ! #1
    i = i + 1
    array(i)%key = 5
    array(i)%index = i
    ! #2
    i = i + 1
    array(i)%key = 4
    array(i)%index = i
    ! #3
    i = i + 1
    array(i)%key = 7
    array(i)%index = i
    
    call qsort_array ( array, QSORT_COMPARE )
    !
    ! Check array content
    !
    call logmsg ( "Checking array of size 3" )
    call assert ( array(1)%key == 4 , "Wrong array value." )
    call assert ( array(2)%key == 5 , "Wrong array value." )
    call assert ( array(3)%key == 7 , "Wrong array value." )
    
  end subroutine test_sort_size3
  !
  ! test_sort_size2 --
  !   Sort an array of size 2.
  !
  subroutine test_sort_size2
    type(MY_SPECIAL_DATA), pointer :: array(:)
    integer, parameter :: array_size = 2
    integer i
    
    allocate(array(array_size))
    i = 0
    ! Compute a constant array to sort
    ! #1
    i = i + 1
    array(i)%key = 5
    array(i)%index = i
    ! #2
    i = i + 1
    array(i)%key = 4
    array(i)%index = i
    
    call qsort_array ( array, QSORT_COMPARE )
    !
    ! Check array content
    !
    call logmsg ( "Checking array of size 3" )
    call assert ( array(1)%key == 4 , "Wrong array value." )
    call assert ( array(2)%key == 5 , "Wrong array value." )
    
  end subroutine test_sort_size2
  !
  ! test_sort_perf --
  !   Test the performances of the sorting method.
  !
  subroutine test_sort_perf
    type(MY_SPECIAL_DATA), pointer :: array(:)
    !integer, parameter :: array_size = 1000000
    integer, parameter :: array_size = 1000
    integer i, time_start, time_end
    real :: randomvalue
    
    allocate(array(array_size))
    !
    ! Randomized array
    !
    do i=1,array_size
       call random_number( randomvalue )
       array(i)%key  = int ( randomvalue * 1000 )
       array(i)%index = i
    end do
    
    write(6,*) 'Unsorted:'
    write(6,'(I10,I10)') array(1:10)
    write(6,*) ' ...'
    write(6,'(I10,I10)') array(array_size-9:array_size)
    call system_clock(time_start)
    call qsort_array ( array, QSORT_COMPARE )
    call system_clock(time_end)
    write(6,*) 'Sort time = ',time_end-time_start,' ticks'
    write(6,'(I10,I10)') array(1:10)
    write(6,*) ' ...'
    write(6,'(I10,I10)') array(array_size-9:array_size)
    
  end subroutine test_sort_perf
  
  
end program test_qsortarray_template

