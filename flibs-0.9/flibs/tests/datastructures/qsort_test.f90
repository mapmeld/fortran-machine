! qsort_test --
!     Test program for the combined QuickSort/InsertionSort algorithm
!     by Joe Krahn.
!
!     See: qsort_inline.f90 for more details
!
!     $Id: qsort_test.f90,v 1.1 2008/04/15 04:25:30 arjenmarkus Exp $
!
program qsort_test
  implicit none
  type DATA_TYPE
    real key
    integer index
  end type DATA_TYPE
  type(DATA_TYPE) :: hold
  type(DATA_TYPE), allocatable :: array(:)
  integer, parameter :: QSORT_THRESHOLD = 96
  integer, parameter :: array_size = 1000000
  integer i, time_start, time_end

  allocate(array(array_size))
  do i=1,array_size
    !array(i)%key = random()
    call random_number(array(i)%key)
    array(i)%index = i
  end do

  write(6,*) 'Unsorted:'
  write(6,'(F8.6,I10)') array(1:10)
  write(6,*) ' ...'
  write(6,'(F8.6,I10)') array(array_size-9:array_size)
  call system_clock(time_start)
  call qsort_data()
  call system_clock(time_end)
  write(6,*) 'Sort time = ',time_end-time_start,' ticks'
  write(6,*) 'Sorted:'
  write(6,'(F8.6,I10)') array(1:10)
  write(6,*) ' ...'
  write(6,'(F8.6,I10)') array(array_size-9:array_size)

  stop

contains

  subroutine QSORT_DATA
    include "qsort_inline.f90"
  end subroutine QSORT_DATA

  logical function QSORT_COMPARE(a,b)
    integer, intent(in) :: a,b
    QSORT_COMPARE = ( array(a)%key < array(b)%key )
  end function QSORT_COMPARE

end program qsort_test
