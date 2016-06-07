! qsort_plain_test --
!     Test program for the plain QuickSort algorithm
!
!     See: sorting.f90 for more details
!
!     IMPORTANT NOTE:
!     --------------
!     This file is deprecated - use test_qsortarray_template.f90
!     instead
!
!     $Id: qsort_plain_test.f90,v 1.2 2008/04/21 17:45:49 arjenmarkus Exp $
!
module sorting_data
    type SORT_DATA
        real key
        integer index
    end type SORT_DATA

contains

integer function COMPARE(a,b)
    type(SORT_DATA), intent(in) :: a,b
    COMPARE = merge(1, -1, a%key < b%key )
end function COMPARE

end module sorting_data

program qsort_plain_test
    use sorting_data

    implicit none
    type(SORT_DATA) :: hold
    type(SORT_DATA), allocatable :: array(:)
    integer, parameter :: array_size = 1000000
    integer i, time_start, time_end

    interface
    end interface

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
    call qsort_array( array, compare )
    call system_clock(time_end)
    write(6,*) 'Sort time = ',time_end-time_start,' ticks'
    write(6,*) 'Sorted:'
    write(6,'(F8.6,I10)') array(1:10)
    write(6,*) ' ...'
    write(6,'(F8.6,I10)') array(array_size-9:array_size)

    stop

contains

include "sorting.f90"

end program qsort_plain_test
