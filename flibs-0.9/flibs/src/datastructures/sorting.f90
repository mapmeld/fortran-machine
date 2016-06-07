! sorting.f90 --
!     QuickSort for arrays:
!
!     See the example/test program for the way to use this
!
!     IMPORTANT NOTE:
!     --------------
!     This file is deprecated in favour of "qsortarray_template.f90"
!     Please use this file and its test program.
!
!     The reason is that there are several interface issues
!     (the need for a use statement when defining the interface to
!     the comparison function). The current file does not even
!     compile properly.
!
!     Note:
!     The data that need to be sorted are stored in
!     an array. A comparison function is used to enable
!     sorting wrt different criteria.
!     The data have the generic type SORT_DATA.
!
!     Note:
!     Algorithm translated from Kernighan and Pike,
!     The Practice of Programming.
!
!     $Id: sorting.f90,v 1.3 2008/04/21 17:45:48 arjenmarkus Exp $
!

!
! Routines and functions specific to dictionaries
!

! qsort_array --
!     Use the QuickSort algorithm to sort an array
! Arguments:
!     array      Array to be sorted
!     compare    Comparison function
!
subroutine qsort_array( array, compare )
    type(SORT_DATA), dimension(:) :: array

    interface
        integer function compare(f,g)
            type(SORT_DATA) :: f, g
        end function compare
    end interface

    type(SORT_DATA), dimension(:), allocatable :: backup
    integer, dimension(:), allocatable         :: order
    integer                                    :: i

    allocate( backup(1:size(array)) )
    allocate( order(1:size(array)) )

    do i = 1,size(order)
        order(i) = i
    enddo

    call qsort_sort( array, order, 1, size(array), compare )

    do i = 1,size(order)
        backup(i) = array(order(i))
    enddo

    array = backup

    deallocate( backup )
    deallocate( order  )

end subroutine qsort_array

! qsort_sort --
!     Sort the array according to the QuickSort algorithm
! Arguments:
!     array      Array of data
!     order      Array holding the order
!     left       Start index
!     right      End index
!     compare    Comparison function
!
recursive subroutine qsort_sort( array, order, left, right, compare )
    type(SORT_DATA), dimension(:) :: array
    integer, dimension(:)         :: order
    integer                       :: left
    integer                       :: right

    interface
        integer function compare(f,g)
            type(SORT_DATA) :: f, g
        end function compare
    end interface

    integer                                    :: i
    integer                                    :: last

    if ( left .ge. right ) return

    call qsort_swap( order, left, qsort_rand(left,right) )
    last = left

    do i = left+1, right
        if ( compare(array(order(i)), array(order(left)) ) .lt. 0 ) then
            last = last + 1
            call qsort_swap( order, last, i )
        endif
    enddo

    call qsort_swap( order, left, last )

    call qsort_sort( array, order, left, last-1, compare )
    call qsort_sort( array, order, last+1, right, compare )

end subroutine qsort_sort

! qsort_swap --
!     Swap two array elements
! Arguments:
!     order      Array holding the order
!     first      First index
!     second     Second index
!
subroutine qsort_swap( order, first, second )
    integer, dimension(:)         :: order
    integer                       :: first, second

    integer                       :: tmp

    tmp           = order(first)
    order(first)  = order(second)
    order(second) = tmp
end subroutine

! qsort_rand --
!     Determine a random integer number
! Arguments:
!     lower      Lowest value
!     upper      Greatest value
!
integer function qsort_rand( lower, upper )
    integer                       :: lower, upper

    real                          :: r

    call random_number( r )
    qsort_rand =  lower + nint(r * (upper-lower))
end function qsort_rand

