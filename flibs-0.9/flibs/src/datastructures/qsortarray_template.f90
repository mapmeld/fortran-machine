!
! qsortarray_template.f90 --
!     QuickSort for arrays:
!
!     This template has to be preprocessed by defining the 
!     two following macros :
!     - "_QSORTARRAY_TYPE" is the name of the derived type
!     - "_QSORTARRAY_MODULE" is the module defining the 
!       derived type
!
!     Note:
!     The data that need to be sorted are stored in
!     an array. A comparison function is used to enable
!     sorting wrt different criteria.
!     The data have the generic type _QSORTARRAY_TYPE.
!
!     Note:
!     Algorithm translated from Kernighan and Pike,
!     The Practice of Programming.
!
!     $Id: qsortarray_template.f90,v 1.1 2008/04/17 14:39:08 relaxmike Exp $
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
  type ( _QSORTARRAY_TYPE ) , dimension(:) :: array
  interface
     integer function compare(f,g)
       use _QSORTARRAY_MODULE , only : _QSORTARRAY_TYPE
       type( _QSORTARRAY_TYPE ) :: f, g
     end function compare
  end interface
  type ( _QSORTARRAY_TYPE ) , dimension(:), allocatable :: backup
  integer, dimension(:), allocatable         :: order
  integer                                    :: i
  allocate( backup(1:size(array) ) )
  allocate( order(1:size(array) ) )
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
  type ( _QSORTARRAY_TYPE ) , dimension(:) :: array
  integer, dimension(:)         :: order
  integer                       :: left
  integer                       :: right
  interface
     integer function compare ( f , g )
       use _QSORTARRAY_MODULE, only : _QSORTARRAY_TYPE
       type(_QSORTARRAY_TYPE) :: f, g
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

