! Fast inline QSORT for Fortran.
!
! $Id: qsort_inline.f90,v 1.1 2008/04/15 04:24:34 arjenmarkus Exp $
!
! Sorts a one-dimensional array; normally of a derived-type containing a sort key.
!
! Uses a optimized combination of QSORT and INSERTION sorting.
! The algorithm is based on code used in GLIBC.
!
! Using a multidimensional allocatable array to hold multiple data columns is VERY slow.
! It is better to sort a derived type of key/index pairs, then reorder data using the sorted indices.
!
!
! You must define the function QSORT_COMPARE(), and the integer QSORT_THRESHOLD
!
! logical function QSORT_COMPARE(a,b)
! Must be an internal function -- it accesses array() from the parent.
! Should return the logical result of (array(a)%key < array(b)%key), assuming
! an ascending order sort.
!
! QSORT_THRESHOLD:
! The QSORT is used down to the QSORT_THRESHOLD size sorted blocks. Then
! insertion sort is used for the remainder, because it is faster for small sort ranges.
!
! Large sorting elements or small cache may make a smaller threshold more useful.
! You can also set this to a run-time argument/variable with no performance loss.
!
! EXAMPLE:
!
! subroutine QSORT_DATA(array)
!   implicit none
!   type(DATA_TYPE) :: hold, array(:)
!   integer, parameter :: QSORT_THRESHOLD = 96
!   include "qsort_inline.inc"
! contains
!   logical function QSORT_COMPARE(a,b)
!     SQORT_COMPARE = ( array(a)%key < array(b)%key )
!   end function QSORT_COMPARE
! end subroutine QSORT_DATA

  integer :: stack_top, right_size,left_size
  integer :: mid, left, right, low, high

! A stack of 32 can handle the entire extent of a 32-bit index.
! Use 64 for 64-bit indexed arrays, if they might contains more than 2^32 elements.
  integer, parameter :: QSORT_STACK_SIZE = 32
  type qsort_stack; integer :: low, high; end type
  type(qsort_stack) :: stack(QSORT_STACK_SIZE)

  if (size(array,1) > QSORT_THRESHOLD) then
    low = lbound(array,1)
    high = ubound(array,1)
    stack_top = 0
    QSORT_LOOP: do
      mid = (low + high)/2
      if (QSORT_COMPARE (mid, low)) then
        hold=array(mid);array(mid)=array(low);array(low)=hold; ! SWAP(mid,low)
      end if
      if (QSORT_COMPARE (high, mid)) then
        hold=array(high);array(high)=array(mid);array(mid)=hold; ! SWAP(high,low)
        if (QSORT_COMPARE (mid, low)) then
          hold=array(mid);array(mid)=array(low);array(low)=hold; ! SWAP(mid,low)
        end if
      end if
      left  = low + 1
      right = high - 1

      COLLAPSE_WALLS: do
        do while (QSORT_COMPARE (left, mid)); left=left+1; end do
        do while (QSORT_COMPARE (mid, right)); right=right-1; end do
        if (left < right) then
          hold=array(left);array(left)=array(right);array(right)=hold; ! SWAP(left,right)
          if (mid == left) then
            mid = right
          else if (mid == right) then
            mid = left
          end if
          left=left+1
          right=right-1
        else
          if (left == right) then
            left=left+1
            right=right-1
          end if
          exit COLLAPSE_WALLS
        end if
      end do COLLAPSE_WALLS

! Set up indices for the next iteration.
! Determine left and right partition sizes.
! Defer partitions smaller than the QSORT_THRESHOLD.
! If both partitions are significant, push the larger one onto the stack.
      right_size = right - low
      left_size = high - left
      if (right_size <= QSORT_THRESHOLD) then
        if (left_size <= QSORT_THRESHOLD) then
          ! Ignore both small partitions: Pop a partition or exit.
          if (stack_top<1) exit QSORT_LOOP
          low=stack(stack_top)%low; high=stack(stack_top)%high
          stack_top=stack_top-1
        else
          ! Ignore small left partition.
          low = left
        end if
      else if (left_size <= QSORT_THRESHOLD) then
        ! Ignore small right partition.
        high = right
      else if (right_size > left_size) then
        ! Push larger left partition indices.
        stack_top=stack_top+1
        stack(stack_top)=qsort_stack(low,right)
        low = left
      else
        ! Push larger right partition indices.
        stack_top=stack_top+1
        stack(stack_top)=qsort_stack(left,high)
        high = right
      end if
    end do QSORT_LOOP
  end if

!  Sort the remaining small partitions using insertion sort, which
!  should be faster for partitions smaller than the appropriate QSORT_THRESHOLD.

! First, find smallest element in first QSORT_THRESHOLD and place it at the
! array's beginning.  This is the smallest array element,
! and the operation speeds up insertion sort's inner loop.
  low = lbound(array,1)
  high = ubound(array,1)
  left=low
  do right = low+1, MIN(low+QSORT_THRESHOLD,high)
    if (QSORT_COMPARE(right,left)) left=right
  end do
  if (left/=low) then
    hold=array(left);array(left)=array(low);array(low)=hold; ! SWAP(left,low)
  end if

! Insertion sort, rightning from left-hand-side
! up to right-hand-side.
  INSERTION_SORT: do right = low+2,high
    left=right-1
    if (QSORT_COMPARE(right,left)) then
      do while (QSORT_COMPARE (right, left-1)); left=left-1; end do
      hold=array(right)
      array(left+1:right)=array(left:right-1)
      array(left)=hold
    end if
  end do INSERTION_SORT

