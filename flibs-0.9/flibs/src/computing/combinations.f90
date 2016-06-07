! combinations.f90 --
!     Module for some simple combinatorical functions and subroutines
!
!     $Id: combinations.f90,v 1.1 2008/09/21 10:42:33 arjenmarkus Exp $
!
module combinations
    implicit none


contains

! random_permutation --
!     Compute a random permutation of N integers
! Arguments:
!     array    Integer array that will be filled with integers 1 to N in
!              random order
!
subroutine random_permutation( array )
    integer, dimension(:) :: array

    integer               :: i
    integer               :: j
    integer               :: n
    integer               :: t
    real                  :: r

    do i = 1,size(array)
        array(i) = i
    enddo

    n = size(array)
    do i = 1,size(array)-1
       n = n - 1
       call random_number( r )
       j = 1 + n * r
       if ( j >= n+1 ) j = 1

       t          = array(i+j)
       array(i+j) = array(i)
       array(i)   = t
    enddo
end subroutine random_permutation

end module combinations

program tst_comb
    use combinations
    integer, dimension(10) :: array
    integer                :: i

    !
    ! TODO: simple statistics to find out if
    ! the permutations are "honest"
    do i = 1,30
        call random_permutation( array )
        write(*,'(1x,10i4)') array
    enddo
end program
