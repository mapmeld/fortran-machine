! test_btree.f90 --
!     Test program for binary trees
!
! NOTES:
!     - This test program is not yet complete.
!     - A variation on the theme is to implement
!       trees where the elements can be sorted.
!       This requires a comparison function for
!       the data.
!
!     $Id: test_btree.f90,v 1.2 2006/03/26 19:03:53 arjenmarkus Exp $
!
module MYDATA_MODULE

type MYDATA
    character(len=20) :: string
end type MYDATA

end module

module MYDATA_BTREES
    use MYDATA_MODULE, TREE_DATA => MYDATA

    include "binarytree.f90"
end module MYDATA_BTREES

program test_btree
    use MYDATA_BTREES

    implicit none

    type(BINARY_TREE), pointer :: btree
    type(BINARY_TREE), pointer :: elem
    type(TREE_DATA)            :: data

    logical                    :: LEFT  = .false.
    logical                    :: RIGHT = .true.

    data%string = 'D'
    call btree_create( btree, data )

    data%string = 'A'
    call btree_append_data( btree, data, LEFT )
    data%string = 'B'
    call btree_append_data( btree, data, RIGHT )

    elem => btree_child_node( btree, LEFT )
    data%string = 'C'
    call btree_append_data( elem, data, RIGHT )

    call print_btree( '', btree )
    !
    ! Destroy the btree
    !
    call btree_destroy( btree )

contains

recursive subroutine print_btree( text, btree )
    character(len=*)            :: text
    type(BINARY_TREE), pointer  :: btree
    type(BINARY_TREE), pointer  :: elem

    type(TREE_DATA)             :: data
    integer                     :: i

    data = btree_get_data( btree )
    write(*,*) text, data%string

    elem => btree_child_node( btree, LEFT )
    if ( associated(elem) ) then
        call print_btree( text // '   ' , elem )
    endif

    elem => btree_child_node( btree, RIGHT )
    if ( associated(elem) ) then
        call print_btree( text // '   ', elem )
    endif

end subroutine print_btree
end program
