! binarytree.f90 --
!     Include file for defining binary trees where each element holds
!     the same kind of data
!
!     See the example/test program for the way to use this
!
!     A node in the binary tree is itself a binary tree, there
!     is no distinction.
!
!     Note:
!     You should only use pointer variables of this type, no
!     ordinary variables, as sometimes the memory pointed to
!     will be deallocated. The subroutines and functions
!     are designed to minimize mistakes (for instance: using
!     = instead of =>)
!
!     $Id: binarytree.f90,v 1.2 2006/03/26 19:03:53 arjenmarkus Exp $
!
! Define the binary-tree data type
!
type BINARY_TREE
    type(BINARY_TREE), pointer :: left
    type(BINARY_TREE), pointer :: right
    type(TREE_DATA)            :: data
end type BINARY_TREE

!
! Define the subroutines and functions
!
contains

! btree_create --
!     Create and initialise a binary tree
! Arguments:
!     btree      Pointer to new binary tree
!     data       The data for the first element
! Note:
!     This version assumes a shallow copy is enough
!     (that is, there are no pointers within the data
!     to be stored)
!     It also assumes the argument btree does not already
!     refer to a btree. Use btree_destroy first to
!     destroy up an old btree.
!
subroutine btree_create( btree, data )
    type(BINARY_TREE), pointer  :: btree
    type(TREE_DATA), intent(in) :: data

    allocate( btree )
    btree%left  => null()
    btree%right => null()
    btree%data  =  data
end subroutine btree_create

! btree_destroy --
!     Destroy an entire btree
! Arguments:
!     btree       Pointer to the btree to be destroyed
! Note:
!     This version assumes that there are no
!     pointers within the data that need deallocation
!
recursive subroutine btree_destroy( btree )
    type(BINARY_TREE), pointer  :: btree

    type(BINARY_TREE), pointer  :: left
    type(BINARY_TREE), pointer  :: right

    left  => btree%left
    right => btree%right

    if ( associated(left) ) then
        call btree_destroy( left )
    endif

    if ( associated(right) ) then
        call btree_destroy( right )
    endif
    deallocate( btree )
end subroutine btree_destroy

! btree_count --
!     Count the number of nodes in the btree
! Arguments:
!     btree       Pointer to the btree
!
recursive function btree_count( btree ) result(count)
    type(BINARY_TREE), pointer  :: btree
    integer                     :: count

    if ( associated(btree) ) then
        count = 1 + btree_count(btree%left) + btree_count(btree%right)
    else
        count = 0
    endif
end function btree_count

! btree_child_node
!     Return the left or right child node
! Arguments:
!     btree      Some node in the tree
!     right      Return the right node if true,
!                otherwise the left node
! Result:
!
function btree_child_node( btree, right ) result(child)
    type(BINARY_TREE), pointer :: btree
    logical, intent(in)        :: right

    type(BINARY_TREE), pointer :: child

    if ( right ) then
       child => btree %right
    else
       child => btree %left
    endif

end function btree_child_node

! btree_append_data
!     Append a new node (left or right) if
!     possible. Otherwise nothing is done
! Arguments:
!     btree      Some node in the tree
!     data       The data for the new node
!     right      Append right or left
!
subroutine btree_append_data( btree, data, right )
    type(BINARY_TREE), pointer  :: btree
    type(TREE_DATA), intent(in) :: data
    logical, intent(in)        :: right

    type(BINARY_TREE), pointer :: new

    if ( right .and. associated(btree%right) ) then
        return
    endif
    if ( .not. right .and. associated(btree%left) ) then
        return
    endif

    call btree_create(new, data )

    if ( right ) then
        btree%right => new
    else
        btree%left  => new
    endif
end subroutine btree_append_data

! btree_append_subtree
!     Append a new subtree (left or right) if
!     possible. Otherwise nothing is done
! Arguments:
!     btree      Some node in the tree
!     subtree    Subtree to be appended
!     right      Append right or left
!
subroutine btree_append_subtree( btree, subtree, right )
    type(BINARY_TREE), pointer  :: btree
    type(BINARY_TREE), pointer  :: subtree
    logical, intent(in)        :: right

    if ( right .and. associated(btree%right) ) then
        return
    endif
    if ( .not. right .and. associated(btree%left) ) then
        return
    endif

    if ( right ) then
        btree%right => subtree
    else
        btree%left  => subtree
    endif
end subroutine btree_append_subtree

! btree_remove_subtree
!     Remove the subtree on the left or right) if
!     possible. Otherwise nothing is done
! Arguments:
!     btree      Some node in the tree
!     subtree    Subtree that was removed (returned!)
!     right      Append right or left
!
subroutine btree_remove_subtree( btree, subtree, right )
    type(BINARY_TREE), pointer  :: btree
    type(BINARY_TREE), pointer  :: subtree
    logical, intent(in)        :: right

    subtree => null()

    if ( right .and. associated(btree%right) ) then
        return
    endif
    if ( .not. right .and. associated(btree%left) ) then
        return
    endif

    if ( right ) then
        subtree => btree%right
        btree%right => null()
    else
        subtree => btree%left
        btree%left => null()
    endif
end subroutine btree_remove_subtree

! btree_get_data
!     Get the data stored with a tree node
! Arguments:
!     node       Tree node
!
function btree_get_data( node ) result(data)
    type(BINARY_TREE), pointer :: node

    type(TREE_DATA)            :: data

    data = node%data
end function btree_get_data

! btree_put_data
!     Store new data with a tree node
! Arguments:
!     node       Tree node
!     data       The data to be stored
!
subroutine btree_put_data( node, data )
    type(BINARY_TREE), pointer  :: node
    type(TREE_DATA), intent(in) :: data

    node%data = data
end subroutine btree_put_data

