! linkedlist.f90 --
!     Include file for defining linked lists where each element holds
!     the same kind of data
!
!     See the example/test program for the way to use this
!
!     Note:
!     You should only use pointer variables of this type, no
!     ordinary variables, as sometimes the memory pointed to
!     will be deallocated. The subroutines and functions
!     are designed to minimize mistakes (for instance: using
!     = instead of =>)
!
!     $Id: linkedlist.f90,v 1.3 2007/01/26 09:56:43 arjenmarkus Exp $
!
! Define the linked-list data type
!
type LINKED_LIST
    type(LINKED_LIST), pointer :: next
    type(LIST_DATA)            :: data
end type LINKED_LIST

!
! define a private (!) interface to prevent
! mistakes with ordinary assignment
!
!interface assignment(=)
!    module procedure list_assign
!end interface
!private :: list_assign

!
! Define the subroutines and functions
!
contains

! list_assign
!     Subroutine to prevent errors with assignment
! Arguments:
!     list_left   List on the left-hand side
!     list_right  List on the right-hand side
!
! NOTE:
!     This does not work because of a private/public
!     conflict
!
!subroutine list_assign( list_left, list_right )
!    type(LINKED_LIST), INTENT(OUT)  :: list_left
!    type(LINKED_LIST), INTENT(IN)   :: list_right
!   !type(LINKED_LIST), pointer      :: list_left
!   !type(LINKED_LIST), pointer      :: list_right
!
!    !
!    ! Note the order!
!    !
!    stop 'Error: ordinary assignment for lists'
!    list_left%next => null()
!end subroutine list_assign

! list_create --
!     Create and initialise a list
! Arguments:
!     list       Pointer to new linked list
!     data       The data for the first element
! Note:
!     This version assumes a shallow copy is enough
!     (that is, there are no pointers within the data
!     to be stored)
!     It also assumes the argument list does not already
!     refer to a list. Use list_destroy first to
!     destroy up an old list.
!
subroutine list_create( list, data )
    type(LINKED_LIST), pointer  :: list
    type(LIST_DATA), intent(in) :: data

    allocate( list )
    list%next => null()
    list%data =  data
end subroutine list_create

! list_destroy --
!     Destroy an entire list
! Arguments:
!     list       Pointer to the list to be destroyed
! Note:
!     This version assumes that there are no
!     pointers within the data that need deallocation
!
subroutine list_destroy( list )
    type(LINKED_LIST), pointer  :: list

    type(LINKED_LIST), pointer  :: current
    type(LINKED_LIST), pointer  :: next

    current => list
    do while ( associated(current%next) )
        next => current%next
        deallocate( current )
        current => next
    enddo
end subroutine list_destroy

! list_count --
!     Count the number of items in the list
! Arguments:
!     list       Pointer to the list
!
integer function list_count( list )
    type(LINKED_LIST), pointer  :: list

    type(LINKED_LIST), pointer  :: current
    type(LINKED_LIST), pointer  :: next

    if ( associated(list) ) then
        list_count = 1
        current => list
        do while ( associated(current%next) )
            current => current%next
            list_count = list_count + 1
        enddo
    else
        list_count = 0
    endif
end function list_count

! list_next
!     Return the next element (if any)
! Arguments:
!     elem       Element in the linked list
! Result:
!
function list_next( elem ) result(next)
    type(LINKED_LIST), pointer :: elem
    type(LINKED_LIST), pointer :: next

    next => elem%next

end function list_next

! list_insert
!     Insert a new element
! Arguments:
!     elem       Element in the linked list after
!                which to insert the new element
!     data       The data for the new element
!
subroutine list_insert( elem, data )
    type(LINKED_LIST), pointer  :: elem
    type(LIST_DATA), intent(in) :: data

    type(LINKED_LIST), pointer :: next

    allocate(next)

    next%next => elem%next
    elem%next => next
    next%data =  data
end subroutine list_insert

! list_insert_head
!     Insert a new element before the first element
! Arguments:
!     list       Start of the list
!     data       The data for the new element
!
subroutine list_insert_head( list, data )
    type(LINKED_LIST), pointer  :: list
    type(LIST_DATA), intent(in) :: data

    type(LINKED_LIST), pointer :: elem

    allocate(elem)
    elem%data =  data

    elem%next => list
    list      => elem
end subroutine list_insert_head

! list_delete_element
!     Delete an element from the list
! Arguments:
!     list       Header of the list
!     elem       Element in the linked list to be
!                removed
!
subroutine list_delete_element( list, elem )
    type(LINKED_LIST), pointer  :: list
    type(LINKED_LIST), pointer  :: elem

    type(LINKED_LIST), pointer  :: current
    type(LINKED_LIST), pointer  :: prev

    if ( associated(list,elem) ) then
        list => elem%next
        deallocate( elem )
    else
        current => list
        prev    => list
        do while ( associated(current) )
            if ( associated(current,elem) ) then
                prev%next => current%next
                deallocate( current ) ! Is also "elem"
                exit
            endif
            prev    => current
            current => current%next
        enddo
    endif
!    allocate(next)
!
!    next%next => elem%next
!    elem%next => next
!    next%data =  data
end subroutine list_delete_element

! list_get_data
!     Get the data stored with a list element
! Arguments:
!     elem       Element in the linked list
!
function list_get_data( elem ) result(data)
    type(LINKED_LIST), pointer :: elem

    type(LIST_DATA)            :: data

    data = elem%data
end function list_get_data

! list_put_data
!     Store new data with a list element
! Arguments:
!     elem       Element in the linked list
!     data       The data to be stored
!
subroutine list_put_data( elem, data )
    type(LINKED_LIST), pointer  :: elem
    type(LIST_DATA), intent(in) :: data

    elem%data = data
end subroutine list_put_data

