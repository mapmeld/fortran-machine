! test_list.f90 --
!     Test program for linked lists
!
!     $Id: test_list.f90,v 1.3 2006/03/26 19:03:53 arjenmarkus Exp $
!
module MYDATA_MODULE

type MYDATA
    character(len=20) :: string
end type MYDATA

end module

module MYDATA_LISTS
    use MYDATA_MODULE, LIST_DATA => MYDATA

    include "linkedlist.f90"
end module MYDATA_LISTS

program test_list
    use MYDATA_LISTS

    implicit none

    type(LINKED_LIST), pointer :: list
    type(LINKED_LIST), pointer :: elem
    type(LIST_DATA)            :: data

    !
    ! Express error: does the prevention scheme work?
    !
    ! data%string = 'Error!'
    ! call list_create( list, data )
    ! elem = list

    !
    ! Add elements at the start (so we get: D, C, B, A)
    ! Elements are always added after the given element ...
    !
    data%string = 'D'
    call list_create( list, data )

    data%string = 'A'
    call list_insert( list, data )
    data%string = 'B'
    call list_insert( list, data )
    data%string = 'C'
    call list_insert( list, data )

    call print_list( 'Expected: D, C, B, A', list )

    !
    ! Now insert an element after the second element
    !
    elem => list_next(list)
    data%string = 'ZZ'
    call list_insert( elem, data )

    call print_list( 'Expected: D, C, ZZ, B, A', list )

    !
    ! Now replace the data for element 1
    !
    data%string = 'WWW'
    call list_put_data( list, data ) ! list refers to the first element

    call print_list( 'Expected: WWW, C, ZZ, B, A', list )

    !
    ! Now remove element 3 (note: use => here!)
    !
    elem => list_next( list )
    elem => list_next( elem )
    call list_delete_element( list, elem )

    call print_list( 'Expected: WWW, C, B, A', list )

    !
    ! Number of elements
    !
    write(*,*) 'Number of elements (expected 4): ', list_count(list)

    !
    ! Destroy the list
    !
    call list_destroy( list )

contains

subroutine print_list( text, list )
    character(len=*)            :: text
    type(LINKED_LIST), pointer  :: list

    type(LIST_DATA)             :: data
    integer                     :: i

    write(*,*) text
    !
    ! Loop over the list
    !
    i = 0
    elem => list
    do while( associated(elem) )
        data = list_get_data(elem)
        write(*,*) i, data%string
        elem => list_next(elem)
        i = i + 1
    enddo

end subroutine print_list
end program
