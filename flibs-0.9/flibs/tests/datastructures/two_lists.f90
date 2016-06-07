! two_lists.f90 --
!     Program illustrating the use of several types of lists
!     in one program
!
!     $Id: two_lists.f90,v 1.2 2006/03/26 19:03:53 arjenmarkus Exp $
!
! Convenience module, holding all the data types
!
module DATA_MODULE

type REAL_DATA
    real :: value
end type REAL_DATA

type STRING_DATA
    character(len=20) :: string
end type STRING_DATA

end module DATA_MODULE

!
! Modules defining the individal list types
!
module REAL_LISTS
    use DATA_MODULE, ONLY: LIST_DATA => REAL_DATA

    include "linkedlist.f90"
end module REAL_LISTS

module STRING_LISTS
    use DATA_MODULE, ONLY: LIST_DATA => STRING_DATA

    include "linkedlist.f90"
end module STRING_LISTS

!
! Module uniting the two, so that we can use
! a single set of routines
!
module TWO_LIST_TYPES
    use REAL_LISTS, only: REAL_LINKED_LIST   => LINKED_LIST,   &
                          REAL_LIST_DATA     => LIST_DATA,     &
                          list_create_real   => list_create,   &
                          list_insert_real   => list_insert,   &
                          list_next_real     => list_next,     &
                          list_get_real_data => list_get_data, &
                          list_put_real_data => list_put_data, &
                          list_delete_real   => list_delete_element, &
                          list_destroy_real  => list_destroy

    use STRING_LISTS, only: STRING_LINKED_LIST   => LINKED_LIST,   &
                          STRING_LIST_DATA       => LIST_DATA,     &
                          list_create_string     => list_create,   &
                          list_insert_string     => list_insert,   &
                          list_next_string       => list_next,     &
                          list_get_string_data   => list_get_data, &
                          list_put_string_data   => list_put_data, &
                          list_delete_string     => list_delete_element, &
                          list_destroy_string    => list_destroy
    private
    public :: list_create, list_destroy, list_get_data, list_put_data, &
              list_insert, list_next, list_delete_string
    public :: REAL_LINKED_LIST, STRING_LINKED_LIST
    public :: REAL_LIST_DATA, STRING_LIST_DATA

    interface list_create
        module procedure list_create_real
        module procedure list_create_string
    end interface

    interface list_destroy
        module procedure list_destroy_real
        module procedure list_destroy_string
    end interface

    interface list_insert
        module procedure list_insert_real
        module procedure list_insert_string
    end interface

    interface list_next
        module procedure list_next_real
        module procedure list_next_string
    end interface

    interface list_get_data
        module procedure list_get_real_data
        module procedure list_get_string_data
    end interface

    interface list_put_data
        module procedure list_put_real_data
        module procedure list_put_string_data
    end interface

    interface list_delete_element
        module procedure list_delete_real
        module procedure list_delete_string
    end interface
end module TWO_LIST_TYPES

program test_list
    use TWO_LIST_TYPES

    implicit none

    type(REAL_LINKED_LIST), pointer :: real_list
    type(REAL_LINKED_LIST), pointer :: real_elem
    type(REAL_LIST_DATA)            :: real_data

    type(STRING_LINKED_LIST), pointer :: string_list
    type(STRING_LINKED_LIST), pointer :: string_elem
    type(STRING_LIST_DATA)            :: string_data

    !
    ! Add elements at the start (so we get: D, C, B, A)
    ! Elements are always added after the given element ...
    !
    string_data%string = 'D'
    call list_create( string_list, string_data )

    string_data%string = 'A'
    call list_insert( string_list, string_data )
    string_data%string = 'B'
    call list_insert( string_list, string_data )
    string_data%string = 'C'
    call list_insert( string_list, string_data )

    call print_list_string( 'Expected: D, C, B, A', string_list )

    !
    ! Do the same for a list of reals
    !
    real_data%value = 1.0
    call list_create( real_list, real_data )
    real_data%value = 2.0
    call list_insert( real_list, real_data )
    real_data%value = 3.0
    call list_insert( real_list, real_data )

    call print_list_real( 'Expected: 1.0, 3.0, 2.0 (insertion after the head)', real_list )

contains

subroutine print_list_string( text, list )
    character(len=*)                   :: text
    type(STRING_LINKED_LIST), pointer  :: list

    type(STRING_LINKED_LIST), pointer  :: elem
    type(STRING_LIST_DATA)             :: data
    integer                            :: i

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

end subroutine print_list_string

subroutine print_list_real( text, list )
    character(len=*)                 :: text
    type(REAL_LINKED_LIST), pointer  :: list

    type(REAL_LINKED_LIST), pointer  :: elem
    type(REAL_LIST_DATA)             :: data
    integer                          :: i

    write(*,*) text
    !
    ! Loop over the list
    !
    i = 0
    elem => list
    do while( associated(elem) )
        data = list_get_data(elem)
        write(*,*) i, data%value
        elem => list_next(elem)
        i = i + 1
    enddo

end subroutine print_list_real

end program
