! sets.f90 --
!     Implementation of unordered sets
!
!     See the example/test program for the way to use this
!
!     The module is straightforward: it uses the vector
!     data structure to store the data. The operations
!     are the typical mathematical set operations,
!     with operators defined for convenience.
!
!     Note:
!     Define the type SET_DATA for the data to be
!     maintained via this module.
!
!     Note:
!     To allow maximum freedom in chosing the names of
!     the modules the code is split in two parts:
!     - an include file to define the VECTOR data type
!       (see data_for_sets.f90)
!     - this file to define the SET data type itself.
!     The test/example program shows how to use them.
!
!     $Id: sets.f90,v 1.2 2006/03/26 19:03:53 arjenmarkus Exp $

!
! Make everything private, except what we want to
! expose
!
implicit none
private

type SET
    type(VECTOR) :: values
endtype

public :: SET, SET_DATA
public :: set_create, set_destroy, set_add, set_haselement, &
          set_union, set_intersection, set_exclusion, &
          set_hassubset, set_size, set_equal, set_delete_element, &
          operator(.elementof.), &
          operator(.union.), &
          operator(.intersect.), &
          operator(.exclude.), &
          operator(.subsetof.), &
          operator(.eq.), &
          operator(.ne.)

interface operator(.elementof.)
    module procedure set_iselement
end interface

interface operator(.subsetof.)
    module procedure set_issubset
end interface

interface operator(.union.)
    module procedure set_union
end interface

interface operator(.intersect.)
    module procedure set_intersection
end interface

interface operator(.exclude.)
    module procedure set_exclusion
end interface

interface operator(.eq.)
    module procedure set_equal
end interface

interface operator(.ne.)
    module procedure set_notequal
end interface

contains
!
! Code specific for the set data structure
!

! set_create --
!     Create and initialise a set
! Arguments:
!     dataset    New set
!
subroutine set_create( dataset )
    type(SET), intent(inout) :: dataset

    call vector_create( dataset%values )

end subroutine set_create

! set_destroy --
!     Destroy a set
! Arguments:
!     dataset    Existing set
!
subroutine set_destroy( dataset )
    type(SET), intent(inout) :: dataset

    call vector_destroy( dataset%values )

end subroutine set_destroy

! set_size --
!     Function to return the size of the set
! Arguments:
!     dataset    Existing set
!
integer function set_size( dataset )
    type(SET), intent(in) :: dataset

    set_size = vector_size( dataset%values )

end function set_size

! set_haselement --
!     Function to check if an element is contained
!     in the set
! Arguments:
!     dataset    Existing set
!     elem       Element to be checked
!
logical function set_haselement( dataset, elem )
    type(SET), intent(in)      :: dataset
    type(SET_DATA), intent(in) :: elem

    type(VECTOR_DATA)          :: vector_elem
    integer                    :: size
    integer                    :: i

    size = vector_size( dataset%values )

    set_haselement = .false.
    do i = 1,size
        vector_elem = vector_at( dataset%values, i )
        if ( elem == vector_elem%data ) then
            set_haselement = .true.
            exit
        endif
    enddo
end function set_haselement

! set_iselement --
!     Function to check if an element is contained
!     in the set (reversed arguments)
! Arguments:
!     elem       Element to be checked
!     dataset    Existing set
!
logical function set_iselement( elem, dataset )
    type(SET_DATA), intent(in) :: elem
    type(SET), intent(in)      :: dataset

    set_iselement = set_haselement( dataset, elem )
end function set_iselement

! set_hassubset --
!     Function to check if one set is contained
!     in another set
! Arguments:
!     set1       Set that may contain the second
!     set2       Set that is possibly a subset
!
logical function set_hassubset( set1, set2 )
    type(SET), intent(in) :: set1
    type(SET), intent(in) :: set2

    type(VECTOR_DATA)     :: vector_elem
    integer               :: size
    integer               :: i

    size = vector_size( set2%values )

    set_hassubset = .true.
    do i = 1,size
        vector_elem = vector_at( set2%values, i )
        if ( .not. set_haselement( set1, vector_elem%data ) ) then
            set_hassubset = .false.
            exit
        endif
    enddo
end function set_hassubset

! set_issubset --
!     Function to check if one set is contained
!     in another set (reversed arguments)
! Arguments:
!     set1       Set that is possibly a subset
!     set2       Set that may contain the first
!
logical function set_issubset( set1, set2 )
    type(SET), intent(in) :: set1
    type(SET), intent(in) :: set2

    set_issubset = set_hassubset( set2, set1 )
end function set_issubset

! set_equal --
!     Function to check if two sets are equal
! Arguments:
!     set1       First set
!     set2       Second set
!
logical function set_equal( set1, set2 )
    type(SET), intent(in)      :: set1
    type(SET), intent(in)      :: set2

    set_equal = set_hassubset( set2, set1 )
    if ( set_equal ) then
        set_equal = set_hassubset( set1, set2 )
    endif
end function set_equal

! set_notequal --
!     Function to check if two sets are not equal
! Arguments:
!     set1       First set
!     set2       Second set
!
logical function set_notequal( set1, set2 )
    type(SET), intent(in)      :: set1
    type(SET), intent(in)      :: set2

    set_notequal = .not. set_hassubset( set2, set1 )
    if ( .not. set_notequal ) then
       set_notequal = .not. set_hassubset( set1, set2 )
    endif
end function set_notequal

! set_union --
!     Function to return the union of two sets
! Arguments:
!     set1       First set
!     set2       Second set
!
function set_union( set1, set2 ) result(union)
    type(SET), intent(in)         :: set1
    type(SET), intent(in)         :: set2
    type(SET)         :: union

    type(VECTOR_DATA) :: vector_elem
    integer           :: size
    integer           :: i

    call set_create( union )

    size = vector_size( set1%values )
    do i = 1,size
        vector_elem = vector_at( set1%values, i )
        call set_add( union, vector_elem%data )
    enddo

    size = vector_size( set2%values )
    do i = 1,size
        vector_elem = vector_at( set2%values, i )
        call set_add( union, vector_elem%data )
    enddo
end function set_union

! set_intersection --
!     Function to return the intersection of two sets
! Arguments:
!     set1       First set
!     set2       Second set
!
function set_intersection( set1, set2 ) result(intersection)
    type(SET), intent(in) :: set1
    type(SET), intent(in) :: set2
    type(SET)             :: intersection

    type(VECTOR_DATA)     :: vector_elem
    integer               :: size
    integer               :: i

    call set_create( intersection )

    size = vector_size( set1%values )
    do i = 1,size
        vector_elem = vector_at( set1%values, i )
        if ( set_haselement( set2, vector_elem%data ) ) then
            call set_add( intersection, vector_elem%data )
        endif
    enddo
end function set_intersection

! set_exclusion --
!     Function to return the exclusion of two sets
! Arguments:
!     set1       First set
!     set2       Second set
!
function set_exclusion( set1, set2 ) result(exclusion)
    type(SET), intent(in) :: set1
    type(SET), intent(in) :: set2
    type(SET)             :: exclusion

    type(VECTOR_DATA)     :: vector_elem
    integer               :: size
    integer               :: i

    call set_create( exclusion )

    size = vector_size( set1%values )
    do i = 1,size
        vector_elem = vector_at( set1%values, i )
        if ( .not. set_haselement( set2, vector_elem%data ) ) then
            call set_add( exclusion, vector_elem%data )
        endif
    enddo
end function set_exclusion

! set_add --
!     Add an element to the set (if it already exists,
!     it is not added)
! Arguments:
!     dataset    Set to which the element must be added
!     elem       Element to be added
!
subroutine set_add( dataset, elem )
    type(SET), intent(in)      :: dataset
    type(SET_DATA), intent(in) :: elem

    type(VECTOR_DATA)          :: vector_elem

    if ( .not. set_haselement( dataset, elem  ) ) then
        vector_elem%data = elem
        call vector_append( dataset%values, vector_elem )
    endif
end subroutine set_add

! set_delete_element --
!     Delete an element from the set (if it does not exist,
!     nothing happens)
! Arguments:
!     dataset    Set from which the element must be deleted
!     elem       Element to be deleted
!
subroutine set_delete_element( dataset, elem )
    type(SET), intent(in)      :: dataset
    type(SET_DATA), intent(in) :: elem

    type(VECTOR_DATA) :: vector_elem
    integer           :: size
    integer           :: i

    size = vector_size( dataset%values )
    do i = 1,size
        vector_elem = vector_at( dataset%values, i )
        if ( elem == vector_elem%data ) then
            call vector_delete_elements( dataset%values, i, 1 )
            exit
        endif
    enddo
end subroutine set_delete_element
