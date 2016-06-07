! vectors.f90 --
!     Implementation of dynamically growing arrays
!
!     See the example/test program for the way to use this
!
!     The name has been chosen from the Standard Template
!     Library known from C++. Java has a similar class.
!
!     The module is straightforward: it defines a suitable
!     data structure, data can be added to the vector
!     and you can retrieve data from it.
!
!     Note:
!     For the function vector_at() we need a parameter
!     that represents the "empty vector data" value.
!
!     $Id: vectors.f90,v 1.4 2008/06/12 15:12:39 relaxmike Exp $

type VECTOR
    private
    integer                                  :: no_used
    type(VECTOR_DATA), dimension(:), pointer :: data    => null()
end type VECTOR

private
public :: VECTOR
public :: VECTOR_DATA
public :: vector_create
public :: vector_append
public :: vector_at
public :: vector_size
public :: vector_put
public :: vector_delete_elements
public :: vector_destroy
public :: vector_insert_empty

real, parameter :: growth_rate = 1.1

contains

! vector_create
!     Create a new vector
!
! Arguments:
!     vec           Variable that should hold the vector
!     capacity      Initial capacity (optional)
!
! Note:
!     The fields of the vector data structure are set
!
subroutine vector_create( vec, capacity )
    type(VECTOR)       :: vec
    integer, optional  :: capacity

    integer            :: cap

    !
    ! Check that the vector does not have any data left
    !
    if ( associated(vec%data) ) then
        call vector_destroy( vec )
    endif

    if ( present(capacity) ) then
        cap = max( 1, capacity )
    else
        cap = 10
    endif
    allocate( vec%data(1:cap) )
    vec%no_used = 0
end subroutine vector_create

! vector_destroy
!     Destroy a vector
!
! Arguments:
!     vec           Vector in question
!
subroutine vector_destroy( vec )
    type(VECTOR)       :: vec

    !
    ! Check that the vector does not have any data left
    !
    if ( associated(vec%data) ) then
        deallocate( vec%data )
    endif
    vec%no_used = 0
end subroutine vector_destroy

! vector_size
!     Return the number of elements in use
!
! Arguments:
!     vec           Variable that holds the vector
!
integer function vector_size( vec )
    type(VECTOR)       :: vec

    vector_size = vec%no_used
end function vector_size

! vector_at
!     Get the value of the nth element of the vector
!
! Arguments:
!     vec           Vector in question
!     n             Index of the element whose value
!                   should be retrieved
!
type(VECTOR_DATA) function vector_at( vec, n )
    type(VECTOR)       :: vec
    integer            :: n

    if ( n .lt. 1 .or. n .gt. vec%no_used ) then
        vector_at = empty_vector_data
    else
        vector_at = vec%data(n)
    endif
end function vector_at

! vector_insert_empty
!     Insert one or more empty elements
!
! Arguments:
!     vector        Vector in question
!     pos           Position to insert the empty elements
!     number        Number of empty elements
!
subroutine vector_insert_empty( vec, pos, number )
    type(VECTOR)         :: vec
    integer, intent(in)  :: pos
    integer, intent(in)  :: number

    integer              :: i

    if ( number .lt. 1 .or. pos .lt. 1 .or. pos .gt. vec%no_used ) then
        return
    endif

    if ( vec%no_used+number .ge. size(vec%data) ) then
        call vector_increase_capacity( vec, vec%no_used+number )
    endif

    do i = vec%no_used,pos,-1
        vec%data(i+number) = vec%data(i)
    enddo

    do i = 1,number
        vec%data(pos+i-1) = empty_vector_data
    enddo

    vec%no_used = vec%no_used + number
end subroutine vector_insert_empty

! vector_delete_elements
!     Delete one or more elements
!
! Arguments:
!     vector        Vector in question
!     pos           Position to start deletion
!     number        Number of elements
!
subroutine vector_delete_elements( vec, pos, number )
    type(VECTOR)         :: vec
    integer, intent(in)  :: pos
    integer, intent(in)  :: number

    integer              :: i

    if ( number .lt. 1 .or. pos .lt. 1 .or. pos .gt. vec%no_used ) then
        return
    endif

    do i = pos,vec%no_used-number
        vec%data(i) = vec%data(i+number)
    enddo

    vec%no_used = vec%no_used - number
end subroutine vector_delete_elements

! vector_append
!     Append a value to the vector
!
! Arguments:
!     vec           Vector in question
!     data          Data to be appended
!
subroutine vector_append( vec, data )
    type(VECTOR)       :: vec
    type(VECTOR_DATA)  :: data

    if ( vec%no_used .ge. size(vec%data) ) then
        call vector_increase_capacity( vec, vec%no_used+1 )
    endif

    vec%no_used = vec%no_used + 1
    vec%data(vec%no_used) = data
end subroutine vector_append

! vector_put
!     Put a value at a specific element of the vector
!     (it needs not yet exist)
!
! Arguments:
!     vec           Vector in question
!     n             Index of the element
!     data          Data to be put in the vector
!
subroutine vector_put( vec, n, data )
    type(VECTOR)       :: vec
    integer            :: n
    type(VECTOR_DATA)  :: data

    if ( n .lt. 1 ) then
        return
    endif
    if ( n .gt. size(vec%data) ) then
        call vector_increase_capacity( vec, n )
    endif

    vec%no_used = max( vec%no_used, n)
    vec%data(n) = data
end subroutine vector_put

! vector_increase_capacity
!     Expand the array holding the data
!
! Arguments:
!     vec           Vector in question
!     capacity      Minimum capacity
!
subroutine vector_increase_capacity( vec, capacity )
    type(VECTOR)       :: vec
    integer            :: capacity

    integer            :: new_cap
    type(VECTOR_DATA), dimension(:), pointer :: new_data

    new_cap = max( capacity, nint( growth_rate * size(vec%data) ) )

    if ( new_cap .gt. size(vec%data) ) then
        allocate( new_data(1:new_cap) )
        new_data(1:vec%no_used) = vec%data(1:vec%no_used)
        new_data(vec%no_used+1:new_cap) = empty_vector_data

        deallocate( vec%data )
        vec%data => new_data
    endif

end subroutine vector_increase_capacity
