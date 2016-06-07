! hashtable.f90 --
!     Simple hash table implementation
!
!     See the example/test program for the way to use this
!
!     Note:
!     The data that need to be stored have the
!     generic type HASH_DATA. The key is a string
!     of at most 40 characters, trailing blanks
!     are ignored
!
!     Note:
!     Hash algorithm translated from Kernighan and Pike,
!     The Practice of Programming.
!
!     Note:
!     We need a linked list of the proper type, so
!     that is arranged in a different source file
!
!     $Id$

integer, parameter, private :: key_length = 40

type HASH_TABLE
    type(LINKED_LIST), dimension(:), pointer         :: lists
end type HASH_TABLE

private
public :: HASH_TABLE
public :: hash_create, hash_destroy, hash_delete_key, &
          hask_get_value, hash_put_value, hash_has_key

contains

! hash_index
!     Determine the index in the array
!
! Arguments:
!     hash          Hash table in question
!     key           Key (string) to be looked up or inserted
! Returns:
!     index into the array "lists"
!
integer function hash_index( hash, key )
    type(HASH_TABLE)   :: hash
    character(len=*)   :: key

    integer            :: i

    hash_index = 0
    do i = 1,min(len(key),key_length)
        hash_index = hash_multiplier * ichar(key(i:i)) + hash_index
    enddo

    hash_index = 1 + mod( hash_index, size(hash%lists) ) ;
end function hash_index

! hash_create
!     Create a new hash table
!
! Arguments:
!     hash          Hash table to be created
!     size          Indication of the size of the table (defaults to 101)
! Returns:
!     index into the array "lists"
!
subroutine hash_create( hash, size )
    type(HASH_TABLE)              :: hash
    integer, intent(in), optional :: size

    integer                       :: size_

    if ( present(size) ) then
        size_ = size
        call hash_determine_size( size_ )
    else
        size_ = 101
    endif

    allocate( hash%lists(size) )

    hash%lists(:) => null()
end subroutine hash_create

! hash_destroy
!     Destroy a hash table
!
! Arguments:
!     hash          Hash table to be created
!
subroutine hash_destroy( hash )
    type(HASH_TABLE)              :: hash


end subroutine hash_destroy

! hash_get_value
!     Get the value of a key, assuming it is in the hashtable
!
! Arguments:
!     hash          Hash table to be used
!     key           Key to retrieve
!     value         Value of the key (output)
!     error         Error indicator (if true, the key was not found)
!
subroutine hash_get_value( hash, key, value, error )
    type(HASH_TABLE), intent(in)   :: hash
    character(len=*), intent(in)   :: key
    type(HASH_DATA), intent(inout) :: value
    logical, intent(out)           :: error

    ...
end subroutine hash_get_value

! hash_put_value
!     Put the value of a key, replace it, if it is already in the hashtable
!
! Arguments:
!     hash          Hash table to be used
!     key           Key to add
!     value         Value of the key (input)
!
subroutine hash_put_value( hash, key, value )
    type(HASH_TABLE), intent(inout) :: hash
    character(len=*), intent(in)    :: key
    type(HASH_DATA), intent(in)     :: value

    ...
end subroutine hash_put_value

! hash_has_key
!     Check if the key is present
!
! Arguments:
!     hash          Hash table to be used
!     key           Key to check
! Returns:
!     .true. if the key is present, .false. otherwise
!
logical function hash_has_key( hash, key )
    type(HASH_TABLE), intent(inout) :: hash
    character(len=*), intent(in)    :: key

    ...
end function hash_has_key

! hash_delete_key
!     Delete the key and its value if present
!
! Arguments:
!     hash          Hash table to be used
!     key           Key to delete
!
subroutine hash_delete_key( hash, key )
    type(HASH_TABLE), intent(inout) :: hash
    character(len=*), intent(in)    :: key

    ...
end subroutine hash_delete_key
