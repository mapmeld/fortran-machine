! indexset.f90 --
!     Set operations defined for sets whose elements can be represented
!     as non-negative integers (so the smallest element that can be
!     represented is 0)
!
!     The implementation uses one _bit_ per element and does not presume
!     a strict upper limit, that is, the highest element determines the
!     amount of memory used.
!
!     Note:
!     To reduce the chance of memory leaks, the indexset type uses
!     allocatable components. This is part of TR 15581 and is fully
!     supported by the Fortran 2003 standard. If the compiler in
!     question does not support it, replace the allocatable keyword
!     by pointer. Then, however, memory leaks can and will appear,
!     when using assignment for instance.
!
module indexsets
    implicit none

    type INDEXSET
        private
        integer, allocatable, dimension(:) :: vector  ! Default integers!!

        !
        ! In case TR15581 is not supported:
        !
        ! integer, pointer, dimension(:) :: vector => null()

    end type INDEXSET

    private :: idx_ensure_capacity
    private :: idx_set_or_clear_member
    private :: idx_get_elem_bit

contains

! idx_ensure_capacity --
!     Ensure that the index set can hold a set of this amount of members
! Arguments:
!     idxset              The index set in question
!     member              Index of the member to be held
!
subroutine idx_ensure_capacity( idxset, member )
    type(INDEXSET), intent(inout) :: idxset
    integer, intent(in)           :: member

    integer                       :: new_size

    integer, allocatable, dimension(:) :: copy

    if ( .not. allocated( idxset%vector ) ) then     ! Change to associated - Not TR 15581
        new_size = max(1+member/bit_size(1),32)
        allocate( idxset%vector(new_size) )
        idxset%vector = 0
    else
        if ( member > bit_size(1)*size(idxset%vector) ) then
            allocate( copy(size(idxset%vector)) )
            copy = idxset%vector
            deallocate( idxset%vector )
            allocate( idxset%vector(1+member/bit_size(1)) )
            idxset%vector = 0
            idxset%vector(1:size(copy)) = copy
            deallocate( copy )
        endif
    endif

end subroutine idx_ensure_capacity

! idx_get_elem_bit --
!     Split the member index into a vector element and a bit in that element
! Arguments:
!     idxset              The index set in question
!     member              Index of the member to be held
!     elem                Index of the vector element
!     bit                 Bit index
!
subroutine idx_get_elem_bit( idxset, member, elem, bit )
    type(INDEXSET), intent(inout) :: idxset
    integer, intent(in)           :: member
    integer, intent(out)          :: elem
    integer, intent(out)          :: bit

    elem = 1 + member / bit_size(1)
    bit  = mod(member, bit_size(1))

end subroutine idx_get_elem_bit

! idx_set_member --
!     Register that a particular element is within the set
! Arguments:
!     idxset              The index set in question
!     member              Index of the member to be set
!
subroutine idx_set_member( idxset, member )
    type(INDEXSET), intent(inout) :: idxset
    integer, intent(in)           :: member

    call idx_set_or_clear_member( idxset, member, .true. )
end subroutine idx_set_member

! idx_clear_member --
!     Register that a particular element is NOT within the set
! Arguments:
!     idxset              The index set in question
!     member              Index of the member to be removed
!
subroutine idx_clear_member( idxset, member )
    type(INDEXSET), intent(inout) :: idxset
    integer, intent(in)           :: member

    call idx_set_or_clear_member( idxset, member, .false. )
end subroutine idx_clear_member

! idx_set_or_clear_member --
!     Set or clear the bit associated with the set's member
! Arguments:
!     idxset              The index set in question
!     member              Index of the member
!
subroutine idx_set_or_clear_member( idxset, member, set )
    type(INDEXSET), intent(inout) :: idxset
    integer, intent(in)           :: member
    logical, intent(in)           :: set

    integer                       :: elem
    integer                       :: bit

    if ( member < 0 ) then
        return
    endif
    call idx_ensure_capacity( idxset, member )

    call idx_get_elem_bit( idxset, member, elem, bit )

    if ( set ) then
        idxset%vector(elem) = ibset( idxset%vector(elem), bit )
    else
        idxset%vector(elem) = ibclr( idxset%vector(elem), bit )
    endif
end subroutine idx_set_or_clear_member

! idx_has_member --
!     Checks if the set has a  particular member
! Arguments:
!     idxset              The index set in question
!     member              Index of the member
!
logical function idx_has_member( idxset, member )
    type(INDEXSET), intent(inout) :: idxset
    integer, intent(in)           :: member

    integer                       :: elem
    integer                       :: bit

    idx_has_member = .false.
    if ( member < 0 ) then
        return
    endif
    if ( member > bit_size(1)*size(idxset%vector) ) then
        return
    endif

    call idx_get_elem_bit( idxset, member, elem, bit )

    idx_has_member = btest( idxset%vector(elem), bit )

end function idx_has_member

end module indexsets

! test_indexset.f90
!     Test program for indexset module
!
program test_indexset
    use INDEXSETS

    type(indexset) :: small_set
    type(indexset) :: primes

    integer        :: i
    integer        :: count

    !
    ! Some basic functionality:
    ! Define a set with a few elements and ensure that
    ! these are the only ones present
    ! Note:
    ! The minimum capacity is 1024 members (0 to 1023),
    ! so setting element 2000 exercises the reallocation
    ! code.
    !
    call idx_set_member( small_set,    0 )
    call idx_set_member( small_set,    1 )
    call idx_set_member( small_set,   10 )
    call idx_set_member( small_set, 2000 )

    call idx_set_member(   small_set, 1001 )
    call idx_clear_member( small_set, 1001 )

    write(*,*) 'List of members - expected: 0, 1, 10 and 2000'
    count = 0
    do i = 0,2000
        if ( idx_has_member( small_set, i ) ) then
            write(*,*) 'Member: ', i
            count = count + 1
        endif
    enddo
    write(*,*) 'Number of members: ', count

    !
    ! Sieve of Eratosthenes as a more elaborate example
    !
    call idx_clear_member( primes, 0 )
    call idx_clear_member( primes, 1 )
    do i = 2,100
        call idx_set_member( primes, i )
    enddo
    do i = 2,100
        j = 2*i
        do while ( j <= 100 )
            call idx_clear_member( primes, j )
            j = j + i
        enddo
    enddo

    write(*,*)
    write(*,*) 'Primes lower than 100: '

    count = 0
    do i = 0,100
        if ( idx_has_member( primes, i ) ) then
            write(*,*) 'Prime: ', i
            count = count + 1
        endif
    enddo
    write(*,*) 'Number of primes: ', count

end program
