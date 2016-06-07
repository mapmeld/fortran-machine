! mem_pool.f90 --
!     Implementation of a memory pool - reduce the number of
!     allocations/deallocations
!
!     Note:
!     The user-defined derived type "POOL_DATA" must include a
!     field "pool_index" to be used by the routines here
!
!     $Id: data_for_sets.f90,v 1.2 2006/03/26 19:03:53 arjenmarkus Exp $
!
private

public :: pool_acquire, pool_release, pool_setparams, pool_statistics, POOL_DATA

type POOL_CHUNK
    type(POOL_DATA), dimension(:), pointer :: data
    logical, dimension(:), pointer         :: available
    integer                                :: no_available
end type POOL_CHUNK

type(POOL_CHUNK), dimension(:), pointer :: chunk
logical, save                           :: init = .false.
integer, save                           :: chunk_size = 100
logical, save                           :: release_asap = .false.

contains

! pool_acquire --
!     Subroutine to associate a pointer to available POOL_DATA
!
! Arguments:
!     pdata           Pointer to the data
!
! Result:
!     The pointer variable pdata will be associated with
!     data so it can be used, when successful. Otherwise it
!     will still be disassociated.
!
subroutine pool_acquire( pdata )
    type(POOL_DATA), pointer :: pdata

    integer                  :: i
    integer                  :: j
    integer                  :: id

    if ( .not. init ) then
        init = .true.
        call pool_initialise
    endif

    id = -1
    do i = 1,size(chunk)
        if ( chunk(i)%no_available > 0 ) then
            id = i
            exit
        endif
    enddo

    if ( id == -1 ) then
        call pool_add_chunk( id )
    endif

    if ( id == -1 ) then
        return
    endif

    do j = 1,chunk_size
        if ( chunk(id)%available(j) ) then
            pdata => chunk(id)%data(j)
            pdata%pool_index = id * chunk_size + j - 1 ! Optimisation
            chunk(id)%available(j) = .false.
            chunk(id)%no_available = chunk(id)%no_available - 1
            exit
        endif
    enddo

end subroutine pool_acquire

! pool_release --
!     Subroutine to release the pointer that acquired before
!
! Arguments:
!     pdata           Pointer to the data
!
! Result:
!     The pointer variable pdata will be disassociated. The
!     memory it pointed to is available again for further use.
!
subroutine pool_release( pdata )
    type(POOL_DATA), pointer :: pdata

    integer                  :: i
    integer                  :: j

    if ( .not. init ) then
        write(*,*) 'Error in memory pool module: pool_release used before pool_acquire'
        return
    endif

    i = pdata%pool_index / chunk_size
    j = mod( pdata%pool_index, chunk_size ) + 1

    pdata => null()
    chunk(i)%available(j) = .true.
    chunk(i)%no_available = chunk(i)%no_available + 1

    if ( release_asap .and. chunk(i)%no_available == chunk_size ) then
        deallocate( chunk(i)%data )
        deallocate( chunk(i)%available )
        chunk(i)%no_available = -1
    endif
end subroutine pool_release

! pool_initialise --
!     Subroutine to initialise the memory pool
!
! Arguments:
!     None
!
! Result:
!     The array chunk is allocated with one element
!
subroutine pool_initialise
    integer :: id

    allocate( chunk(1) )
    chunk(1)%no_available = -1
    call pool_add_chunk( id )

end subroutine pool_initialise

! pool_add_chunk --
!     Subroutine to add a new chunk to the memory pool
!
! Arguments:
!     id           Index of the block that has become available
!
! Result:
!     The array chunk is expanded with one extra useable array of
!     POOL_DATA data
!
subroutine pool_add_chunk( id )
    integer, intent(out) :: id

    type(POOL_CHUNK), dimension(:), pointer :: newchunk

    integer :: i

    id = -1
    do i = 1,size(chunk)
        if ( chunk(i)%no_available == -1 ) then
            id = i
        endif
    enddo

    if ( id == -1 ) then
        allocate( newchunk(1:size(chunk)+1) )
        newchunk(1:size(chunk)) = chunk
        deallocate( chunk )
        chunk => newchunk

        id = size(chunk)
    endif

    allocate( chunk(id)%data(chunk_size) )
    allocate( chunk(id)%available(chunk_size) )
    chunk(id)%available = .true.
    chunk(id)%no_available = chunk_size

end subroutine pool_add_chunk

!
! TODO
!
subroutine pool_setparams
end subroutine pool_setparams

subroutine pool_statistics
end subroutine pool_statistics


! End source code of generic part
