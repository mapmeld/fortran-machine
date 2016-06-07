! test_mem_pool.f90 --
!     Test program for the memory pool facility
!
!     $Id: test_mem_pool.f90,v 1.1 2006/10/01 08:49:10 arjenmarkus Exp $
!
module MYDATA_MODULE

type MYDATA
    integer           :: pool_index  ! Required field, used internally
    integer           :: value       ! Simple value to store
end type MYDATA

end module

module MYDATA_POOL
    use MYDATA_MODULE, POOL_DATA => MYDATA

    include "mem_pool.f90"
end module MYDATA_POOL

program test_mem_pool
    use MYDATA_POOL

    implicit none

    !
    ! Define a new data type so that we can hold a lot of
    ! pointers to POOL_DATA data
    !
    type MYDATA_ARRAY
        type(POOL_DATA), pointer :: v
    end type
    type(MYDATA_ARRAY), dimension(210) :: array

    integer :: i

    !
    ! Acquire N POOL_DATA items
    !
    do i = 1,210
        call pool_acquire( array(i)%v )
        if ( .not. associated( array(i)%v ) ) then
            write(*,*) 'Error: data not acquired - ', i
            stop
        endif
        array(i)%v%value = 2*i
    enddo

    !
    ! Release half of them
    !
    do i = 51,150
        call pool_release( array(i)%v )
        if ( associated( array(i)%v ) ) then
            write(*,*) 'Error: data not released - ', i
            stop
        endif
    enddo

    !
    ! Acquire them again
    !
    do i = 51,150
        call pool_acquire( array(i)%v )
        if ( .not. associated( array(i)%v ) ) then
            write(*,*) 'Error: data not acquired (part 2) - ', i
            stop
        endif
        array(i)%v%value = -i
    enddo

    !
    ! Test that we have indeed all values
    !
    do i = 1,210
        write(*,*) i, array(i)%v%value
    enddo

end program
