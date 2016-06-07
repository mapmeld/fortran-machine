! test_timing.f90 --
!     Test (as far as possible) the module for timing parts
!     of a program.
!
program test_timing
    use timing
    implicit none

    real                            :: value
    real, dimension(100000)         :: array
    real, dimension(:), allocatable :: alloc
    real, dimension(:), pointer     :: ptr
    integer, dimension(4)           :: timer
    integer                         :: i
    integer                         :: j

    call timer_create( "explicit do-loop", timer(1) )
    call timer_create( "array-operation",  timer(2) )
    call timer_create( "allocatable",      timer(3) )
    call timer_create( "pointer",          timer(4) )

    do i = 1,100
        call random_number( value )
        call timer_start( timer(1) )
        do j = 1,100
            call do_loop( array, value )
        enddo
        call timer_stop( timer(1), 100 )

        call timer_start( timer(2) )
        do j = 1,100
            call array_op( array, value )
        enddo
        call timer_stop( timer(2), 100 )

        call timer_start( timer(3) )
        do j = 1,100
            allocate( alloc(1:100000) )
            call array_op( alloc, value )
            deallocate( alloc )
        enddo
        call timer_stop( timer(3), 100 )

        call timer_start( timer(4) )
        do j = 1,100
            allocate( ptr(1:100000) )
            call array_op( ptr, value )
            deallocate( ptr )
        enddo
        call timer_stop( timer(4), 100 )

    enddo

    call timer_report( 0 )

contains
subroutine do_loop( array, value )
    real, dimension(:), intent(out) :: array
    real, intent(in)                :: value

    integer                         :: i

    do i = 1,size(array)
        array(i) = value
    enddo
end subroutine do_loop

subroutine array_op( array, value )
    real, dimension(:), intent(out) :: array
    real, intent(in)                :: value

    array = value
end subroutine array_op

end program
