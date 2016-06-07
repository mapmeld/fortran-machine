! example.f90 --
!     Example of using "exceptions" in combination with the
!     preproc preprocessor
!
module example_routines
    use exception_handling

contains

subroutine mysub( x, y )
    real :: x, y

    ! pre: x > 0.0 .and. &
    !      y > 0.0

    if ( x <= 0.0 ) then
        throw( 'Parameter x must be positive' )
    endif
    if ( x > 10.0 ) return

    y = log10(x)
end subroutine mysub

end module

!
! The main program
!
program example
    use example_routines

    real    :: &
        x, y
    integer :: i

    try
        do i = 1,5
            x = 1.5 - 0.5*i
            call mysub( x, y )
            write(*,*) i, x, y
        enddo
    catch
        ! An exception is caught, report it!
        call &
            exception_report( 0 )
    endtry

    x = -1.0
    call mysub( x, y )
    write(*,*) 'Finally: ', x, y

    stop
end program
