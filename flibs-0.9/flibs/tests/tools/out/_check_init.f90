! check_init.f90 --
!     Example of using the editcode program to check certain
!     aspects of a program - such as the use of uninitialised variables
!
program check_init
use check_reals
    type(checked_real) :: x, y
    real(kind=kind(1.0)) :: sum ! Problem: can not write x, because of pointer argument
                                ! This "real" will not be replaced

    y = 3.0
call check_assignment( 10, 'check_init.f90' )
    call compute_sum( x, y )
call check_assignment( 11, 'check_init.f90' )
    y = 6.0
call check_assignment( 12, 'check_init.f90' )
    call compute_sum( x, y )
call check_assignment( 13, 'check_init.f90' )

call check_assignment( 14, 'check_init.f90' )
    sum = x
call check_assignment( 15, 'check_init.f90' )
    write(*,*) 'Sum: ', sum
call check_assignment( 16, 'check_init.f90' )
contains

subroutine compute_sum( x, y )
    type(checked_real) :: x, y

    x = x + y
call check_assignment( 22, 'check_init.f90' )
end subroutine compute_sum

end program check_init
