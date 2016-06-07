! check_init.f90 --
!     Example of using the editcode program to check certain
!     aspects of a program - such as the use of uninitialised variables
!
program check_init
    real :: x, y
    real(kind=kind(1.0)) :: sum ! Problem: can not write x, because of pointer argument
                                ! This "real" will not be replaced

    y = 3.0
    call compute_sum( x, y )
    y = 6.0
    call compute_sum( x, y )

    sum = x
    write(*,*) 'Sum: ', sum
contains

subroutine compute_sum( x, y )
    real :: x, y

    x = x + y
end subroutine compute_sum

end program check_init
