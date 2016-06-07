! Can we join arrays via array constructors?
!
program joinarrays
    real, dimension(10) :: a, b
    call random_number( a )
    call random_number( b )
    call print_numbers( (/ a, b /) )
    call change_numbers( (/ a, b /) )
    call print_numbers( (/ a, b /) )
contains
subroutine print_numbers( array )
    real, dimension(:) :: array
    write(*,*) 'Size: ', size(array)
    write(*,*) 'Numbers:: ', array
end subroutine
subroutine change_numbers( array )
    !real, dimension(:), intent(out) :: array  ! This is not accepted
    real, dimension(:) :: array  ! This _is_ accepted, but the original arrays are not changed
    array = 1.0
end subroutine
end program
