! test_decimal.f90 --
!     Test the decimal arithmeitc module
!
program test_timing
    use decimal_arithmetic
    implicit none

    real          :: value1, value2
    type(DECIMAL) :: x, y

    call print_decs

    x = real_to_dec(1.01)
    y = real_to_dec(1.21)

    write(*,*) 'Approximation via reals:'
    write(*,*) '1.01=', x
    write(*,*) '1.21=', y
    write(*,*) '1.01+1.21=', x+y
    write(*,*) '1.01-1.21=', x-y
    write(*,*) '1.01*1.21=', x*y

    write(*,*) 'Computations with decimals, defined directly:'
                 !123456789
    x = decimal(1, 10000000)
    y = decimal(1,210000000)

    write(*,*) '1.01=', x
    write(*,*) '1.21=', y
    write(*,*) '1.01+1.21=', x+y
    write(*,*) '1.01-1.21=', x-y
    write(*,*) '1.01*1.21=', x*y
    write(*,*) '1.01*1.21=', 1.01*1.21
    write(*,*) '(1.01*1.21)/1.21=', (x*y)/y

    write(*,*) 'x = ', dec_to_string(x)
    write(*,*) 'y = ', dec_to_string(y)
    write(*,*) 'z = ', dec_to_string(real_to_dec(1.23))

end program
