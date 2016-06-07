! test_intervals.f90 --
!     Test for the interval_arithmetic module
!
!     Also find a root of some equation via Newton-Raphson
!
module root_functions
    use select_precision
contains

real(wp) function g( x )
    real(wp), intent(in) :: x

    g = x**2 - 2.0

end function g

real(wp) function gprime( x )
    real(wp), intent(in) :: x

    gprime = 2.0*x

end function gprime

end module root_functions

program test_intervals
    use root_functions
    use interval_arithmetic
    use select_precision

    implicit none

    type(INTERVAL)  :: x, y, root
    integer         :: i
    logical         :: found

    x = intval(1.0, 2.0)
    y = intval(2.0, 4.0)

    write(*,*) 'x   = ', x
    write(*,*) 'y   = ', y
    write(*,*) 'x+y = ', x+y
    write(*,*) 'x-y = ', x-y
    write(*,*) 'x*y = ', x*y
    write(*,*) 'x/y = ', x/y

    call find_root( g, gprime, x, 1.0e-6_wp, root, found )
    write(*,*) 'Equation: x**2 - 2 = 0'
    write(*,*) 'Root: ', root, ' - expected: ', sqrt(2.0_wp)

    write(*,*) 'Elementary functions:'
    write(*,*) 'x =', x
    write(*,*) 'exp(x) = ', exp(x)
    write(*,*) 'log(x) = ', log(x)
    write(*,*) 'sin(x) = ', sin(x)
    write(*,*) 'tan(x) = ', tan(x)
    write(*,*) 'y =', y
    write(*,*) 'cos(y) = ', cos(y)

end program


