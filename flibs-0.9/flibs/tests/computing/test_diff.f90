! test_diff.f90 --
!     Test (not exhausitvely, alas) the module for automatic
!     differentation
!
!     Define a non-trivial function and show that we can
!     evaluate both the value and the first derivative without having to
!     go through mathematics ourselves.
!
!     Also find a root of some equation via Newton-Raphson
!

module find_a_root
    use automatic_differentiation

contains
!
! g may not be an internal routine, hence the module
!
type(AUTODERIV) function g( x )
    type(AUTODERIV), intent(in)  :: x

    g = x ** 2 - 2.0
end function g

end module

program test_diff
    use automatic_differentiation
    use find_a_root

    implicit none

    type(AUTODERIV)  :: x, root
    integer          :: i
    logical          :: found

    do i = 1,20
        x = derivvar( 0.2 * (i-1) )
        write( *, * ) f(x), df(x%v)
    enddo

    x = derivvar( 3.0 )
    call find_root( g, x, 1.0e-6_wp, root, found )
    write(*,*) 'Equation: x**2 - 2 = 0'
    write(*,*) 'Root: ', root%v, ' - expected: ', sqrt(2.0_wp)

    write(*,*) 'Elementary functions: '
    x = derivvar(0.0)
    write(*,*) 'x = ', x
    write(*,*) 'exp(x)  = ', exp(x)
    write(*,*) 'sin(x)  = ', sin(x)
    write(*,*) 'asin(x) = ', asin(x)
    write(*,*) 'sinh(x) = ', sinh(x)
    write(*,*) 'cos(x)  = ', cos(x)
    write(*,*) 'acos(x) = ', acos(x)
    write(*,*) 'cosh(x) = ', cosh(x)
    write(*,*) 'tan(x)  = ', tan(x)
    write(*,*) 'atan(x) = ', atan(x)
    write(*,*) 'tanh(x) = ', tanh(x)

    x = derivvar(1.0)
    write(*,*) 'x = ', x
    write(*,*) 'log(x)  = ', log(x)
    write(*,*) 'exp(x)  = ', exp(x)

contains
!
! The AUTODERIV function and a function that implements the
! first derivative directly.
!
type(AUTODERIV) function f( x )
    type(AUTODERIV), intent(in)  :: x

    f = x ** 3 + 2.0 * x ** 2 - x + 3.0
end function f

real(wp) function df( x )
    real(wp) :: x

    df = 3.0 * x ** 2 + 4.0 * x  - 1.0
end function df

end program


