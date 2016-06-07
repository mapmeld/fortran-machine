! test_annealing.f90 --
!     Test for the simulated_annealing module
!
module minimize_function
    use select_precision
contains

real(wp) function f( coords )
    real(wp), dimension(:), intent(in) :: coords

    real(wp)                           :: x, y

    x = coords(1)
    y = coords(2)
    f = x**2 + y**2 + sin(10.0*x) + 4*cos(20.0*y)

end function f

end module minimize_function

program test_annealing
    use minimize_function
    use simulated_annealing
    use select_precision

    implicit none

    type(ANNEALING_PARAMETERS)     :: params
    integer, parameter             :: ncoords = 2
    real(wp), dimension(2,ncoords) :: range
    real(wp), dimension(ncoords)   :: vector
    real(wp)                       :: value

    !
    ! Set the parameters for the procedure
    !
    call set_parameters( params, update = .false., verbose = .true., &
             number_iterations = 1000 )

    !
    ! We can use the default procedure, so let's do that
    ! (Otherwise we would use the reversed communication example)
    !
    range(:,1) = (/ -5.0, 5.0 /)
    range(:,2) = (/ -5.0, 5.0 /)
    vector     = (/ -5.0, -5.0 /)

    call find_minimum( params, range, vector, f, value )

    write(*,*) 'Minimum: ',value
    write(*,*) 'Solution vector: ',vector

end program


