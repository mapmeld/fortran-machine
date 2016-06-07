! test_genalg.f90 --
!     Test for the genetic_algorithms module
!
!     Note:
!     The first step (find the minimum for f) does not work as
!     expected, the algorithm gets stuck on a suboptimum (at least
!     with one random sequence). Probably it is necessary to tune
!     the parameters to get a better performance
!     (Intended minimum: (-1.1, 0, 0, ... )
!
!     The second step does find the absolute minimum, but this is
!     a much simpler function.
!
module minimize_function
    use select_precision
contains

real(wp) function f( coords )
    real(wp), dimension(:), intent(in) :: coords

    real(wp)                           :: r

    r = sqrt( sum(coords**2) )
    f = (r**2 - 1.0) ** 2 - coords(1)

end function f

real(wp) function g( coords )
    real(wp), dimension(:), intent(in) :: coords

    g = sum(coords**2) - 1.0

end function g

end module minimize_function

program test_genalg
    use minimize_function
    use genetic_algorithms
    use select_precision

    implicit none

    type(GA_PARAMETERS)            :: params
    integer, parameter             :: ncoords = 10
    real(wp), dimension(2,ncoords) :: range
    real(wp), dimension(ncoords)   :: vector
    real(wp)                       :: value
    integer                        :: i

    !
    ! Set the parameters for the procedure
    !
    call set_parameters( params, update = .false., verbose = .true., &
             number_iterations = 1000 )

    !
    ! We can use the default procedure, so let's do that
    ! (Otherwise we would use the reversed communication example)
    !
    range(1,:) = -5.0
    range(2,:) =  5.0

    call find_minimum( params, range, vector, f, value )

    write(*,*) 'Minimum:         ',value
    write(*,*) 'Solution vector: ',vector
    write(*,*) 'f(vector):       ',f(vector)

    call find_minimum( params, range, vector, g, value )

    write(*,*) 'Minimum:         ',value
    write(*,*) 'Solution vector: ',vector
    write(*,*) 'g(vector):       ',g(vector)

end program


