! test_m_coordinates.f90 --
!     Test program for the m_coordinates module
!
program test_m_coordinates

    use m_coordinates

    implicit none

    real, parameter :: pi = acos(-1.0)

    integer :: i
    real    :: x, y, z, z2, z3
    real    :: rad, phi ,theta, rad2, phi2, theta2

    !
    ! Spherical to cartesian
    !
    do i = 0,6
        rad = 10.0; phi = i * pi / 6.0; theta = i * pi / 12.0

        call spherical_to_cart( rad, phi, theta, x, y, z )
        call cart_to_spherical( x, y, z, rad2, phi2, theta2 )

        write( *, '(9f8.4)' ) rad, phi, theta, x, y, z, rad2, phi2, theta2
    enddo


    !
    ! Cylindrical to cartesian
    !
    do i = 0,6
        rad = 10.0; phi = i * pi / 6.0; z = i / 5.0

        call cylindrical_to_cart( rad, phi, z, x, y, z2 )
        call cart_to_cylindrical( x, y, z2, rad2, phi2, z3 )

        write( *, '(9f8.4)' ) rad, phi, z, x, y, z2, rad2, phi2, z3
    enddo


    !
    ! Polar to cartesian
    !
    do i = 0,6
        rad = 10.0; phi = i * pi / 6.0

        call polar_to_cart( rad, phi, x, y )
        call cart_to_polar( x, y, rad2, phi2 )

        write( *, '(9f8.4)' ) rad, phi, x, y, rad2, phi2
    enddo

end program
