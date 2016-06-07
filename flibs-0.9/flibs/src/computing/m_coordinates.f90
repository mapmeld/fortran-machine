! coordinates.f90 --
!     Straightforward module for coordinates transformations:
!     - spherical to cartesian and back
!     - cylindrical to cartesian and back
!     - polar to cartesian and back
!
!     $Id: m_coordinates.f90,v 1.1 2008/09/22 18:42:43 arjenmarkus Exp $
!
module m_coordinates
    implicit none

contains

subroutine spherical_to_cart( rad, phi, theta, x, y, z )
    real, intent(in)  :: rad, phi, theta
    real, intent(out) :: x, y, z

    x = rad * cos(phi) * cos(theta)
    y = rad * sin(phi) * cos(theta)
    z = rad * sin(theta)

end subroutine spherical_to_cart

subroutine cart_to_spherical( x, y, z, rad, phi, theta )
    real, intent(in)  :: x, y, z
    real, intent(out) :: rad, phi, theta

    rad    = sqrt( x ** 2 + y ** 2 + z ** 2 )
    theta  = asin(z/rad)
    if ( x /= 0.0 .or. y /= 0.0 ) then
        phi = atan2(y,x)
    else
        phi = 0.0
    endif

end subroutine cart_to_spherical

subroutine cylindrical_to_cart( rad, phi, zin, x, y, z )
    real, intent(in)  :: rad, phi, zin
    real, intent(out) :: x, y, z


    x = rad * cos(phi)
    y = rad * sin(phi)
    z = zin

end subroutine cylindrical_to_cart

subroutine cart_to_cylindrical( x, y, z, rad, phi, zout )
    real, intent(in)   :: x, y, z
    real, intent(out)  :: rad, phi, zout

    rad    = sqrt( x ** 2 + y ** 2 )
    if ( x /= 0.0 .or. y /= 0.0 ) then
        phi = atan2(y,x)
    else
        phi = 0.0
    endif
    zout = z

end subroutine cart_to_cylindrical

subroutine polar_to_cart( rad, phi, x, y )
    real, intent(in)  :: rad, phi
    real, intent(out) :: x, y


    x = rad * cos(phi)
    y = rad * sin(phi)

end subroutine polar_to_cart

subroutine cart_to_polar( x, y, rad, phi )
    real, intent(in)  :: x, y
    real, intent(out) :: rad, phi

    rad    = sqrt( x ** 2 + y ** 2 )
    if ( x /= 0.0 .or. y /= 0.0 ) then
        phi = atan2(y,x)
    else
        phi = 0.0
    endif

end subroutine cart_to_polar

end module m_coordinates
