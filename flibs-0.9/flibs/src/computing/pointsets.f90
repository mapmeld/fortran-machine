! pointsets.f90 --
!     Collection of routines to generate sets of points in
!     one, two or three dimensions with certain properties
!
!     TODO: 3D, circles, spheres, on spherical surface
!
!     $Id: pointsets.f90,v 1.2 2006/06/25 07:24:31 arjenmarkus Exp $
!
module pointsets
    use select_precision
    implicit none

    real, parameter, private :: pi = 3.1415926_wp ! Alas, no atan() possible

contains

! arithmetic_spacing --
!     Generate uniformly spaced coordinates
! Arguments:
!     x       Array to be filled
!     begin   First bound of the interval
!     end     Second bound of the interval
! Result:
!     Array x filled with values between begin and end
!
subroutine arithmetic_spacing( x, begin, end )
    real(kind=wp), dimension(:), intent(inout) :: x
    real(kind=wp), intent(in)                  :: begin
    real(kind=wp), intent(in)                  :: end

    integer                                    :: i
    real(kind=wp)                              :: step

    if ( size(x) <= 1 ) return

    step = (end-begin)/(size(x)-1)
    x(1) = begin
    do i = 2,size(x)
        x(i) = x(i-1) + step
    enddo
end subroutine arithmetic_spacing

! geometric_spacing --
!     Generate coordinates spaced as a geometric series
! Arguments:
!     x       Array to be filled
!     begin   First bound of the interval
!     end     Second bound of the interval
!     ratio   Ratio between successive spacings
! Result:
!     Array x filled with values between begin and end
!
subroutine geometric_spacing( x, begin, end, ratio )
    real(kind=wp), dimension(:), intent(inout) :: x
    real(kind=wp), intent(in)                  :: begin
    real(kind=wp), intent(in)                  :: end
    real(kind=wp), intent(in)                  :: ratio

    integer                                    :: i
    real(kind=wp)                              :: step
    real(kind=wp)                              :: scale

    if ( size(x) <= 1 ) return
    if ( ratio <= 0.0 ) return
    if ( ratio == 1.0 ) return

    step  = ratio
    x(1)  = 0.0
    do i = 2,size(x)
        x(i) = x(i-1) + step
        step = step * ratio
    enddo

    scale = (end-begin) / (x(size(x))-x(1))
    x = begin + scale * x
end subroutine geometric_spacing

! random_spacing --
!     Generate coordinates randomly spaced over an interval
! Arguments:
!     x       Array to be filled
!     begin   First bound of the interval
!     end     Second bound of the interval
! Result:
!     Array x filled with values between begin and end,
!     all coordinates non-decreasing
!
subroutine random_spacing( x, begin, end )
    real(kind=wp), dimension(:), intent(inout) :: x
    real(kind=wp), intent(in)                  :: begin
    real(kind=wp), intent(in)                  :: end

    integer                                    :: i
    real(kind=wp)                              :: scale

    if ( size(x) <= 1 ) return

    call random_number( x )
    scale = end-begin
    x(1)  = 0.0
    do i = 2,size(x)
        x(i) = x(i-1) + x(i)
    enddo

    scale = (end-begin) / x(size(x))
    x = begin + scale * x
end subroutine random_spacing

! random_rectangle --
!     Generate points within a rectangle
!     that are uniformly distributed
! Arguments:
!     x       Array of x-coordinates to be filled
!     y       Array of y-coordinates to be filled
!     length  Length of the block
!     width   Width of the block
! Result:
!     Points uniformly distributed in the sense
!     that the expected number of points in a
!     region of the block depends only on the
!     region's area
!     Coordinates from 0 to length and 0 to width
!
subroutine random_rectangle( x, y, length, width )
    real(kind=wp), dimension(:), intent(inout) :: x
    real(kind=wp), dimension(:), intent(inout) :: y
    real(kind=wp), intent(in)                  :: length
    real(kind=wp), intent(in)                  :: width

    integer                                    :: i
    integer                                    :: n
    real(kind=wp)                              :: range

    if ( length <= 0.0 ) return
    if ( width  <= 0.0 ) return

    range = max( length, width )

    i = 1
    n = size(x)

    do while ( i < n )
        call random_number( x(i) )
        call random_number( y(i) )
        x(i) = range * x(i)
        y(i) = range * y(i)
        if ( x(i) <= length .and. y(i) <= width ) then
            i = i + 1
        endif
    enddo
end subroutine random_rectangle

! random_block --
!     Generate points within a (three-dimensional) block
!     that are uniformly distributed
! Arguments:
!     x       Array of x-coordinates to be filled
!     y       Array of y-coordinates to be filled
!     z       Array of z-coordinates to be filled
!     length  Length of the block
!     width   Width of the block
!     height  Height of the block
! Result:
!     Points uniformly distributed in the sense
!     that the expected number of points in a
!     region of the block depends only on the
!     region's area
!     Coordinates from 0 to length, 0 to width
!     and 0 to height
!
subroutine random_block( x, y, z, length, width, height )
    real(kind=wp), dimension(:), intent(inout) :: x
    real(kind=wp), dimension(:), intent(inout) :: y
    real(kind=wp), dimension(:), intent(inout) :: z
    real(kind=wp), intent(in)                  :: length
    real(kind=wp), intent(in)                  :: width
    real(kind=wp), intent(in)                  :: height

    integer                                    :: i
    integer                                    :: n
    real(kind=wp)                              :: range

    if ( length <= 0.0 ) return
    if ( width  <= 0.0 ) return
    if ( height <= 0.0 ) return

    range = max( length, width, height )

    i = 1
    n = size(x)

    do while ( i < n )
        call random_number( x(i) )
        call random_number( y(i) )
        call random_number( z(i) )
        x(i) = range * x(i)
        y(i) = range * y(i)
        z(i) = range * z(i)
        if ( x(i) <= length .and. y(i) <= width .and. z(i) <= height ) then
            i = i + 1
        endif
    enddo
end subroutine random_block

! random_rectangle_filter --
!     Generate points within a two-dimensional block
!     that are uniformly distributed
! Arguments:
!     x       Array of x-coordinates to be filled
!     y       Array of y-coordinates to be filled
!     length  Length of the block
!     width   Width of the block
!     filter  Filter function
!     params  Array of parameters for the filter
! Result:
!     The points are generated as in random_rectangle
!     but are passed to the filter function as well,
!     only if they pass the filter are they accepted
!
subroutine random_rectangle_filter( x, y, length, width, filter, params )
    real(kind=wp), dimension(:), intent(inout) :: x
    real(kind=wp), dimension(:), intent(inout) :: y
    real(kind=wp), intent(in)                  :: length
    real(kind=wp), intent(in)                  :: width
    real(kind=wp), dimension(:), intent(in)    :: params

    interface
        logical function filter( xp, yp, x, y, params )
            use select_precision
            real(kind=wp), intent(inout)            :: xp
            real(kind=wp), intent(inout)            :: yp
            real(kind=wp), dimension(:), intent(in) :: x
            real(kind=wp), dimension(:), intent(in) :: y
            real(kind=wp), dimension(:), intent(in) :: params
        end function filter
    end interface

    integer                                    :: i
    integer                                    :: n
    real(kind=wp)                              :: range

    if ( length <= 0.0 ) return
    if ( width  <= 0.0 ) return

    range = max( length, width )

    i = 1
    n = size(x)

    do while ( i < n )
        call random_number( x(i) )
        call random_number( y(i) )
        x(i) = range * x(i)
        y(i) = range * y(i)
        if ( x(i) <= length .and. y(i) <= width .and. &
            filter( x(i), y(i), x(1:i-1), y(1:i-1), params ) ) then
            i = i + 1
        endif
    enddo
end subroutine random_rectangle_filter

! filter_triangle --
!     Filter points that do not lie in a triangle
! Arguments:
!     xp      X-coordinate of candidate point
!     yp      Y-coordinate of candidate point
!     x       Array of x-coordinates filled so far
!     y       Array of y-coordinates filled so far
!     params  Array of parameters for the filter
! Result:
!     True if the point lies within the triangle,
!     false otherwise
!
logical function filter_triangle( xp, yp, x, y, params )
    use select_precision
    real(kind=wp), intent(inout)            :: xp
    real(kind=wp), intent(inout)            :: yp
    real(kind=wp), dimension(:), intent(in) :: x
    real(kind=wp), dimension(:), intent(in) :: y
    real(kind=wp), dimension(:), intent(in) :: params

    if ( xp/params(1)+yp/params(2) < 1.0 ) then
        filter_triangle = .true.
    else
        filter_triangle = .false.
    endif
end function filter_triangle

! random_triangle --
!     Generate points within the triangle (0,0)-(length,0)-(0,width)
!     that are uniformly distributed
! Arguments:
!     x       Array of x-coordinates to be filled
!     y       Array of y-coordinates to be filled
!     length  Length of the block
!     width   Width of the block
! Result:
!     Uniformly random points
!
subroutine random_triangle( x, y, length, width )
    real(kind=wp), dimension(:), intent(inout) :: x
    real(kind=wp), dimension(:), intent(inout) :: y
    real(kind=wp), intent(in)                  :: length
    real(kind=wp), intent(in)                  :: width

    real(kind=wp), dimension(1:2)              :: params

    params(1) = length
    params(2) = width

    call random_rectangle_filter( x, y, length, width, filter_triangle, params )
end subroutine random_triangle

! random_disk --
!     Generate points within a circle of given radius.
!     The points are uniformly distributed
! Arguments:
!     x       Array of x-coordinates to be filled
!     y       Array of y-coordinates to be filled
!     radius  Radius of the circle
! Result:
!     Uniformly random points (that is: average
!     number of points depends only on the area,
!     not the position)
!
subroutine random_disk( x, y, radius )
    real(kind=wp), dimension(:), intent(inout) :: x
    real(kind=wp), dimension(:), intent(inout) :: y
    real(kind=wp), intent(in)                  :: radius

    real(kind=wp)                              :: xp
    real(kind=wp)                              :: yp
    integer                                    :: i

    call random_rectangle( x, y, 1.0_wp, 1.0_wp )

    x = sqrt(x)
    y = 2.0_wp * pi * y

    do i = 1,size(x)
        xp = x(i)
        yp = y(i)
        x(i) = xp * cos(yp)
        y(i) = yp * sin(yp)
    enddo

end subroutine random_disk

! random_ball --
!     Generate points within a sphere of given radius.
!     The points are uniformly distributed
! Arguments:
!     x       Array of x-coordinates to be filled
!     y       Array of y-coordinates to be filled
!     z       Array of z-coordinates to be filled
!     radius  Radius of the sphere
! Result:
!     Uniformly random points (that is: average
!     number of points depends only on the area,
!     not the position)
!
subroutine random_ball( x, y, z, radius )
    real(kind=wp), dimension(:), intent(inout) :: x
    real(kind=wp), dimension(:), intent(inout) :: y
    real(kind=wp), dimension(:), intent(inout) :: z
    real(kind=wp), intent(in)                  :: radius

    real(kind=wp)                              :: xp
    real(kind=wp)                              :: yp
    real(kind=wp)                              :: zp
    integer                                    :: i

    call random_block( x, y, z, 1.0_wp, 1.0_wp, 1.0_wp )

    x = x**(1.0_wp/3.0_wp)
    y = 2.0_wp * pi * y
    z = acos( 2.0_wp*z-1.0_wp )

    do i = 1,size(x)
        xp = x(i)
        yp = y(i)
        zp = z(i)
        x(i) = xp * cos(yp) * cos(zp)
        y(i) = xp * sin(yp) * sin(zp)
        z(i) = xp * sin(zp)
    enddo

end subroutine random_ball

! random_sphere --
!     Generate points on the surface of a sphere of
!     given radius. The points are uniformly distributed
! Arguments:
!     x       Array of x-coordinates to be filled
!     y       Array of y-coordinates to be filled
!     z       Array of z-coordinates to be filled
!     radius  Radius of the sphere
! Result:
!     Uniformly random points (that is: average
!     number of points depends only on the area,
!     not the position)
!
subroutine random_sphere( x, y, z, radius )
    real(kind=wp), dimension(:), intent(inout) :: x
    real(kind=wp), dimension(:), intent(inout) :: y
    real(kind=wp), dimension(:), intent(inout) :: z
    real(kind=wp), intent(in)                  :: radius

    real(kind=wp)                              :: yp
    real(kind=wp)                              :: zp
    integer                                    :: i

    call random_rectangle( x, y, 1.0_wp, 1.0_wp )

    y = 2.0_wp * pi * y
    z = acos( 2.0_wp*z-1.0_wp )

    do i = 1,size(x)
        yp = y(i)
        zp = z(i)
        x(i) = radius * cos(yp) * cos(zp)
        y(i) = radius * sin(yp) * sin(zp)
        z(i) = radius * sin(zp)
    enddo
end subroutine random_sphere

end module pointsets
