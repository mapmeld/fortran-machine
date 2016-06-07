! test_pointsets.f90 --
!     Test the module for generating sets of points in 1, 2 or 3
!     dimensions
!
program test_pointsets
    use pointsets
    implicit none

    real, dimension(20)   :: x
    real, dimension(1000) :: x2d
    real, dimension(1000) :: y2d
    real                  :: ratio
    real                  :: xb
    real                  :: xe
    real                  :: yb
    real                  :: ye
    integer               :: i

!
! One-dimensional:
! - Uniform spacing
! - Spacing based on geometric series
! - Random spacing
!
    write(*,*) 'Uniform spacing:'
    call arithmetic_spacing( x, 1.0, 20.0 )
    write(*,'(5f10.4)') x
    if ( differ( x(1), 1.0 ) ) then
        write(*,*) '   Too large a difference at start'
    endif
    if ( differ( x(20), 20.0 ) ) then
        write(*,*) '   Too large a difference at end'
    endif

    write(*,*) 'Geometric spacing:'
    call geometric_spacing( x, 1.0, 20.0, 0.5 )
    write(*,'(5f10.4)') x
    if ( differ( x(1), 1.0 ) ) then
        write(*,*) '   Too large a difference at start'
    endif
    if ( differ( x(20), 20.0 ) ) then
        write(*,*) '   Too large a difference at end'
    endif
    ratio = (x(4)-x(3))/(x(3)-x(2))
    if ( differ( ratio, 0.5 ) ) then
        write(*,*) '   Ratio is incorrect'
    endif

    write(*,*) 'Random spacing:'
    call random_spacing( x, 1.0, 20.0 )
    write(*,'(5f10.4)') x
    if ( differ( x(1), 1.0 ) ) then
        write(*,*) '   Too large a difference at start'
    endif
    if ( differ( x(20), 20.0 ) ) then
        write(*,*) '   Too large a difference at end'
    endif

    if ( any( x(2:) < x(1:size(x)-1) ) ) then
        write(*,*) '   Coordinates not in ascending order'
    endif

!
! Two-dimensional case:
! TODO: We ought to test the uniformity ...
!
    write(*,*) 'Two-dimensional random points'
    call random_rectangle( x2d, y2d, 1.0, 10.0 )
    if ( any( x2d < 0.0 ) .or. any( y2d < 0.0 ) ) then
        write(*,*) '    Negative coordinates found'
    endif
    if ( any( x2d > 1.0 ) .or. any( y2d > 10.0 ) ) then
        write(*,*) '    Coordinates out of range'
    endif

    do i = 1,10
        yb = i-1
        ye = i
        write(*,'(2f10.4,i6)') yb, ye, &
            count( y2d >= yb .and. y2d <= ye )
    enddo

    do i = 1,10
        xb = 0.1*(i-1)
        xe = 0.1*i
        write(*,'(2f10.4,i6)') xb, xe, &
            count( x2d >= xb .and. x2d <= xe )
    enddo

contains
logical function differ( x, y )
    real :: x, y

    differ = abs(x-y) > 0.1e-5
end function differ
end program
