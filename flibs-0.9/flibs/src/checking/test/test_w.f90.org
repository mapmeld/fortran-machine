! test_w.f90
!     test program for the module "w_"
!
module module_a
    implicit none
contains
subroutine suba( y )
    integer :: y
    write( *, * ) 'Suba: ', y
    if ( y == 1 ) return
    write( *, * ) 'Suba: y /= 1'
end subroutine suba
end

subroutine subb( y )
    integer :: y
    write( *, * ) 'Subb: ', y
    if ( y == 1 ) return
    write( *, * ) 'Subb: y /= 1'
end

program test_w
    use module_a

    implicit none

    integer :: x

    open( 10, file = 'test_w.inp', err = 900 )

    close( 10 )

    call suba( 1 )
    call suba( 2 )
    call subb( 1 )

    goto 10

 10 continue
    x = 1
    if ( x .eq. 1 ) call subc( 1 )
    if ( x .eq. 1 ) &
        call subc( 2 )

    stop
900 continue
    write(*,*) 'Problem opening file'
contains
subroutine subc( y )
    integer :: y
    write( *, * ) 'Subc: ', y
end subroutine subc
end
