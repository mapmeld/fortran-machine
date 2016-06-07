! test_w.f90
!     test program for the module "w_"
!
module module_a
      use w_
implicit none
contains
subroutine suba( y )
integer :: y
write( *, * ) 'Suba: ', y
if ( y == 1 ) call w_return('test_w.f90',10,'suba', *99999)
write( *, * ) 'Suba: y /= 1'
99999 continue
end subroutine suba
end

subroutine subb( y )
      use w_
integer :: y
write( *, * ) 'Subb: ', y
if ( y == 1 ) call w_return('test_w.f90',18,'subb', *99999)
write( *, * ) 'Subb: y /= 1'
99999 continue
end

program test_w
      use w_
use module_a

implicit none

integer :: x

call w_open('test_w.f90',29,'test_w',*900, 10, file = 'test_w.inp', err = 900 )

call w_close('test_w.f90',31,'test_w',*99999, 10 )

      call w_call('test_w.f90',33,'test_w','suba')
call suba( 1 )
      call w_endcall
      call w_call('test_w.f90',34,'test_w','suba')
call suba( 2 )
      call w_endcall
      call w_call('test_w.f90',35,'test_w','subb')
call subb( 1 )
      call w_endcall

call w_goto('test_w.f90',37,'test_w', '10',*10)

10 continue
x = 1
if ( x .eq. 1 ) then
      call w_call('test_w.f90',41,'test_w','subc')
      call subc( 1 ) 
      call w_endcall 
      endif
if ( x .eq. 1 ) &
then
      call w_call('test_w.f90',42,'test_w','subc')
      call subc( 2 ) 
      call w_endcall 
      endif

call w_stop('test_w.f90',45,'test_w')
900 continue
write(*,*) 'Problem opening file'
99999 continue
contains
subroutine subc( y )
integer :: y
write( *, * ) 'Subc: ', y
99999 continue
end subroutine subc
end

