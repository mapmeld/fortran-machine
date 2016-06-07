! example.f90 --
!     Example of using "exceptions" in combination with the
!     preproc preprocessor
!
module example_routines
use exceptions
    use exception_handling

 implicit none
contains

subroutine mysub( x, y )
    real(dp) :: x, y

if ( &
   x > 0.0 .and. &
   y > 0.0
) then
write(*,*) "ERROR: condition failed at line 14 in file example.f90"
write(*,*) "   x > 0.0 .and. &"
write(*,*) "   y > 0.0"
stop
endif

write(*,*) 'In mysub (example_routines)'
    if ( x <= 0.0 ) then
write(*,*) 'At 16'
call exception_throw( 'example.f90',   17,&
 'Parameter x must be positive' )
return
 'Parameter x must be positive' )
write(*,*) 'At 17'
    endif
write(*,*) 'At 18'
    if ( x > 10.0 ) then
write(*,*) 'Leaving mysub'
return                          
endif
    if ( x > 10.0 ) then
write(*,*) 'At 19'

write(*,*) 'At 20'
    y = log10(x)
write(*,*) 'At 21'
write(*,*) 'Leaving mysub'
end subroutine mysub

end module

!
! The main program
!
program example
use exceptions
    use example_routines

 implicit none
    real(dp)    :: &
        x, y
    integer :: i

! TRY
exception_catch = exception_catch + 1
if ( exception_thrown ) then
     call exception_setpos( 'example.f90',   36)
     goto  1111
endif
    try
if ( exception_thrown ) then
     call exception_setpos( 'example.f90',   37)
     goto  1111
endif
write(*,*) 'In example (example)'
        do i = 1,5
write(*,*) 'At 37'
if ( exception_thrown ) then
     call exception_setpos( 'example.f90',   38)
     goto  1111
endif
            x = 1.5 - 0.5*i
write(*,*) 'At 38'
if ( exception_thrown ) then
     call exception_setpos( 'example.f90',   39)
     goto  1111
endif
            call mysub( x, y )
write(*,*) 'At 39'
if ( exception_thrown ) then
     call exception_setpos( 'example.f90',   40)
     goto  1111
endif
            write(*,*) i, x, y
write(*,*) 'At 40'
if ( exception_thrown ) then
     call exception_setpos( 'example.f90',   41)
     goto  1111
endif
        enddo
write(*,*) 'At 41'
! CATCH
goto  1112
 1111 continue
exception_thrown = .false.
    catch
write(*,*) 'At 42'
        ! An exception is caught, report it!
write(*,*) 'At 43'
        call &
            exception_report( 0 )
write(*,*) 'At 45'
! ENDTRY
 1112 continue
exception_catch = exception_catch - 1
    endtry
write(*,*) 'At 46'

write(*,*) 'At 47'
    x = -1.0
write(*,*) 'At 48'
    call mysub( x, y )
write(*,*) 'At 49'
    write(*,*) 'Finally: ', x, y
write(*,*) 'At 50'

write(*,*) 'At 51'
write(*,*) 'Leaving example'
    stop
write(*,*) 'At 52'
end program
