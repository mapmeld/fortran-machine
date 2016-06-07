! getenv.f90 --
!     Testing if getenv() is available
program test_getenv

    character(len=50) :: temp

    call getenv( 'TEMP', temp )

    write(*,*) temp
end program
