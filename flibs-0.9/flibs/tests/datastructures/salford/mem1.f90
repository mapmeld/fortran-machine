! mem1.f90 --
!     Simply include the source text
!

public :: printline
private

contains

! printline --
!     Subroutine to print a line of text
!
subroutine printline( string )
    character(len=*) :: string

    write(*,*) string

end subroutine printline
