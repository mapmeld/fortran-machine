! gather.f90 --
!     Second part of the configuration of OS utils module:
!     collect the code that works
!
program gather
    implicit none

    integer           :: lunin   = 10
    integer           :: lunout  = 20
    integer           :: luncode = 11
    character(len=40) :: filename
    integer           :: ierr

    open( lunin, file = 'gather.inp' )
    open( lunout, file = 'osutils.f90' )

    write( lunout, '(a)' ) &
        '! osutils.f90 --',&
        '!     Utilities to interface with the operating system',&
        '!     -- do not edit: generated code --',&
        'module osutils',&
        '    implicit none',&
        '    !',&
        '    ! Convenience interfaces',&
        '    !'

    do
        read( lunin, '(a)', iostat = ierr ) filename
        if ( ierr /= 0 ) exit

        call copy_interface( filename )
    enddo

    write( lunout, '(a)' ) 'contains'

    rewind( lunin )

    do
        read( lunin, '(a)', iostat = ierr ) filename
        if ( ierr /= 0 ) exit

        call copy_code( filename )
    enddo

    write( lunout, '(a)' ) 'end module'

    close( lunin  )
    close( lunout )

contains

subroutine copy_interface( filename )

    character(len=*)   :: filename

    character(len=100) :: line
    integer            :: ierr
    integer            :: k

    open( luncode, file = trim(filename) // '.f90' )

    do
        read( luncode, '(a)', iostat = ierr ) line
        if ( ierr /= 0 ) exit

        k = index( line, '!!' )
        if ( k >= 1 ) then
            write( lunout, '(a)' ) trim(line(k+2:))
        endif
    enddo

    close( luncode )

end subroutine

subroutine copy_code( filename )

    character(len=*)   :: filename

    character(len=100) :: line
    integer            :: ierr

    open( luncode, file = trim(filename) // '.f90' )

    do
        read( luncode, '(a)', iostat = ierr ) line
        if ( ierr /= 0 ) exit

        if ( index( line, 'contains' ) >= 1 ) then
            exit
        endif
    enddo

    do
        read( luncode, '(a)', iostat = ierr ) line
        if ( ierr /= 0 ) exit

        if ( index( line, 'end program' ) >= 1 ) then
            exit
        else
            write( lunout, '(a)' ) trim(line)
        endif
    enddo

    close( luncode )

end subroutine
end program
