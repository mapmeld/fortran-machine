! reopen_stdin --
!     Program to read a file via redirection and then open standard
!     input for ordinary interaction
!
!     Based on an example by Charles Coldwell
!     http://groups.google.com/group/comp.lang.fortran/browse_frm/thread/56ca2ee5b9d77453#
!
program stdin
    implicit none
    logical           :: readline
    character(len=40) :: line

    write(*,*) 'Read from redirected file ...'
    do
        read (5,*,end=100) line
        write (6,*) line
    end do
100 close(5)

    !
    ! Open the UNIX/Linux/OSX way
    !
    readline = .false.
    open(unit=5,file="/dev/tty", status='old', err = 110)
    write(*,*) 'Reopening standard input ... use ctrl-D to close the read'
    do
        read (5,*,end=200) line
        readline = .true.
        write (6,*) line
    end do
    goto 200

    !
    ! Open the Windows way
    !
110 continue
    readline = .false.
    write(*,*) 'Reopening standard input ... use ctrl-Z to close the read'
    open(unit=5,file="con", status='old', err = 120)
    write(*,*) 'Read from redirected file ...'
    do
        read (5,*,end=200) line
        readline = .true.
        write (6,*) line
    end do
    goto 200

120 continue
    write(*,*) 'Did not succeed in reopening standard input!'
    stop

200 continue
    if ( .not. readline ) then
        write(*,*) 'No line read after reopening standard input!'
    endif
end program stdin
