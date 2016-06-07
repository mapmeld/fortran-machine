! config_utils.f90 --
!     Configuration program to determine the specifics for the
!     current program
!
! TODO: only one line per type!
!
program config_utils
    implicit none

    !
    ! Test: how can we run a system command?
    !
    call write_code( '_system_1', '_system_',&
        (/ '!!! system() needs no interface  ',&
           'character(len=20) :: cmd         ',&
           'logical :: exists                ',&
           '@check@                          ',&
           'cmd = "echo test"                ',&
           'call system( cmd )               ',&
           '@result@                         ' /) )  ! Strings must have the same length!
    call write_code( '_system_2', '_system_',&
        (/ '!!interface system               ',&  ! For actual module
           '!!    module procedure system_fnc',&
           '!!end interface                  ',&
           'character(len=20) :: cmd         ',&
           'logical :: exists                ',&
           '@check@                          ',&
           'cmd = "echo test"                ',&
           'call system_fnc( cmd )           ',&
           '@result@                         ',&
           'contains                         ',&
           'subroutine system_fnc( cmd )     ',&
           '    character(len=*) :: cmd      ',&
           '    integer          :: rc       ',&
           '    rc = system( cmd )           ',&
           'end subroutine                   ' /) )
    call write_code( '_system_3', '_system_',&
        (/ '!!! system() not supported!            ',&
           'character(len=20) :: cmd               ',&
           'logical :: exists                      ',&
           '@check@                                ',&
           'cmd = "echo test"                      ',&
           'call system( cmd )                     ',&
           '@result@                               ',&
           'contains                               ',&
           'subroutine system_fnc( cmd )           ',&
           '    character(len=*) :: cmd            ',&
           '    write(*,*) "system() not supported"',&
           '    rc = system( cmd )                 ',&
           'end subroutine                         ' /) )

    !
    ! Two flavours:
    ! - sleep() takes its argument as seconds. Use usleep() in addition
    ! - sleep() takes its argument as milliseconds.
    !
    call write_code( '_fsleep_1', '_fsleep_',&
        (/ 'real    :: wait = 0.1                  ',&
           'logical :: exists                      ',&
           '@check@                                ',&
           'call fsleep( wait )                    ',&
           '@result@                               ',&
           'contains                               ',&
           'subroutine fsleep( wait )              ',&
           '    real, intent(in) :: wait           ',&
           '    integer :: seconds                 ',&
           '    integer :: useconds                ',&
           '    seconds  = int(wait)               ',&
           '    useconds = int(1.0e6*wait)         ',&
           '    if ( seconds > 0 ) then            ',&
           '        call sleep( seconds )          ',&
           '    endif                              ',&
           '    if ( useconds > 0 ) then           ',&
           '        call usleep( useconds )        ',&
           '    endif                              ',&
           'end subroutine                         ' /) )
    call write_code( '_fsleep_2', '_fsleep_',&
        (/ 'real    :: wait = 0.1                  ',&
           'logical :: exists                      ',&
           '@check@                                ',&
           'call fsleep( wait )                    ',&
           '@result@                               ',&
           'contains                               ',&
           'subroutine fsleep( wait )              ',&
           '    real, intent(in) :: wait           ',&
           '    integer :: mseconds                ',&
           '    mseconds  = int(1000.0*wait)       ',&
           '    if ( mseconds > 0 ) then           ',&
           '        call sleep( mseconds )         ',&
           '    endif                              ',&
           'end subroutine                         ' /) )

contains
subroutine write_code( filename, category, code )
    character(len=*)               :: filename
    character(len=*)               :: category
    character(len=*), dimension(:) :: code

    integer :: lunout = 10
    integer :: i

    open( lunout, file = trim(filename) // '.f90' )

    write( lunout, '(a)' ) 'program configtest'
    do i = 1,size(code)
        if ( index( code(i), '@' ) < 1 ) then
            write( lunout, '(a)' ) code(i)
        else
            if ( index( code(i), '@check@'  ) >= 1 ) then
                write( lunout, '(3a)' ) 'inquire( file = "',trim(category),'", exist = exists)'
                write( lunout, '(3a)' ) 'if ( exists ) stop'
            endif
            if ( index( code(i), '@result@'  ) >= 1 ) then
                write( lunout, '(3a)' ) 'open( 10, file = "gather.inp", position="append")'
                write( lunout, '(3a)' ) 'write( 10, "(a)" ) "', filename, '"'
                write( lunout, '(3a)' ) 'close( 10 )'
                write( lunout, '(3a)' ) 'open( 10, file = "',trim(category),'")'
                write( lunout, '(3a)' ) 'close( 10 )'
            endif
        endif
    enddo
    write( lunout, '(a)' ) 'end program'
end subroutine
end program
