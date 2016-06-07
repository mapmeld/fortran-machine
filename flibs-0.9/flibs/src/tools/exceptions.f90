! exceptions.f90 --
!     Very preliminary version of "exception_handling"
!
module exception_handling

    logical :: exception_thrown = .false.
    integer :: exception_catch  = 0
    integer :: exception_lineno = 0
    character(len=100) :: exception_filename = ''
    character(len=100) :: exception_msg      = ''

contains

subroutine exception_setpos( filename, lineno )
    character(len=*) :: filename
    integer          :: lineno

    exception_lineno   = lineno
    exception_filename = filename

end subroutine exception_setpos

subroutine exception_throw( filename, lineno, msg )
    character(len=*) :: filename
    integer          :: lineno
    character(len=*) :: msg

    exception_lineno   = lineno
    exception_filename = filename
    exception_msg      = msg
    exception_thrown   = .true.

    if ( exception_catch == 0 ) then
         call exception_report( 0, .true. )
         stop
    endif

end subroutine exception_throw

subroutine exception_report( lun, uncaught )
    integer           :: lun
    logical, optional :: uncaught

    logical           :: uncaught_

    uncaught_ = .false.
    if ( present(uncaught) ) then
        uncaught_ = uncaught
    endif
    if ( uncaught_ ) then
        if ( lun > 0 ) then
            write( lun, * ) 'Uncaught exception: ', trim(exception_msg)
            write( lun, * ) '    (line', exception_lineno, ' in ', &
                trim(exception_filename), ')'
        else
            write( *, * ) 'Uncaught exception: ', trim(exception_msg)
            write( *, * ) '    (line', exception_lineno, ' in ', &
                trim(exception_filename), ')'
        endif
    else
        if ( lun > 0 ) then
            write( lun, * ) 'Exception caught: ', trim(exception_msg)
            write( lun, * ) '    (line', exception_lineno, ' in ', &
                trim(exception_filename), ')'
        else
            write( *, * ) 'Exception caught: ', trim(exception_msg)
            write( *, * ) '    (line', exception_lineno, ' in ', &
                trim(exception_filename), ')'
        endif
    endif

end subroutine exception_report


end module
