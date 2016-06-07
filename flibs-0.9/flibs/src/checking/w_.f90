! w_.f90
!     Log various actions of a program
!
!     The idea behind this module is this:
!     If you need to understand a program written by somebody else,
!     it is often useful to run it in a debugger to see what it is
!     doing, but that can be time-consuming. Adding write statements
!     to the program is an alternative, but again time-consuming.
!
!     The module "w_" is meant, in conjunction with the Tcl program
!     "instrument.tcl" to automate the process of adding such
!     write statements.
!
!     The write statements that are added record:
!     - Calls to a subroutine (which routine, where)
!     - Returns from the subroutine
!     - Goto's within the main program or subroutine
!     - Opening/closing of files
!
!     Messages indicating what is happening in the program are written
!     to the screen (unit * to be more precise), because that way no
!     extra logical unit is consumed and it is easier to see the
!     program's actions in conjunction to the output it produces.
!
!     The design goals were:
!     - The code transformations should be simple (because these would
!       be easier to implement!)
!     - The code transformations should not change the program's logic
!       (of course!)
!
!     The module uses "alternative returns" to succeed in achieving
!     these goals, even though they are marked as "depricated" in the
!     Fortran 90 standard.
!
!     (The name "w_" was chosen, because it is unlikely to be used in
!     any ordinary program as the name of a module or routine.)
!
!     Notes:
!     - It can not easily log where functions are called, as
!       calls to functions are much more difficult to identify in the
!       source code than subroutine calls. Still, the source code for
!       functions is instrumented in the same way as subroutines
!       (with the exception of pure and elemental functions and
!       subroutines!)
!
!     - The instrumentation program is not foolproof: the generated
!       source will not be correct Fortran, if you use variable names
!       equal to keywords such as "return" or "open". (It is not a
!       full parser of Fortran code, it merely detects certain string
!       patterns and acts accordingly)
!
!
module w_

    implicit none

    integer, save, private :: indent = 0

contains

! w_call --
!     Log the calling of a subroutine
!
! Arguments:
!     filename      Name of the source file containing the call
!     lineno        Line number in the file
!     caller        Name of the subroutine/function that calls
!     callee        Name of the subroutine/function that is called
!
subroutine w_call( filename, lineno, caller, callee )
    character(len=*), intent(in) :: filename
    integer, intent(in)          :: lineno
    character(len=*), intent(in) :: caller
    character(len=*), intent(in) :: callee

    write( *, '(9a,i0)' ) 'W_', repeat('  ', min(indent,5)), &
       'Calling: ', trim(callee), &
       ' by: ', trim(caller), &
       ' -- file: ', trim(filename), ' line: ', lineno

    indent = indent + 1

end subroutine w_call

! w_endcall --
!     Handle the end of the call
!
! Arguments:
!     None
!
subroutine w_endcall

    indent = indent - 1

end subroutine w_endcall

! w_return --
!     Log the return from a subroutine
!
! Arguments:
!     filename      Name of the source file containing the call
!     lineno        Line number in the file
!     routine       Name of the subroutine/function
!     *             Added label (end of routine)
!
subroutine w_return( filename, lineno, routine, * )
    character(len=*), intent(in) :: filename
    integer, intent(in)          :: lineno
    character(len=*), intent(in) :: routine

    write( *, '(7a,i0)' ) 'W_', repeat('  ', min(indent,5)), &
       'Returning from: ', trim(routine), &
       ' -- file: ', trim(filename), ' line: ', lineno

    return 1

end subroutine w_return

! w_goto --
!     Log the use of a goto in a program/routine
!
! Arguments:
!     filename      Name of the source file containing the call
!     lineno        Line number in the file
!     routine       Name of the subroutine/function
!     label         Label to jump to
!     *             Actual label
!
subroutine w_goto( filename, lineno, routine, label, * )
    character(len=*), intent(in) :: filename
    integer, intent(in)          :: lineno
    character(len=*), intent(in) :: routine
    character(len=*), intent(in) :: label

    write( *, '(9a,i0)' ) 'W_', repeat('  ', min(indent,5)), &
       'Goto: ', trim(label), ' in routine: ', trim(routine), &
       ' -- file: ', trim(filename), ' line: ', lineno

    return 1

end subroutine w_goto

! w_stop --
!     Log the use of a stop statement
!
! Arguments:
!     filename      Name of the source file containing the call
!     lineno        Line number in the file
!     routine       Name of the subroutine/function
!
subroutine w_stop( filename, lineno, routine )
    character(len=*), intent(in) :: filename
    integer, intent(in)          :: lineno
    character(len=*), intent(in) :: routine

    write( *, '(7a,i0)' ) 'W_', repeat('  ', min(indent,5)), &
       'Stopped! in routine: ', trim(routine), &
       ' -- file: ', trim(filename), ' line: ', lineno

    stop

end subroutine w_stop

! w_close --
!     Log the closing of a file
!
! Arguments:
!     filename      Name of the source file containing the call
!     lineno        Line number in the file
!     routine       Name of the subroutine/function
!     unit          LU-number of the file
!     *             Label for the ERR= keyword
!     ...           Arguments matching the CLOSE keywords
!
!
subroutine w_close( filename, lineno, routine, *, unit, iostat, err, status )
    character(len=*), intent(in) :: filename
    integer, intent(in)          :: lineno
    character(len=*), intent(in) :: routine
    integer, intent(in)          :: unit
    integer, intent(out), optional         :: iostat  ! Note the intent!
    integer, intent(in), optional          :: err
    character(len=*), intent(in), optional :: status

    character(len=400)           :: name
    integer                      :: iostat_
    character(len=20)            :: status_

    inquire( unit = unit, name = name )
    write( *, '(5a,i0,3a,i0)' ) 'W_', repeat('  ', min(indent,5)), &
       'Closing file: ', trim(name), ' unit: ', unit, &
       ' -- file: ', trim(filename), ' line: ', lineno

    status_ = 'keep'
    if ( present(status) ) then
        status_ = status
    endif

    !
    ! Only use the status keyword, if it is not "keep"
    ! This is due to the restriction on the keyword for scratch files
    !
    if ( status_ /= 'keep' .and. status_ /= 'KEEP' ) then
        close( unit, status = status_, iostat=iostat_ )
    else
        close( unit, iostat=iostat_ )
    endif

    if ( present(iostat) ) then
        iostat = iostat_
    endif
    if ( present(err) .and. iostat_ /= 0 ) then
        return 1
    endif

end subroutine w_close

! w_open --
!     Log the opening of a file
!
! Arguments:
!     filename      Name of the source file containing the call
!     lineno        Line number in the file
!     routine       Name of the subroutine/function
!     *             Label for the ERR= keyword
!     ...           Arguments matching the OPEN keywords
!
! Note:
!     The names of the arguments must match the keywords in the
!     OPEN statement.
!
subroutine w_open( srcname, lineno, routine, *, unit, file, status, err, &
                   access, form, recl, blank, position, action, delim, &
                   pad, iostat )
    character(len=*), intent(in) :: srcname
    integer, intent(in)          :: lineno
    character(len=*), intent(in) :: routine

    integer, intent(in)          :: unit
    character(len=*), intent(in) :: file
    character(len=*), intent(in), optional :: status
    integer, intent(in), optional          :: err
    character(len=*), intent(in), optional :: access
    character(len=*), intent(in), optional :: form
    integer, intent(in), optional          :: recl
    character(len=*), intent(in), optional :: blank
    character(len=*), intent(in), optional :: position
    character(len=*), intent(in), optional :: action
    character(len=*), intent(in), optional :: delim
    character(len=*), intent(in), optional :: pad
    integer, intent(out), optional         :: iostat ! Note the intent!

    character(len=20) :: status_
    integer           :: err_
    character(len=20) :: access_
    character(len=20) :: form_
    integer           :: recl_
    character(len=20) :: blank_
    character(len=20) :: position_
    character(len=20) :: action_
    character(len=20) :: delim_
    character(len=20) :: pad_
    integer           :: iostat_

    write( *, '(5a,i0,3a,i0)' ) 'W_', repeat('  ', min(indent,5)), &
       'Opening file: ', trim(file), ' unit: ', unit , &
       ' -- file: ', trim(srcname), ' line: ', lineno

    status_ = 'unknown'
    if ( present(status) ) status_ = status
    access_ = 'sequential'
    if ( present(access) ) access_ = access
    form_ = 'formatted'
    if ( present(form) ) form_ = form
    recl_ = 1
    if ( present(recl) ) recl_ = recl
    blank_ = 'null'
    if ( present(blank) ) blank_ = blank
    position_ = 'asis'
    if ( present(position) ) position_ = position
    action_ = 'readwrite'
    if ( present(action) ) action_ = action
    delim_ = 'none'
    if ( present(delim) ) delim_ = delim
    pad_ = 'yes'
    if ( present(pad) ) pad_ = pad

    if ( access_ == 'direct' .or. access_ == 'DIRECT' ) then
        open( unit=unit, file=file, form=form_, access=access_, &
              recl=recl_, action=action_, iostat=iostat_ )
    else
        open( unit=unit, file=file, form=form_, access=access_, &
              delim=delim_, pad=pad_, blank=blank_, &
              position=position_, action=action_, iostat=iostat_ )
    endif

    if ( present(iostat) ) then
        iostat = iostat_
    endif

    if ( present(err) .and. iostat_ /= 0 ) then
        return 1
    endif

end subroutine w_open

end module w_
