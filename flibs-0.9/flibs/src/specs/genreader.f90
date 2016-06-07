! genreader.f90 --
!     Program to create a reader routine based on simple specifications
!
!     General:
!     -------
!     This program generates a routine that is capable of reading and
!     analysing data line by line from an external file. The code
!     entered by the user is filled into a general framework to robustly
!     read the data file.
!     Thus the program takes care of the tedious details and enables the
!     user to concentrate on getting the data in.
!
!     The idea:
!     --------
!     The user enters fragments of code associated with the various
!     fields of the records of the file. The genreader program can then
!     fill these in boiler plate code so that error handling and data
!     conversion is automatically taken care of.
!
!     Keywords indicate what type of fragment follows. Here is a list
!     of allowable keywords (to appear in this order, when given):
!
!     INTERFACE :=     The lines after this define the actual interface
!                      of the routine. This is an optional keyword.
!                      If not given, the keyword "ROUTINENAME := name"
!                      is expected to be given. This generates a
!                      subroutine with the interface:
!                          subroutine name( lun )
!                          integer, intent(in) :: lun
!
!                      The file is supposed to be opened at LU-number lun
!     ROUTINENAME := name
!                      See above
!     LINE-LENGTH := number
!                      Defines the maximum line length in the file
!                      (default: 80). Should occur early in the input
!                      (before the definition of field types and such)
!     SEPARATORS  := ' ,;'// achar(9)
!                      Defines the characters that separate the fields
!                      in the line. The default is: space, comma,
!                      semicolon and tab. A sequence of two or more
!                      separators is considered to be one.
!     EMPTY-FIELDS := true/false
!                      Defines if two or more separators delimit an
!                      empty field or not. Default: false.
!
!     BEFORE-LOOP :=   Code to be run _before_ reading any line in the file
!     READ-LINE   :=   Code to read the line, defaults to reading a
!                      single line from the file. If you need to read a
!                      set of lines or you want to skip comments, use
!                      this keyword to enter the code that will do this.
!     REPORT-ERRORS := true/false
!                      Produce a detailed error report or not
!     BEGIN-LOOP  :=   Code to be run _after_ reading and splitting a line
!                      but _before_ handling any fields
!
!     Field specifications come in between (see below)
!
!     END-LOOP    :=   Code to be run _after_ handling all fields
!     AFTER-LOOP  :=   Code to be run _after_ completing the loop
!     NEXT-RECORD-TYPE := comment
!                      Indicates that a new reading loop is to be started
!                      After this, any of the above keywords (except
!                      LINELENGTH, ROUTINE and INTERFACE) may occur
!                      again, as it will effectively start a new loop.
!
!     The following field types are allowed:
!     INTEGER :=       The field should represent a valid integer and
!                      the code that follows it will be called when this
!                      condition is met. Relevant variable holding the
!                      value: i_value
!     REAL :=          Similarly for single-precision reals (stored in: r_value)
!     DOUBLE :=        Similarly for double-precision reals (d_value)
!     STRING :=        Similarly for a character string (s_value)
!     LOGICAL :=       Similarly for a logical variable (l_value)
!     COMPLEX :=       Similarly for a complex variable (c_value)
!
!     The value is stored in a variable like r_value and the index of
!     the field is stored in the variable fieldno. If you need access to
!     the original string, use field(fieldno)
!
!     If all fields on the rest of the line have the same type and can
!     be treated in the same way, you can use these keywords:
!
!     ALL-INTEGERS :=
!     ALL-REALS :=
!     etc.
!
!     The fields will be handled one by one, just as in the case of
!     single fields. The difference is, however, that there is _no_
!     check on the number of fields.
!
!
!     Available routines to control the reading:
!     call next_record_type
!                      Use this to end the current loop and proceed with
!                      the next
!     call stop_reading
!                      Use this routine to stop the reading immediately
!                      after processing the current record
!
!     Procedure:
!     ---------
!     The procedure is this:
!     lun = 10
!     open( 10, file = filename, status = 'old' )
!     call name( lun )
!     close( 10 )
!
!     By leaving the opening and closing of the file to the user, it is
!     possible to read the file with different routines
!
program genreader
    implicit none

    character(len=80) :: filename
    character(len=80) :: line
    character(len=80) :: keyword
    character(len=32) :: routine_name
    integer           :: luinp = 10
    integer           :: luout = 20
    integer           :: fieldno
    integer           :: ierr
    integer           :: k
    logical           :: reported

    integer           :: line_length = 80
    character(len=20) :: separators = ' ,;'// achar(9)
    logical           :: empty_fields = .false.

    logical           :: interface_given = .false.
    logical           :: name_given      = .false.

    logical           :: before_loop_set = .false.
    logical           :: read_line_set   = .false.
    logical           :: begin_loop_set  = .false.
    logical           :: case_set        = .false.
    logical           :: end_loop_set    = .false.
    logical           :: after_loop_set  = .false.


    open( luinp, file = 'genreader.inp' )
    read( luinp, '(a)' ) filename
    close( luinp )

    open( luinp, file = filename, status = 'old', iostat = ierr )
    if ( ierr /= 0 ) then
        write( *, * ) 'Could not open file: ',trim(filename)
        stop
    endif

    open( luout, file = 'genreader.out' )

    do
        read( luinp, '(a)', iostat = ierr ) line
        if ( ierr /= 0 ) then
            write( *, * ) 'Only comments found in file - or reading error!'
            stop
        endif

        if ( line(1:1) /= '!' ) exit
    enddo

    fieldno = 0

outer_loop: &
    do while ( ierr == 0 )
        k = index( line, ':=' )
        if ( k <= 0 ) then
            write( *, '(3a)' ) 'Wrongly placed line: ', trim(line), ' - ignored'
            cycle
        endif

        keyword = line(1:k-1)

        select case( keyword )
            case ( 'INTERFACE' )
                if ( name_given ) then
                    write( *, * ) 'Error: you can not give both a routine name and an interface'
                    stop
                endif
                interface_given = .true.
                call copy_lines( line )
                cycle
            case ( 'ROUTINENAME' )
                if ( interface_given ) then
                    write( *, * ) 'Error: you can not give both a routine name and an interface'
                    stop
                endif
                name_given = .true.
                routine_name = get_string_value( line )
            case ( 'LINE-LENGTH' )
                line_length = get_integer_value( line )
            case ( 'SEPARATORS' )
                separators = get_string_value( line )
            case ( 'EMPTY-FIELDS' )
                empty_fields = get_logical_value( line )
            case ( 'BEFORE-LOOP' )
                !
                ! TODO: make sure all declarations have been written
                !
                call add_fragments_up_to( 'before-loop' )
                call copy_lines( line )
                cycle
            case ( 'READ-LINE' )
                !
                ! TODO: make sure all declarations plus + loop start have been written
                !
                call add_fragments_up_to( 'read-line' )
                call copy_lines( line )
                cycle
            case ( 'REPORT-ERRORS' )
                keyword = get_logical_keyword( line )
                write( luout, '(a)' ) '    report_errors = ' // trim(keyword)

            case ( 'BEGIN-LOOP' )
                !
                ! TODO: make sure all declarations plus + loop start and scan have been written
                !
                call add_fragments_up_to( 'begin-loop' )
                call copy_lines( line )
                cycle
            case ( 'END-LOOP' )
                !
                ! TODO: make sure the whole loop has been written
                !
                call add_fragments_up_to( 'end-loop' )
                call copy_lines( line )
                call write_fragment( 'end-loop' )
                cycle
            case ( 'AFTER-LOOP' )
                !
                ! TODO: make sure the whole loop has been written
                !
                call add_fragments_up_to( 'after-loop' )
                call copy_lines( line )
                cycle
            case ( 'NEXT-RECORD-TYPE' )
                !
                ! Reset lots of things
                !
            case ( 'INTEGER' )
                call add_fragments_up_to( 'case' )
                fieldno = fieldno + 1
                write( luout, '(a,i0,a)') '        case (', fieldno, ')'
                call write_fragment( 'start-integer' )
                call copy_lines( line )
                call write_fragment( 'stop-integer' )
                cycle
            case ( 'REAL' )
                call add_fragments_up_to( 'case' )
                fieldno = fieldno + 1
                write( luout, '(a,i0,a)') '        case (', fieldno, ')'
                call write_fragment( 'start-real' )
                call copy_lines( line )
                call write_fragment( 'stop-real' )
                cycle
            case ( 'DOUBLE' )
                call add_fragments_up_to( 'case' )
                fieldno = fieldno + 1
                write( luout, '(a,i0,a)') '        case (', fieldno, ')'
                write( luout, '(a,i0,a)') '        case (', fieldno, ')'
                call write_fragment( 'start-double' )
                call copy_lines( line )
                call write_fragment( 'stop-double' )
                cycle
            case ( 'STRING' )
                call add_fragments_up_to( 'case' )
                fieldno = fieldno + 1
                write( luout, '(a,i0,a)') '        case (', fieldno, ')'
                call write_fragment( 'start-string' )
                call copy_lines( line )
                call write_fragment( 'stop-string' )
                cycle
            case ( 'LOGICAL' )
                call add_fragments_up_to( 'case' )
                fieldno = fieldno + 1
                write( luout, '(a,i0,a)') '        case (', fieldno, ')'
                call write_fragment( 'start-logical' )
                call copy_lines( line )
                call write_fragment( 'stop-logical' )
                cycle
            case ( 'COMPLEX' )
                call add_fragments_up_to( 'case' )
                fieldno = fieldno + 1
                write( luout, '(a,i0,a)') '        case (', fieldno, ')'
                call write_fragment( 'start-complex' )
                call copy_lines( line )
                call write_fragment( 'stop-complex' )
                cycle
            case ( 'ALL-INTEGER' )
                call add_fragments_up_to( 'case' )
                write( luout, '(a,i0,a)') '        case default'
                call write_fragment( 'start-integer' )
                call copy_lines( line )
                call write_fragment( 'stop-integer' )
                cycle
            case ( 'ALL-REAL' )
                call add_fragments_up_to( 'case' )
                write( luout, '(a,i0,a)') '        case default'
                call write_fragment( 'start-real' )
                call copy_lines( line )
                call write_fragment( 'stop-real' )
                cycle
            case ( 'ALL-DOUBLE' )
                call add_fragments_up_to( 'case' )
                write( luout, '(a,i0,a)') '        case default'
                call write_fragment( 'start-double' )
                call copy_lines( line )
                call write_fragment( 'stop-double' )
                cycle
            case ( 'ALL-STRING' )
                call add_fragments_up_to( 'case' )
                write( luout, '(a,i0,a)') '        case default'
                call write_fragment( 'start-string' )
                call copy_lines( line )
                call write_fragment( 'stop-string' )
                cycle
            case ( 'ALL-LOGICAL' )
                call add_fragments_up_to( 'case' )
                write( luout, '(a,i0,a)') '        case default'
                call write_fragment( 'start-logical' )
                call copy_lines( line )
                call write_fragment( 'stop-logical' )
                cycle
            case ( 'ALL-COMPLEX' )
                call add_fragments_up_to( 'case' )
                write( luout, '(a,i0,a)') '        case default'
                call write_fragment( 'start-complex' )
                call copy_lines( line )
                call write_fragment( 'stop-complex' )
                cycle
            case default
                write( *, '(3a)' ) 'Unknown keyword: ', trim(keyword)
        end select

        do
            read( luinp, '(a)', iostat = ierr ) line
            if ( ierr /= 0 ) then
                exit outer_loop
            endif

            if ( line(1:1) /= '!' ) exit
        enddo
    enddo outer_loop
    call add_fragments_up_to( 'end-of-routine' )
contains

subroutine write_fragment( keyword )
    character(len=*) :: keyword

    ! DUMMY
    write( luout, '(2a)' ) '    ==> ',keyword
end subroutine write_fragment

subroutine copy_lines( line )
    character(len=*) :: line

    !
    ! Note: ierr from main program!
    !
    do
        read( luinp, '(a)', iostat = ierr ) line
        if ( ierr /= 0 ) exit


        if ( index(line, ':=' ) <= 0 ) then
            write( luout, '(a)' ) trim(line)
        else
            exit
        endif
    enddo
end subroutine copy_lines

integer function get_integer_value( line )
    character(len=*) :: line

    integer          :: k

    k = index(line, ':=' )
    k = k + 2
    read( line(k:), * ) get_integer_value

end function get_integer_value

function get_string_value( line ) result( keyword )
    character(len=*)         :: line
    character(len=len(line)) :: keyword

    integer          :: k

    k = index(line, ':=' )
    k = k + 2
    keyword = adjustl(line(k:))

end function get_string_value

logical function get_logical_value( line )
    character(len=*)         :: line
    character(len=len(line)) :: keyword

    integer                  :: k

    keyword = get_string_value(line(k:))

    if ( index( '.true.|.T.|yes|YES', keyword ) >= 1 ) then
        get_logical_value = .true.
    else
        get_logical_value = .false.
    endif

end function get_logical_value

character(len=10) function get_logical_keyword( line )
    character(len=*)         :: line

    if ( get_logical_value(line) ) then
        get_logical_keyword = ".true."
    else
        get_logical_keyword = ".false."
    endif

end function get_logical_keyword

subroutine add_fragments_up_to( keyword )
    character(len=*) :: keyword

    if ( .not. before_loop_set ) then
        call write_fragment( 'interface' )
        call write_fragment( 'declarations' )
        call write_fragment( 'initialise' )
        call write_fragment( 'before-loop' )
        before_loop_set = .true.
    endif
    if ( keyword == 'before-loop' ) return

    if ( .not. read_line_set ) then
        call write_fragment( 'read-line' )
        read_line_set = .true.
    endif
    if ( keyword == 'read-line' ) return

    if ( .not. begin_loop_set ) then
        call write_fragment( 'begin-loop' )
        begin_loop_set = .true.
    endif
    if ( keyword == 'begin-loop' ) return

    if ( .not. case_set ) then
        call write_fragment( 'case' )
        case_set = .true.
    endif
    if ( keyword == 'case' ) return

    if ( .not. end_loop_set ) then
        call write_fragment( 'end-loop' )
        end_loop_set = .true.
    endif
    if ( keyword == 'end-loop' ) return

    if ( .not. after_loop_set ) then
        call write_fragment( 'after-loop' )
        after_loop_set = .true.
    endif
    if ( keyword == 'after-loop' ) return

    call write_fragment( 'end-of-routine' )

end subroutine add_fragments_up_to

end program
