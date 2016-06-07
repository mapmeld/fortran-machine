! test_cgi.f90 --
!     Test program/example for the cgi_protocol module
!     The program will generate a table of function values:
!     sin(x), cos(x) or J0(x) (as the latter is not implemented
!     this will cause an error).
!     The program illustrates the use of the various routines
!     in the module and the use of the mode "output_no_header"
!
program test_cgi

    use cgi_protocol

    type(DICT_STRUCT), pointer :: dict => null() ! Initialisation is important!
    integer                    :: i
    integer                    :: luout
    integer                    :: steps
    real                       :: xmin
    real                       :: xmax
    character(len=20)          :: function
    character(len=20)          :: output

    real, dimension(:), allocatable :: x
    real, dimension(:), allocatable :: y

    !
    ! Get the CGI information. As we do not know the type of
    ! output yet (either an HTML page or a CSV file), we use
    ! output_no_header.
    !
    call cgi_begin( output_no_header, dict, luout )

    function = '?'
    xmin     = 0.0
    xmax     = 1.0
    steps    = 10
    call cgi_get( dict, "function", function )
    call cgi_get( dict, "minimum",  xmin     )
    call cgi_get( dict, "maximum",  xmax     )
    call cgi_get( dict, "steps",    steps    )

    call cgi_get( dict, "output",   output   )

    !
    ! Simple checks (could probably better be done via JavaScript in the
    ! page itself, but this is an illustration)
    !
    if ( function == '?' ) then
        call cgi_error( 'No function selected', 'error_message.html' )
    endif
    if ( abs(xmin) > 100.0 .or. abs(xmax) > 100.0 ) then
        call cgi_error( 'Minimum and maximum should be in the range -100 to 100', &
           'error_message.html' )
    endif
    if ( function == 'J0' ) then
        call cgi_error( 'Sorry, the Bessel function is not yet implemented', &
           'error_message.html' )
    endif

    !
    ! Actual processing
    !
    allocate( x(steps+1), y(steps+1) )

    x = (/ (xmin + i*(xmax-xmin)/steps, i=0,steps) /)
    if ( function == 'sin' ) then
        y = sin(x)
    endif
    if ( function == 'cos' ) then
        y = cos(x)
    endif

    !
    ! Write the HTML output or the CSV file
    !
    if ( output == 'html' ) then
        call cgi_header( output_html )
        write( luout, '(a)' ) '<html>'
        write( luout, '(a)' ) '<head><title>Table</title></head>'
        write( luout, '(a)' ) '<body>'
        write( luout, '(a)' ) '<table>'
        write( luout, '(a)' ) '<tr>'
        write( luout, '(3a)' ) '   <td>X</td><td>', trim(function), '(X)</td>'
        write( luout, '(a)' ) '</tr>'

        do i = 0,steps
            write( luout, '(a)' ) '<tr>'
            write( luout, '(a,f10.4,a,f10.4,a)' ) &
                '    <td>', x(i), '</td><td>', y(i), '</td>'
            write( luout, '(a)' ) '</tr>'
        enddo
        write( luout, '(a)' ) '</table>'
        write( luout, '(a)' ) '</body>'
        write( luout, '(a)' ) '</html>'
    else
        call cgi_header( output_text )
        write( luout, '(2a)' ) 'X,', trim(function)

        do i = 0,steps
            write( luout, '(f10.4,a,f10.4)' ) x(i), ',', y(i)
        enddo
    endif

    !
    ! We are done
    !
    call cgi_end

end program
