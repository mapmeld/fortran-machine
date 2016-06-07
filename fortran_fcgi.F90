!
! Demonstration Fortran FastCGI program, to run with the nginx webserver
! By Ricolindo.Carino@gmail.com and arjen.markus895@gmail.com
!
! Requires:
!    - the FLIBS modules cgi_protocol and fcgi_protocol
!    - the FastCGI library
! See 'readme' for setup instructions of the compiler, nignx, and FastCGI library
!
! See 'makefile' for compile and execute commands. In summary,
!   To compile test_fcgi.f90      : make
!   To execute as FastCGI process : spawn-fcgi -a 127.0.0.1 -p 9000 ./test_fcgi
!      The "-a 127.0.0.1" and "-p 9000" options to spawn-fcgi must match the
!          "fastcgi_pass   127.0.0.1:9000;" in nginx.conf
!
! Notes:
!    1. Example 2 is from FLIBS test_cgi.f90
!    2. Customize routine respond() for your own application
!

program test_fcgi

    use fcgi_protocol

    implicit none

    type(DICT_STRUCT), pointer  :: dict => null() ! Initialisation is important!
    logical                     :: stopped = .false. ! set to true in respond() to terminate program
    integer                     :: unitNo ! unit number  for a scratch file

    ! open scratch file
    open(newunit=unitNo, status='scratch')
    ! comment previous line AND uncomment next line for debugging;
    !open(newunit=unitNo, file='fcgiout', status='unknown') ! file 'fcgiout' will show %REMARKS%

    ! wait for environment variables from webserver
    do while (fcgip_accept_environment_variables() >= 0)

        ! build dictionary from GET or POST data, environment variables
        call fcgip_make_dictionary( dict, unitNo )

        ! give dictionary to the user supplied routine
        ! routine writes the response to unitNo
        ! routine sets stopped to true to terminate program
        call respond(dict, unitNo, stopped)

        ! copy file unitNo to the webserver
        call fcgip_put_file( unitNo, 'text/html' )

        ! terminate?
        if (stopped) exit

    end do !  while (fcgip_accept_environment_variables() >= 0)

    ! before termination, it is good practice to close files that are open
    close(unitNo)

    ! webserver will return an error since this process will now terminate
    unitNo = fcgip_accept_environment_variables()


contains


    subroutine respond ( dict, unitNo, stopped )

        type(DICT_STRUCT), pointer        :: dict
        integer, intent(in)               :: unitNo
        logical, intent(out)              :: stopped

        ! the following are defined in fcgi_protocol
        !character(len=3), parameter :: AFORMAT = '(a)'
        !character(len=2), parameter :: CRLF = achar(13)//achar(10)
        !character(len=1), parameter :: NUL = achar(0)

        ! the script name
        character(len=80)  :: scriptName

        ! variables for Example 1
        character(len=80)  :: actionButton, username, password

        ! variables for Example 2 (from test_cgi.f90 of FLIBS)
        integer                           :: steps
        real                              :: xmin
        real                              :: xmax
        character(len=20)                 :: fnName
        character(len=20)                 :: output

        real, dimension(:), allocatable   :: x
        real, dimension(:), allocatable   :: y

        integer                           :: i
        logical                           :: okInputs

        ! start of response
        ! lines starting with %REMARK% are for debugging & will not be copied to webserver
        write(unitNo, AFORMAT) &
            '%REMARK% respond() started ...', &
            '<html>', &
            '<head><title>Fortran FastCGI</title></head>', &
            '<body>', &
            '<h1>Fortran FastCGI</h1>'

        ! retrieve script name (key=DOCUMENT_URI) from dictionary
        call cgi_get( dict, "DOCUMENT_URI", scriptName )

        if ( trim(scriptName) /= '/' ) & ! a script was requested
            write(unitNo,AFORMAT) 'Script is : '//trim(scriptName)

        select case (trim(scriptName))

            case ('/login') ! See form in Example 1 below
                ! keys are: action, username, password

                call cgi_get( dict, "action", actionButton)
                write(unitNo,AFORMAT) '<br>Action is : '//trim(actionButton)

                call cgi_get( dict, "username", username)
                write(unitNo,AFORMAT) '<br>Username is : '//trim(username)

                call cgi_get( dict, "password", password)
                write(unitNo,AFORMAT) '<br>Password is : '//trim(password)


            case ('/calculate') ! See form in Example 2 below
                ! keys are: function, minimum, maximum, steps, output

                fnName  = '?'
                xmin     = 0.0
                xmax     = 1.0
                steps    = 10
                call cgi_get( dict, "function", fnName  )
                call cgi_get( dict, "minimum",  xmin     )
                call cgi_get( dict, "maximum",  xmax     )
                call cgi_get( dict, "steps",    steps    )
                call cgi_get( dict, "output",   output   )

                write(unitNo, AFORMAT) '%REMARK% function='//trim(fnName )
                write(unitNo, '(a,f8.3)') '%REMARK% minimum=', xmin
                write(unitNo, '(a,f8.3)') '%REMARK% maximum=', xmax
                write(unitNo, '(a,i4)') '%REMARK% steps=', steps
                write(unitNo, AFORMAT) '%REMARK% output='//trim(output)

                okInputs = .true.
                if ( trim(fnName ) == '?' ) then
                    write(unitNo,AFORMAT) '<br>No function selected'
                    okInputs = .false.
                endif
                if ( abs(xmin) > 100.0 .or. abs(xmax) > 100.0 ) then
                    write(unitNo,AFORMAT) '<br>Minimum and maximum should be in the range -100 to 100'
                    okInputs = .false.
                endif
                if ( trim(fnName ) == 'J0' ) then
                    write(unitNo,AFORMAT) '<br>Sorry, the Bessel function is not yet implemented'
                    okInputs = .false.
                endif

                if (okInputs) then
                    !
                    ! Actual processing
                    !
                    allocate( x(0:steps), y(0:steps) )

                    x = (/ (xmin + i*(xmax-xmin)/steps, i=0,steps) /)
                    if ( trim(fnName ) == 'sin' ) then
                        y = sin(x)
                    endif
                    if ( trim(fnName ) == 'cos' ) then
                        y = cos(x)
                    endif

                    !
                    ! Write the HTML output or the CSV file
                    !
                    if ( trim(output) == 'html' ) then
                        write( unitNo,AFORMAT ) &
                            '<table>', &
                            '<tr><td>X</td><td>'//trim(fnName)//'(X)</td></tr>'
                        do i = 0,steps
                            write( unitNo, '(a,f12.6,a,f12.6,a)' ) &
                                '<tr><td>', x(i), '</td><td>', y(i), '</td></tr>'
                        enddo
                        write( unitNo,AFORMAT ) &
                            '</table>'
                    else
                        write( unitNo,AFORMAT ) &
                            '<pre>', '      X     ,      '//trim(fnName)//'(X)'
                        do i = 0,steps
                            write( unitNo, '(f12.6,a,f12.6)' ) x(i), ',', y(i)
                        enddo
                        write( unitNo,AFORMAT ) &
                            '</pre>'
                    endif

                end if


            case ('/shutdown') ! to terminate program
                write(unitNo,AFORMAT) '<br>Program has terminated.<br><a href="/">Verify</a>'
                stopped = .true.

        end select

        ! generate page for next action
        if (.not. stopped) then

            write(unitNo,AFORMAT) &
                "<hr>", &
                "<b>Example 1: POST method</b>", &
                "<table>", &
                "<form action='login' method='post'>", &
                "<tr><td>Username:</td><td>Password:</td><td>&nbsp;Action:</td></tr>", &
                "<tr><td><input type='text' name='username' value=''></td>", &
                "    <td><input type='password' name='password' value=''></td>", &
                "    <td>&nbsp;<input type='submit' name='action' value='Login'>", &
                "        or <input type='submit' name='action' value='Register'></td></tr>", &
                "</form>", &
                "</table>"

            write(unitNo,AFORMAT) &
                "<hr>", &
                "<b>Example 2: GET method</b>", &
                "<form action='calculate' method='get'>", &
                "<p>", &
                "Function: f(x) =", &
                "<select name='function'>", &
                "    <option value='sin' selected>sin(x)</option>", &
                "    <option value='cos'>cos(x)</option>", &
                "    <option value='J0'>J0(x)</option>", &
                "</select>", &
                "<br>", &
                "Domain:", &
                "<table>", &
                "<tr>", &
                "<td>Minimum = </td><td><input type='text' name='minimum' value='0.0'></td>", &
                "</tr>", &
                "<tr>", &
                "<td>Maximum = </td><td><input type='text' name='maximum' value='1.0'></td>", &
                "</tr>", &
                "<tr>", &
                "<td>Steps = </td><td><input type='text' name='steps' value='10'></td>", &
                "</tr>", &
                "</table>", &
                "<p>", &
                "Type of output:", &
                "<input type='radio' name='output' value='html', checked>HTML</input>", &
                "<input type='radio' name='output' value='csv'>CSV</input>", &
                "<p>", &
                "<input type='submit' value='Calculate'>", &
                "</form>"

            write(unitNo,AFORMAT) &
                "<hr>", &
                "<b>Example 3: Hyperlink</b><br>", &
                "&nbsp;<a href='shutdown'><b>Stop</b></a> the Fortran FastCGI program."

        end if

        ! end of response
        write(unitNo,AFORMAT) '</body>', '</html>', &
            '%REMARK% respond() completed ...'

        return

    end subroutine respond

end program test_fcgi
