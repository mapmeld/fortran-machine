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

        ! end of response
        write(unitNo,AFORMAT) '</body>', '</html>', &
            '%REMARK% respond() completed ...'

        return

    end subroutine respond

end program test_fcgi
