!
! fcgi_protocol.f90 --
!
!     By Ricolindo.Carino@gmail.com
!
!     Module for interfacing a Fortran program with a web server (nginx) via the FastCGI protocol
!
!     Requires the cgi_protocol module from FLIBS
!
!     Provides the following:
!
!         function fcgip_accept_environment_variables(), bound to libfcgi/FCGI_Accept()
!            - waits for FCGI environment variables from the webserver
!
!         function fcgip_get_char(), bound to libfcgi/FCGI_get_char()
!            - reads a character from POSTed data
!
!         function fcgip_put_string(), bound to libfcgi/FCGI_puts()
!            - copies a line of text to the webserver
!
!         subroutine fcgip_make_dictionary()
!            - breaks up QUERY_STRING or POSTed data into a dictionary
!
!         subroutine fcgip_put_file ()
!            - copies a file line by line to the webserver
!
!     User must supply the routine to interpret the dictionary and compose a
!         response to QUERY_STRING or POSTed data
!

module fcgi_protocol

    use cgi_protocol

    implicit none

    ! FCGI library (libfcgi) routines
    interface

        ! The function to wait for FastCGI environment variables from the webserver
        function fcgip_accept_environment_variables () bind(C, NAME='FCGI_Accept')
            use ISO_C_BINDING
            implicit none
            integer(C_INT) :: fcgip_accept_environment_variables
        end function fcgip_accept_environment_variables

        ! The function to retrieve a character from POSTed data
        function fcgip_get_char () bind(C, NAME='FCGI_getchar')
            use ISO_C_BINDING
            implicit none
            character(C_CHAR) :: fcgip_get_char
        end function fcgip_get_char

        ! The function to copy a null-terminated string to the webserver
        function fcgip_put_string (s) bind(C, NAME='FCGI_puts')
            use ISO_C_BINDING
            implicit none
            integer(C_INT) :: fcgip_put_string
            character(C_CHAR), dimension(*) :: s
        end function fcgip_put_string

    end interface

    ! public abbreviations
    character(len=3), parameter                :: AFORMAT = '(a)'
    character(len=2), parameter                :: CRLF = achar(13)//achar(10)
    character(len=1), parameter                :: NUL = achar(0)

    ! private objects
    ! MAX_CONTENT_LENGTH must be enough for "long" QUERY_STRING or POSTed content
    integer, parameter, private                :: MAX_CONTENT_LENGTH = 1024
    character(len=MAX_CONTENT_LENGTH), private :: content
    integer, private                           :: iStat

contains

    subroutine fcgip_make_dictionary( dict, unitNo )
        ! Retrieve FastCGI environment variables into dictionary 'dict'
        ! Invoked after FCGI_Accept()/fcgip_accept_environment_variable() has completed
        ! Write debugging information to file unit number 'unitNo', which must already be open
        ! Debugging information should begin with %REMARK%, so as not to be sent
        !     to the webserver, see fcgi_put_file()

        type(DICT_STRUCT), pointer        :: dict
        integer, intent(in)               :: unitNo

        integer                           :: i
        integer                           :: iLen
        character(len=1)                  :: ch

        ! write to the beginning of file unitNo
        rewind (unitNo)

        ! Clean up dictionary ?
        if ( associated(dict) ) then
            call dict_destroy( dict )
            write(unitNo, AFORMAT) '%REMARK% cleaned dictionary...'
        else
            write(unitNo, AFORMAT) '%REMARK% dictionary NOT associated()...'
        endif

        ! add the requested script ('/' if none) to dictionary
        call get_environment_variable('DOCUMENT_URI', content)
        iLen = len_trim(content)
        if ( iLen > 0 ) then
            content = 'DOCUMENT_URI='//content
        else ! default is /, to ensure dictionary is not empty
            content = 'DOCUMENT_URI=/'
        endif
        iLen = len_trim(content)
        call cgi_store_dict( dict, content(:iLen) )
        write(unitNo, AFORMAT) '%REMARK% added to dictionary: '//content(:iLen)

        ! QUERY_STRING (request method was GET) ?
        call get_environment_variable( "QUERY_STRING", value=content, length=iLen, status=iStat )
        if ( iStat == 0 ) then
            write(unitNo, AFORMAT) '%REMARK% QUERY_STRING='//trim(content)
            if ( iLen > 0 ) then
                    call cgi_store_dict( dict, content(:iLen) )
                    write(unitNo, AFORMAT) '%REMARK% added to dictionary: QUERY_STRING='//content(:iLen)
            end if
        endif

        ! anything in CONTENT_LENGTH (request method was POST) ?
        call get_environment_variable( "CONTENT_LENGTH", value=content, status=iStat )
        if ( iStat == 0 ) then
            write(unitNo, AFORMAT) '%REMARK% CONTENT_LENGTH='//trim(content)
            iLen = len_trim(content)
            if ( iLen > 0 ) then
                read( content, * ) iLen
                do i=1,iLen
                    ch = fcgip_get_char()
                    content( i:i ) = ch
                end do
                content( iLen+1: ) = ' '
                call cgi_store_dict( dict, content(:iLen) )
                write(unitNo, AFORMAT) '%REMARK% added to dictionary: CONTENT='//content(:iLen)
            end if
        endif

        ! for other environment variables, see <nginx directory>/conf/fastcgi_params

        write(unitNo, AFORMAT) '%REMARK% completed dictionary...'

    end subroutine fcgip_make_dictionary



    subroutine fcgip_put_file ( unitNo, mimetype )
        ! Copy file 'unitNo' line by line to the webserver via FCGI_puts()
        !     except for lines beginning with %REMARK%
        ! File must already exist, expected to contain the response to some query

        integer, intent(in)                    :: unitNo
        character(len=*), intent(in), optional :: mimetype

        character(len=80)                      :: mimetype_

        mimetype_ = 'text/html'
        if ( present(mimetype) ) then
            mimetype_ = mimetype
        endif

        ! flush any pending writes
        flush(unitNo)

        ! let the server know what type of data
        iStat = fcgip_put_string ('Content-type: '//trim(mimetype_)//CRLF//NUL)

        ! copy line by line to webserver, except those starting with %REMARK%
        rewind(unitNo)
        do while (.true.)
            read(unitNo, AFORMAT, iostat=iStat) content
            if (iStat < 0) exit ! no more lines
            if (content(:8) == '%REMARK%') cycle
            iStat = fcgip_put_string (trim(content)//NUL) ! FCGI_puts expects NULL terminated strings
        end do

    end subroutine fcgip_put_file


end module fcgi_protocol
