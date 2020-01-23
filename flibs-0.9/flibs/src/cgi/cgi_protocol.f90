! cgi_protocol.f90 --
!     Module for interfacing with a web server via the CGI protocol
!     (GET, POST, ...)
!
!     TODO:
!     - find out how to deal with text area data
!     - find out how to deal with uploaded files
!     - merge environment variables into the cgi_get routines
!     - implement the SCGI protocol
!     - implement delayed responses (also via customisable template)
!
module cgi_protocol
    implicit none

    integer, parameter :: DICT_KEY_LENGTH    = 80
    integer, parameter :: DICT_VALUE_LENGTH  = 200
    integer, parameter :: DICT_BUFFER_LENGTH = DICT_KEY_LENGTH + DICT_VALUE_LENGTH + 1

    integer, parameter :: output_no_header    = 0
    integer, parameter :: output_html         = 1
    integer, parameter :: output_text         = 2
    integer, parameter, private :: output_html_delayed = 3  ! Not implemented yet!
    integer, parameter, private :: output_text_delayed = 4

    type DICT_DATA
        character(len=DICT_VALUE_LENGTH) :: value
    end type DICT_DATA

    interface cgi_get
        module procedure cgi_get_string
        module procedure cgi_get_integer
        module procedure cgi_get_real
    end interface

    type(DICT_DATA), parameter :: dict_null = dict_data('')

    integer, private, save     :: method    = -1
    integer, private, save     :: luout_cgi = -1
    logical, private, save     :: header_written

!
! Body of source code for storing and retrieving the data
! (also contains the CONTAINS clause)
!
include 'dictionary.f90'

! cgi_begin --
!     Determine the type of interaction with the server and retrieve the data
!
! Arguments:
!     html           Whether the output will be HTML or plain text
!     dict           Dictionary holding the information from the server (output)
!     luout          LU-number for writing the file (output!)
!
! Note:
!     This routine determines the way the server passes on the data
!     by looking for clues in the environment variables QUERY_LENGTH
!     and QUERY_STRING
!
!
! TODO:
!     Support for two-pass run (if the computation takes a longer
!     than a few seconds)
!
subroutine cgi_begin( html, dict, luout )
    integer, intent(in)               :: html
    type(DICT_STRUCT), pointer        :: dict
    integer, intent(out)              :: luout

    integer                           :: length
    integer                           :: status
    logical                           :: opend
    character(len=DICT_BUFFER_LENGTH) :: string
    character(len=1)                  :: ch

    !
    ! Clean up, if necessary
    !
    if ( associated(dict) ) then
        call dict_destroy( dict )
    endif

    header_written = .false.

    !
    ! Determine which input method
    !
    call get_environment_variable( "QUERY_STRING", length=length, status=status )
    if ( status == 0 ) then
        call cgi_get_method( dict, length )
        method = 1
    else
        call get_environment_variable( "CONTENT_LENGTH", value=string, status=status )
        if ( status == 0 ) then
            read( string, * ) length
            call cgi_post_method( dict, length )
            method = 1
        else
            read( *, '(A)', advance = 'no' ) ch
            if ( ch == '%' ) then
                !
                ! TODO: better method for determining length
                call cgi_dustmote_method( dict )
                method = 2
            elseif ( index( '1234567890', ch ) > 0 ) then
   !            call cgi_simple_cgi( dict )
                method = -1
            else
                method = -1
            endif
        endif
    endif

    !
    ! If we did not get the correct information, just blow the
    ! whole thing off
    !
    if ( method == -1 ) then
        call cgi_error( "CGI protocol not recognised or not implemented" )
    endif

    !
    ! What LU-number for the output
    ! method 1: write directly to standard output (assumed to be at unit 6)
    ! method 2: write to a file first (cgiout)
    !
    if ( method == 1 ) then
        luout = 6
    endif
    if ( method == 2 ) then
        do luout = 10,99
            inquire( luout, opened = opend )
            if ( .not. opend ) then
                exit
            endif
        enddo
        open( luout, file = 'cgiout' )
    endif
    luout_cgi = luout

    !
    ! Write the header lines
    !
    select case ( html )
        case ( output_html, output_text )
            call cgi_header( html )
        case( output_html_delayed, output_text_delayed )
            ! TODO
        case( output_no_header )
            ! Writing the header is delayed, because the type is not known yet
        case default
            call cgi_error( "Programming error: wrong value for parameter 'html' in CGI_BEGIN" )
    end select

end subroutine cgi_begin

! cgi_header --
!     Write the CGI header information
!
! Arguments:
!     type           Type of header
!
subroutine cgi_header( type )
    integer, intent(in) :: type

    header_written = .true.

    select case ( type )
        case ( output_html, output_html_delayed )
            write( luout_cgi, '(a)' ) 'Content-Type: text/html;charset=iso8859-1'
            write( luout_cgi, '(a)' ) ''
        case( output_text, output_text_delayed )
            write( luout_cgi, '(a)' ) 'Content-Type: text/plain;charset=iso8859-1'
            write( luout_cgi, '(a)' ) ''
        case( output_no_header )
            call cgi_error( "Programming error: value 'output_no_header' not allowed in CGI_HEADER" )
        case default
            call cgi_error( "Programming error: wrong value for parameter 'type' in CGI_HEADER" )
    end select
end subroutine cgi_header

! cgi_get_method --
!     Get the information via the environment variable QUERY_STRING
!
! Arguments:
!     dict           Dictionary holding the information from the server (output)
!     length         Total length of the input
!
subroutine cgi_get_method( dict, length )
    type(DICT_STRUCT), pointer :: dict
    integer, intent(in)        :: length

    character(len=length)      :: buffer

    call get_environment_variable( "QUERY_STRING", value=buffer )
    call cgi_store_dict( dict, buffer )

end subroutine cgi_get_method

! cgi_post_method --
!     Get the information via standard input
!
! Arguments:
!     dict           Dictionary holding the information from the server (output)
!     length         Total length of the input
!
subroutine cgi_post_method( dict, length )
    type(DICT_STRUCT), pointer :: dict
    integer, intent(in)        :: length

    character(len=length)      :: buffer

    read( *, '(a)', advance='no' ) buffer
    call cgi_store_dict( dict, buffer )

end subroutine cgi_post_method

! cgi_dustmote_method --
!     Get the information line by line
!
! Arguments:
!     dict           Dictionary holding the information from the server (output)
!
!
subroutine cgi_dustmote_method( dict )
    type(DICT_STRUCT), pointer :: dict

    type(DICT_DATA)            :: data
    character(len=DICT_KEY_LENGTH)    :: key
    character(len=DICT_BUFFER_LENGTH) :: input
    integer                    :: k
    integer                    :: lu
    integer                    :: ierr
    logical                    :: opend

    read( *, '(a)' ) input ! Skip the remainder of the first line
    read( *, '(a)' ) input

    do while ( input /= '%END%' )
        call cgi_store_dict( dict, input )

        read( *, '(a)', iostat=ierr ) input
        if ( ierr /= 0 ) then
            exit
        endif
    enddo

end subroutine cgi_dustmote_method

! cgi_store_dict --
!     Store the information in the dictionary
!
! Arguments:
!     dict           Dictionary holding all information
!     string         Complete string received from CGI server
!
subroutine cgi_store_dict( dict, string )
    type(DICT_STRUCT), pointer      :: dict
    character(len=*), intent(in)    :: string

    character(len=DICT_KEY_LENGTH)  :: key
    character(len=len(string))      :: buffer
    type(DICT_DATA)                 :: data

    integer                         :: k
    integer                         :: keq

    buffer = string

    do
        k = index( buffer, '&' )
        if ( k .le. 0 ) then
            if ( buffer == ' ' ) then
                exit
            else
                k = len(buffer) + 1   ! Remaining piece
            endif
        endif

        !
        ! Store the string
        !
        keq = index( buffer(1:k-1), '=' )
        if ( keq > 0 ) then
            key = buffer(1:keq-1)
            call cgi_decode_string( key )
            data%value = buffer(keq+1:k-1)
            call cgi_decode_string( data%value )

            if ( .not. associated( dict ) ) then
                call dict_create( dict, key, data )
            else
                call dict_add_key( dict, key, data )
            endif
        endif

        if ( k < len(buffer) ) then
            buffer = buffer(k+1:)
        else
            buffer = ' '
        endif

    enddo
end subroutine cgi_store_dict

! cgi_decode_string --
!     Decode the string (replace + and %xx)
!
! Arguments:
!     dict           Dictionary holding all information
!     string         Complete string received from CGI server
!
subroutine cgi_decode_string( string )
    character(len=*), intent(inout) :: string

    integer :: k
    integer :: ch
    integer :: n
    integer :: p
    integer :: ierr

    !
    ! First the +'s
    !
    do
        k = index( string, '+' )
        if ( k .le. 0 ) exit

        string(k:k) = ' '
    enddo

    !
    ! Now %xx
    !
    n = len(string)
    p = 1
    do
        k = index( string(p:), '%' ) + p - 1
        if ( k .lt. p .or. k+2 .gt. n) exit

        read( string(k+1:k+2), '(z2)', iostat=ierr ) ch
        if ( ierr /= 0 ) then
            p = k + 1
            cycle
        endif
        string(k:) = achar(ch) // string(k+3:)
        n = n - 2
        p = k + 1
    enddo
end subroutine cgi_decode_string

! cgi_end --
!     Indicate to the server that we are done
! Arguments:
!     None
! Note:
!     This is simply done by writing a file cgiready,
!     if method 2 is used. Stop in all cases
!
subroutine cgi_end

    integer :: lu
    logical :: opend

    if ( method == 2 ) then
        do lu = 10,99
            inquire( lu, opened=opend )
            if ( .not. opend ) then
                open( lu, file = "cgiready" )
                close( lu )
                exit
            endif
        enddo
    endif

    stop

end subroutine cgi_end

! cgi_error --
!     Report a fatal error
! Arguments:
!     msg               Message to be printed
!     template          Template file to be used (optional)
!
subroutine cgi_error( msg, template )
    character(len=*), intent(in)           :: msg
    character(len=*), intent(in), optional :: template

    character(len=200)                     :: text
    integer :: k
    integer :: ierr
    integer :: lu
    logical :: opend
    logical :: exists

    exists = .false.
    if ( present(template) ) then
        inquire( file = template, exist = exists )
    endif

    if ( .not. header_written ) then
        write( luout_cgi, '(a)' ) 'Content-Type: text/html;charset=iso8859-1'
        write( luout_cgi, '(a)' ) ''
    endif

    if ( exists ) then
        do lu = 10,99
            inquire( lu, opened = opend )
            if ( .not. opend ) then
                exit
            endif
        enddo
        open( lu, file = template )

        do
            read( lu, '(a)', iostat=ierr ) text
            if ( ierr /= 0 ) exit

            k = index( text, 'MSG' )
            if ( k > 0 ) then
                write( luout_cgi, '(3a)' ) text(1:k-1), trim(msg), text(k+3:)
            else
                write( luout_cgi, '(a)' ) text
            endif
        enddo
        close( lu )
    else
        write( luout_cgi, * ) '<html>'
        write( luout_cgi, * ) '<head><title>Severe error</title></head>'
        write( luout_cgi, * ) '<body>'
        write( luout_cgi, * ) '<b>', trim(msg), '</b>'
        write( luout_cgi, * ) '</body></html>'
    endif

    call cgi_end

end subroutine cgi_error

! cgi_get_session --
!     Get the value of the "sessionid" variable
! Arguments:
!     dict          Dictionary with values
!     value         Value of the session ID (character(len=20))
! Note:
!     The session ID can be used to uniquely identify the
!     connection with the user. But it should be passed into the
!     HTML output as a hidden variable (see the documentation
!     for more information)
!
subroutine cgi_get_session( dict, value )
    type(DICT_STRUCT), pointer :: dict
    character(len=*)           :: value

    character(len=20)          :: time_string
    type(DICT_DATA)            :: data

    if ( dict_has_key( dict, "sessionid" ) ) then
        data = dict_get_key( dict, "sessionid" )
        value = data%value
    else
        call date_and_time( time = time_string )
        value = time_string(5:6) // time_string(8:10)
        data%value = value
        call dict_add_key( dict, "sessionid", data )
    endif

end subroutine cgi_get_session

! cgi_get_* --
!     Get the value of variables
! Arguments:
!     dict          Dictionary with values
!     varname       Name of the variable to retrieve
!     value         Value of the variable
! Note:
!     If the variable does not exist, then the value
!     is not changed! (Use dict_has_key() to check the
!     existence)
!
subroutine cgi_get_string( dict, varname, value )
    type(DICT_STRUCT), pointer :: dict
    character(len=*)           :: varname
    character(len=*)           :: value

    type(DICT_DATA)            :: data

    if ( dict_has_key( dict, varname ) ) then
        data = dict_get_key( dict, varname )
        value = data%value
    endif

end subroutine cgi_get_string

subroutine cgi_get_integer( dict, varname, value )
    type(DICT_STRUCT), pointer :: dict
    character(len=*)           :: varname
    integer                    :: value

    type(DICT_DATA)            :: data
    integer                    :: ierr
    integer                    :: new_value

    if ( dict_has_key( dict, varname ) ) then
        data = dict_get_key( dict, varname )
        read( data%value, *, iostat=ierr ) new_value
        if ( ierr == 0 ) then
            value = new_value
        endif
    endif

end subroutine cgi_get_integer

subroutine cgi_get_real( dict, varname, value )
    type(DICT_STRUCT), pointer :: dict
    character(len=*)           :: varname
    real                       :: value

    type(DICT_DATA)            :: data
    integer                    :: ierr
    real                       :: new_value

    if ( dict_has_key( dict, varname ) ) then
        data = dict_get_key( dict, varname )
        read( data%value, *, iostat=ierr ) new_value
        if ( ierr == 0 ) then
            value = new_value
        endif
    endif

end subroutine cgi_get_real

subroutine cgi_get_logical( dict, varname, value )
    type(DICT_STRUCT), pointer :: dict
    character(len=*)           :: varname
    logical                    :: value

    type(DICT_DATA)            :: data
    integer                    :: ierr
    integer                    :: new_value

    if ( dict_has_key( dict, varname ) ) then
        data = dict_get_key( dict, varname )
        read( data%value, *, iostat=ierr ) new_value
        if ( ierr == 0 ) then
            value = (new_value == 1)
        endif
    endif

end subroutine cgi_get_logical

end module cgi_protocol
