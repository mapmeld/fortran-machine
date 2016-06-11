! cgi.f90 --
!     Module for supporting CGI applications via the
!     simple CGI server script
!
!     $Id: dictionary.f90,v 1.2 2006/03/26 19:03:53 arjenmarkus Exp $
!
module cgi_protocol
    integer, parameter :: DICT_KEY_LENGTH   = 80
    integer, parameter :: DICT_VALUE_LENGTH = 200

    type DICT_DATA
        character(len=DICT_VALUE_LENGTH) :: value
    end type DICT_DATA

    interface cgi_get
        module procedure cgi_get_integer
        module procedure cgi_get_real
    end interface

    type(DICT_DATA), parameter :: dict_null = dict_data('')

!
! Body of source code for storing and retrieving the data
!
    include 'dictionary.f90'

!
! Routines specific to CGI_PROTOCOL
!

! cgi_begin --
!     Read the HTTP data from stdin - beginning of the processing
! Arguments:
!     dict          Variable that will hold all data
! Note:
!     Takes care of all details
!
subroutine cgi_begin( dict )
    type(DICT_STRUCT), pointer :: dict

    type(DICT_DATA)            :: data
    character(len=DICT_KEY_LENGTH)                     :: key
    character(len=DICT_KEY_LENGTH+DICT_VALUE_LENGTH+1) :: input
    integer                    :: k
    integer                    :: lu
    integer                    :: ierr
    logical                    :: opend

    if ( associated(dict) ) then
        call dict_destroy( dict )
    endif

    read( *, '(a)' ) input

    do while ( input /= '%END%' )
        k = index( input, "=" )
        if ( k > 0 ) then
            key        = input(1:k-1)
            data%value = input(k+1:)
            if ( .not. associated( dict ) ) then
                call dict_create( dict, key, data )
            else
                call dict_add_key( dict, key, data )
            endif
        endif

        read( *, '(a)', iostat=ierr ) input
        if ( ierr /= 0 ) then
            exit
        endif
    enddo

end subroutine cgi_begin

! cgi_end --
!     Indicate to the server that we are done
! Arguments:
!     None
! Note:
!     This is simply done by removing the file cgiwait
!
subroutine cgi_end

    integer :: lu
    logical :: opend

    do lu = 10,99
        inquire( lu, opened=opend )
        if ( .not. opend ) then
            open( lu, file = "cgiready" )
            close( lu )
            exit
        endif
    enddo

end subroutine cgi_end

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

end module cgi_protocol
