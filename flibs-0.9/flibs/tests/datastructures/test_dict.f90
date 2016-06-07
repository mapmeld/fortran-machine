! test_dict.f90 --
!     Test program for dictionaries
!
!     $Id: test_dict.f90,v 1.3 2007/01/26 09:56:44 arjenmarkus Exp $
!
module MYDATA_MODULE

!
! The length of the keys
!
integer, parameter :: DICT_KEY_LENGTH = 20

!
! The data that will be stored with each key
type MYDATA
    character(len=20) :: string
end type MYDATA

!
! The "null" value for these data
!
type(MYDATA), parameter :: DICT_NULL = mydata( '' )

end module

module MYDATA_DICTS
    use MYDATA_MODULE, DICT_DATA => MYDATA
    implicit none

    include "dictionary.f90"
end module MYDATA_DICTS

program test_dict
    use MYDATA_DICTS

    implicit none

    type(DICT_STRUCT), pointer     :: dict
    type(DICT_DATA)                :: data
    character(len=DICT_KEY_LENGTH) :: key

    !
    ! Create a small dictionary
    !
    data%string = 'Xylophone'
    call dict_create( dict, 'X', data )

    data%string = 'Qi'
    call dict_add_key( dict, 'Q', data )

    data%string = 'Piano-forte'
    call dict_add_key( dict, 'P', data )


    !
    ! Retrieve a particular key ...
    !
    write(*,*) 'Has "A"? ', dict_has_key(dict, 'A' )
    write(*,*) 'Has "Q"? ', dict_has_key(dict, 'Q' )

    !
    ! Retrieve the value for 'P'
    !
    write(*,*) 'P = ', dict_get_key(dict, 'P' )

    !
    ! Remove that key and check again
    !
    call dict_delete_key( dict, "P" )
    write(*,*) 'Still has "P"? ', dict_has_key(dict, 'P' )

    !
    ! Adding a few longer keys
    !
    call dict_add_key( dict, "Piano", data )
    call dict_add_key( dict, "Piano2", data )
    call dict_add_key( dict, "Piano-forte is long ", data )
    !                         12345678901234567890
    write(*,*) 'Has key "Piano"? ', dict_has_key(dict, 'Piano' )
    write(*,*) 'Has key "Piano2"? ', dict_has_key(dict, 'Piano2' )
    write(*,*) 'Has key "Piano-forte is long "? ', &
        dict_has_key(dict, 'Piano-forte is long ' )
    !
    ! Destroy the dictionary
    !
    call dict_destroy( dict )

end program
