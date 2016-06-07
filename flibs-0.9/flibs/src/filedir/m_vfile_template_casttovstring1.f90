!
! This template is used when one base function takes one vstring 
! as an argument.
! This template provides a function which takes a character string
! as an argument and call the associated vstring version.
! The core of the function simply upgrade the character string
! into a vstring, calls the base function and free the vstring.
!
! _TEMPLATE_ROUTINE_NAME_CHARSTRING : the name of the function which
!   takes the charstring as argument
! _TEMPLATE_ROUTINE_NAME_VSTRING : the name of the function which
!   takes the vstring as argument
! _TEMPLATE_ROUTINE_VALUE : the type of data returned
!

function _TEMPLATE_ROUTINE_NAME_CHARSTRING ( filename ) result ( value )
  character(len=*) , intent(in) :: filename
  type ( t_vstring ) :: filename_vstring
  _TEMPLATE_ROUTINE_VALUE :: value
  call vstring_new ( filename_vstring , filename )
  value = _TEMPLATE_ROUTINE_NAME_VSTRING ( filename_vstring )
  call vstring_free ( filename_vstring )
end function _TEMPLATE_ROUTINE_NAME_CHARSTRING

