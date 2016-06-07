!
! This template is used when one base function takes 2 vstrings 
! as arguments.
! This template provides a function which takes a character string
! as the 2nd argument and call the associated vstring version.
! The core of the function simply upgrade the character string
! into a vstring, calls the base function and free the vstring.
!
! _TEMPLATE_ROUTINE_NAME_CHARSTRING : the name of the function which
!   takes the charstring as argument
! _TEMPLATE_ROUTINE_NAME_VSTRING : the name of the function which
!   takes the vstring as argument
!

function _TEMPLATE_ROUTINE_NAME_CHARSTRING ( dirname , filename ) result ( value )
  type ( t_vstring ) , intent(in) :: dirname
  character(len=*) , intent(in) :: filename
  type ( t_vstring ) :: value
  type ( t_vstring ) :: filename_vstring
  call vstring_new ( filename_vstring , filename )
  value = _TEMPLATE_ROUTINE_NAME_VSTRING ( dirname , filename_vstring )
  call vstring_free ( filename_vstring )
end function _TEMPLATE_ROUTINE_NAME_CHARSTRING

