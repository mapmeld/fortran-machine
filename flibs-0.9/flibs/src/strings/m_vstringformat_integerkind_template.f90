!
! This is a template to manage integer(kind= _FORMAT_INTEGER_KIND ) values.
! To use it, define the following preprocessing macros :
! _INTEGER_KIND : integer, the kind of integer to process
! _INTEGER_FORMAT_CHAR_ROUTINE : the name of the function which formats an
!   integer of kind _INTEGER_KIND against a given character(len=*) format
! _INTEGER_FORMAT_VSTRING_ROUTINE : the name of the function which formats an
!   integer of kind _INTEGER_KIND against a given vstring format
! _INTEGER_FORMAT_AUTO_ROUTINE : the name of the function which formats an
!   integer of kind _INTEGER_KIND against an automatic format
! _INTEGER_COMPUTE_AUTOFORMAT_ROUTINE : the name of a subroutine which
!   computes a character(len=*) automatic format
! _INTEGER_COMPUTE_CHARNB : the name of a function which
!   computes the minimum number of characters used to print an integer
!
!
! _INTEGER_FORMAT_CHAR_ROUTINE --
!   Returns a new vstring by formatting an integer with a character(len=*) format.
! Arguments :
!   value : integer of kind _INTEGER_KIND to format
!   fmt : format is used to compute the new string.
!
function _INTEGER_FORMAT_CHAR_ROUTINE ( value , fmt ) result ( newstring )
  integer (kind = _INTEGER_KIND ) , intent(in) :: value
  character(len=*) , intent(in) :: fmt
  type ( t_vstring ) :: newstring
    ! TODO : what if the logical is expected to be formatted with more than the maximum possible number of characters ?
  character(len=VSTRING_FORMAT_MAXIMUM_FORMAT_LENGTH) :: charstring
  write ( charstring , fmt ) value
  !
  ! Note:
  !   The "trim" removes all trailing blanks and, since fortran add
  !   blanks only at the left, the string result of trim(charstring) is
  !   the expected one.
  !
  call vstring_new ( newstring , trim(charstring) )
end function _INTEGER_FORMAT_CHAR_ROUTINE
!
! _INTEGER_FORMAT_VSTRING_ROUTINE --
!   Returns a new vstring by formatting an integer with a vstring format.
! Arguments :
!   value : integer of kind _INTEGER_KIND to format
!   fmt : format is used to compute the new string
!
function _INTEGER_FORMAT_VSTRING_ROUTINE ( value , fmt ) result ( newstring )
  integer (kind = _INTEGER_KIND ) , intent(in) :: value
  type ( t_vstring ) , intent(in) :: fmt
  type ( t_vstring ) :: newstring
  character(len=vstring_length(fmt)) :: fmt_charstring
  ! Downgrade the format to a character string
  call vstring_cast ( fmt , fmt_charstring )
  newstring = _INTEGER_FORMAT_CHAR_ROUTINE ( value , fmt_charstring )
end function _INTEGER_FORMAT_VSTRING_ROUTINE
!
! _INTEGER_FORMAT_AUTO_ROUTINE --
!   Returns a new vstring by formatting an integer with an automatic format.
!   With automatic format, the computed string has a minimum length, that is,
!   does not contain any blank and begins with "-" only if the value is a strictly
!   negative integer.
! Arguments :
!   value : integer of kind _INTEGER_KIND to format
!
function _INTEGER_FORMAT_AUTO_ROUTINE ( value ) result ( newstring )
  integer (kind = _INTEGER_KIND ) , intent(in) :: value
  type ( t_vstring ) :: newstring
  character(len=VSTRING_FORMAT_MAXIMUM_FORMAT_LENGTH) :: charstring
  ! auto_format : format to print the integer
  character(len=10) :: autoformat
  type ( t_vstring ) :: message
  integer :: nbchar
  !
  ! Compute the automatic format.
  !
  call _INTEGER_COMPUTE_AUTOFORMAT_ROUTINE ( value , autoformat )
  !
  ! Format the integer
  !
  nbchar = _INTEGER_COMPUTE_CHARNB ( value )
  if ( nbchar <= VSTRING_FORMAT_MAXIMUM_FORMAT_LENGTH ) then
     write ( charstring , autoformat ) value
     call vstring_new ( newstring , trim(charstring) )
  else
     call vstring_new ( message , &
          "The integer value could not be formatted into a vstring in _INTEGER_FORMAT_AUTO_ROUTINE ." )
     call vstringformat_error ( message = message )
     call vstring_free ( message )
  endif
end function _INTEGER_FORMAT_AUTO_ROUTINE
!
! _INTEGER_COMPUTE_AUTOFORMAT_ROUTINE --
!   Computes an automatic format for the given value.
! Arguments:
!   value : an integer of kind _INTEGER_KIND
!   autoformat : the output character(len=*) string which contains
!     the automatic format.
!
subroutine _INTEGER_COMPUTE_AUTOFORMAT_ROUTINE ( value , autoformat )
  integer (kind = _INTEGER_KIND ), intent(in) :: value
  character(len=*), intent(out) :: autoformat
  type ( t_vstring ) :: message
  integer :: nbchar
  nbchar = _INTEGER_COMPUTE_CHARNB ( value )
  !
  ! Compute the automatic format.
  ! If value = 0 or 5, the auto_format is "(I1)".
  ! If value = -5 or 15, the auto_format is "(I2)".
  ! If value = -50 or 150, the auto_format is "(I3)".
  ! etc...
  ! Note :
  ! A negative 32-bit integer requires 11 characters.
  !
  if ( nbchar < 10 ) then
     write ( autoformat , "(A2,I1,A1)") "(I" , nbchar , ")"
  elseif ( nbchar < 100 ) then
     write ( autoformat , "(A2,I2,A1)") "(I" , nbchar , ")"
  else
     call vstring_new ( message , &
          "The automatic format for the integer value could not be computed in vstring_formatauto_integer." )
     call vstringformat_error ( message = message )
     call vstring_free ( message )
  endif
end subroutine _INTEGER_COMPUTE_AUTOFORMAT_ROUTINE
!
! _INTEGER_COMPUTE_CHARNB --
!   Returns the number of characters to print the given integer.
!   If the sign of the integer is positive, the "+" is not printed.
!   If the sign of the integer is negative, the "-" is printed.
! Arguments:
!   value : the integer of kind _INTEGER_KIND
!
function _INTEGER_COMPUTE_CHARNB ( value ) result ( nbchar )
  integer (kind = _INTEGER_KIND ), intent (in) :: value
  integer :: nbchar
  integer (kind = _INTEGER_KIND ) :: valueanalyzed
  if (value < 0) then
     nbchar = 2
     valueanalyzed = - value
  else
     nbchar = 1
     valueanalyzed = value
  end if
  do
     if ( valueanalyzed < 10 ) then
        exit
     endif
     valueanalyzed = valueanalyzed / 10
     nbchar = nbchar + 1
  end do
end function _INTEGER_COMPUTE_CHARNB

