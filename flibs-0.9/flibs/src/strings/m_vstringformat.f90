!
! m_vstringformat --
!
!   The module m_vstringformat provides services to format one string, that is to 
!   say to convert a basic fortran data type into a dynamic vstring.
!   The current version of m_vstringformat can handle logical variables
!   and integers of all kinds. The format can be given by the client code,
!   or computed automatically. The format can be given as a basic character 
!   string, or as a vstring.
!
!   Overview
!   The only method provided here is vstring_format, which takes one basic 
!   basic fortran data type as the first argument (and, optionnaly, a format),
!   and returns a dynamic string which represents that value.
!   
!   Formatting logical values
!   One can format a logical into a vstring with automatic or explicit format.
!   In the following sample, one format the ".false." logical into a vstring.
!   After formatting, the string "mystring" has a length of 1 and its content 
!   is "F".
!
!      logical :: mybool
!      type ( t_vstring ) :: mystring
!      mybool = .false.
!      mystring = vstring_format ( mybool )
!
!   In the following sample, one format the ".false." logical into a vstring 
!   with an explicit format given as a character string :
!
!      logical :: mybool
!      type ( t_vstring ) :: mystring
!      mybool = .false.
!      mystring = vstring_format ( mybool , "(L3)" )
!
!   After formatting, the string "mystring" has a length of 3 and its content 
!   is "  F".
!
!   In the following sample, one format the ".false." logical into a vstring 
!   with an explicit format given as a vstring :
!
!      logical :: mybool
!      type ( t_vstring ) :: mystring
!      type ( t_vstring ) :: myformat
!      mybool = .false.
!      call vstring_new ( myformat , "(L3)" )
!      mystring = vstring_format ( mybool , myformat )
!      call vstring_free ( myformat )
!
!   Formatting integer values
!   One can format an integer into a vstring, with automatic or explict format.
!   The string formatting system can handle all fortran kinds of integers.
!   The automatic format makes so that the resulting string has the 
!   length which exactly matches the number of digits necessary to represent 
!   the integer :
!   - if the integer is negative, the "-" sign is at the begining of the string,
!   - if the integer is positive, the "+" sign is not in the string.
!   In the following example, one formats the integer 2008 into a vstring.
!   After formatting, the string has a length of 4 and its content is the 
!   string "2008".
!
!      type ( t_vstring ) :: mystring
!      mystring = vstring_format ( 2008 )
!
!   To acheive the same effect, one can use the "I0" format of the fortran 90
!   standard :
!
!      type ( t_vstring ) :: mystring
!      mystring = vstring_format ( 2008 , "(I0)" )
!
!   The component can handle long integers with specified kinds 1, 2, 4 or 8.
!   In the following example, one automatically format a 32-bits integer 
!   (1073741824 = 2**31) into a dynamic string.
!
!      integer (kind=4) :: integerkind4 = 1073741824
!      mystring = vstring_format ( integerkind4 )
!
!   After formatting, the string has a length of 4 and its content is the 
!   string "1073741824".
!
!   Limitations
!   The current implementation of the string formatting is based 
!   on the standard "write" fortran statement, which takes 
!   a basic character string as argument.
!   Moreover the format given to the string formatting command 
!   is not analysed at all, so that the length of the resulting 
!   string is not computed.
!   The current component use therefore a character string of 
!   fixed length VSTRING_FORMAT_MAXIMUM_FORMAT_LENGTH.
!   This leads to at least two limitations :
!   - if the client code has to manage formated strings which length is over 
!     VSTRING_FORMAT_MAXIMUM_FORMAT_LENGTH characters, the 
!     current algorithm will generate an error at execution time.
!   - the "write" statement has to manage a long string, even
!     if the result is short, which raise performances issues.
!
!   The current string formatting takes only one value. 
!   An effective implementation would in fact take a list of
!   one or more arguments, which may be of different types.
!
!   The current implementation does not manage real values.
!
!   The limitations of the current component are so strong that the 
!   component should not be released... but is useful to format 
!   integers and that is why it has been finally released.
!
!   Planned features
!   It is expected that future releases of this component can manage 
!   more basic data types, including real types of all kinds.
!   Future releases may also extend the formats by taking into account
!   string formatting coming from other languages than fortran (for example C), 
!   including:
!   - the "-" specification which specifies that the converted argument 
!     should be left-justified in its field
!   - the "+" specification which specifies that a number should always 
!     be printed with a sign, even if positive.
!   - etc...
!
! Copyright (c) 2008 Michael Baudin michael.baudin@gmail.com
!
! $Id: m_vstringformat.f90,v 1.2 2008/06/12 16:23:47 relaxmike Exp $
!
!
module m_vstringformat
  use m_vstring, only : &
       t_vstring, &
       vstring_new, &
       vstring_free , &
       vstring_exists , &
       vstring_length , &
       vstring_range , &
       vstring_cast , &
       vstring_append
  implicit none
  private
  !
  ! Public methods
  !
  public :: vstring_format
  !
  ! vstring_format --
  !   Generic converter from a basic fortran data type into a vstring.
  !   Returns a new dynamic string with type t_vstring by formating the
  !   given value against the optional format fmt.
  ! Arguments :
  !   value : logical to format
  !     or integer of kind 1, 2, 4 or 8 to format
  !   fmt, optional : if provided, then this format is used to compute the 
  !     new string. If format is provided, it may be either with a character(len=*) type 
  !     or with a t_vstring type.
  !     If format is not provided, an automatic format is computed and applied 
  !     to compute the new string. With automatic format, the computed 
  !     string has a minimum length, that is, does not contain any blank and
  !     begins with "-" only if the value is a strictly negative integer.
  !
  interface vstring_format
     !
     ! Logical
     !
     module procedure vstring_format_auto_logical
     module procedure vstring_format_char_logical
     module procedure vstring_format_vstring_logical
     !
     ! Integer
     !
     module procedure vstring_format_auto_integer_kind1
     module procedure vstring_format_auto_integer_kind2
     module procedure vstring_format_auto_integer_kind4
     module procedure vstring_format_auto_integer_kind8
     module procedure vstring_format_char_integer_kind1
     module procedure vstring_format_char_integer_kind2
     module procedure vstring_format_char_integer_kind4
     module procedure vstring_format_char_integer_kind8
     module procedure vstring_format_vstring_integer_kind1
     module procedure vstring_format_vstring_integer_kind2
     module procedure vstring_format_vstring_integer_kind4
     module procedure vstring_format_vstring_integer_kind8
  end interface vstring_format
  !
  ! Set to true to stop whenever an error comes in the vstring component.
  logical, save :: vstringformat_stoponerror = .true.
  !
  ! Constants
  !
  integer, parameter :: VSTRING_FORMAT_MAXIMUM_FORMAT_LENGTH = 1000
contains
  !
  ! vstring_format_auto_logical --
  !   Returns a new vstring by formatting a logical with automatic format.
  !
  function vstring_format_auto_logical ( value ) result ( newstring )
    logical , intent(in) :: value
    type ( t_vstring ) :: newstring
    character(len=1) :: charlength1
    write ( charlength1 , "(l1)" ) value
    call vstring_new ( newstring , charlength1 )
  end function vstring_format_auto_logical
  !
  ! vstring_format_char_logical --
  !   Returns a new vstring by formatting a logical with the given format.
  !
  function vstring_format_char_logical ( value , fmt ) result ( newstring )
    logical , intent(in) :: value
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
  end function vstring_format_char_logical
  !
  ! vstring_format_vstring_logical --
  !   Returns a new vstring by formatting a logical with the given vstring format.
  !
  function vstring_format_vstring_logical ( value , fmt ) result ( newstring )
    logical , intent(in) :: value
    type ( t_vstring ) , intent(in) :: fmt
    type ( t_vstring ) :: newstring
    character(len=vstring_length(fmt)) :: fmt_charstring
    ! Downgrade the format to a character string
    call vstring_cast ( fmt , fmt_charstring )
    newstring = vstring_format_char_logical ( value , fmt_charstring )
  end function vstring_format_vstring_logical
  !
  ! vstring_format_auto_integer_kind1 --
  !   Format an integer(kind=1) into a vstring.
  ! Arguments :
  !   value : integer of kind 1 to format
  !   fmt, optional : if provided, then this format is used to compute the 
  !     new string.
  !     If not provided, an automatic format is computed and applied 
  !     to compute the new string. With automatic format, the computed 
  !     string has a minimum length, that is, does not contain any blank and
  !     begins with "-" only if the value is a strictly negative integer.
  !     The format may be provided either with a character(len=*) type 
  !     or with a t_vstring type.
  !
#define _INTEGER_FORMAT_VSTRING_ROUTINE vstring_format_vstring_integer_kind1
#define _INTEGER_FORMAT_CHAR_ROUTINE vstring_format_char_integer_kind1
#define _INTEGER_FORMAT_AUTO_ROUTINE vstring_format_auto_integer_kind1
#define _INTEGER_COMPUTE_AUTOFORMAT_ROUTINE vstring_compute_autoformat_integer_kind1
#define _INTEGER_COMPUTE_CHARNB vstring_format_intcharnb_kind1
#define _INTEGER_KIND 1
#include "m_vstringformat_integerkind_template.f90"
#undef _INTEGER_FORMAT_AUTO_ROUTINE
#undef _INTEGER_KIND
#undef _INTEGER_COMPUTE_AUTOFORMAT_ROUTINE
#undef _INTEGER_COMPUTE_CHARNB
#undef _INTEGER_FORMAT_CHAR_ROUTINE
#undef _INTEGER_FORMAT_VSTRING_ROUTINE
  !
  ! vstring_format_auto_integer_kind2 --
  !   Format an integer(kind=2) into a vstring.
  ! Arguments :
  !   value : integer of kind 2 to format
  !   fmt, optional : if provided, then this format is used to compute the 
  !     new string.
  !     If not provided, an automatic format is computed and applied 
  !     to compute the new string. With automatic format, the computed 
  !     string has a minimum length, that is, does not contain any blank and
  !     begins with "-" only if the value is a strictly negative integer.
  !     The format may be provided either with a character(len=*) type 
  !     or with a t_vstring type.
  !
#define _INTEGER_FORMAT_VSTRING_ROUTINE vstring_format_vstring_integer_kind2
#define _INTEGER_FORMAT_CHAR_ROUTINE vstring_format_char_integer_kind2
#define _INTEGER_FORMAT_AUTO_ROUTINE vstring_format_auto_integer_kind2
#define _INTEGER_COMPUTE_AUTOFORMAT_ROUTINE vstring_compute_autoformat_integer_kind2
#define _INTEGER_COMPUTE_CHARNB vstring_format_intcharnb_kind2
#define _INTEGER_KIND 2
#include "m_vstringformat_integerkind_template.f90"
#undef _INTEGER_FORMAT_AUTO_ROUTINE
#undef _INTEGER_KIND
#undef _INTEGER_COMPUTE_AUTOFORMAT_ROUTINE
#undef _INTEGER_COMPUTE_CHARNB
#undef _INTEGER_FORMAT_CHAR_ROUTINE
#undef _INTEGER_FORMAT_VSTRING_ROUTINE
  !
  ! vstring_format_auto_integer_kind4 --
  !   Format an integer(kind=4) into a vstring with automatic format.
  ! Arguments :
  !   value : integer of kind 8 to format
  !   fmt, optional : if provided, then this format is used to compute the 
  !     new string.
  !     If not provided, an automatic format is computed and applied 
  !     to compute the new string. With automatic format, the computed 
  !     string has a minimum length, that is, does not contain any blank and
  !     begins with "-" only if the value is a strictly negative integer.
  !     The format may be provided either with a character(len=*) type 
  !     or with a t_vstring type.
  !
#define _INTEGER_FORMAT_VSTRING_ROUTINE vstring_format_vstring_integer_kind4
#define _INTEGER_FORMAT_CHAR_ROUTINE vstring_format_char_integer_kind4
#define _INTEGER_FORMAT_AUTO_ROUTINE vstring_format_auto_integer_kind4
#define _INTEGER_COMPUTE_AUTOFORMAT_ROUTINE vstring_compute_autoformat_integer_kind4
#define _INTEGER_COMPUTE_CHARNB vstring_format_intcharnb_kind4
#define _INTEGER_KIND 4
#include "m_vstringformat_integerkind_template.f90"
#undef _INTEGER_FORMAT_AUTO_ROUTINE
#undef _INTEGER_KIND
#undef _INTEGER_COMPUTE_AUTOFORMAT_ROUTINE
#undef _INTEGER_COMPUTE_CHARNB
#undef _INTEGER_FORMAT_CHAR_ROUTINE
#undef _INTEGER_FORMAT_VSTRING_ROUTINE
  !
  ! vstring_format_auto_integer_kind8 --
  !   Format an integer(kind=8) into a vstring with automatic format.
  ! Arguments :
  !   value : integer of kind 8 to format
  !   fmt, optional : if provided, then this format is used to compute the 
  !     new string.
  !     If not provided, an automatic format is computed and applied 
  !     to compute the new string. With automatic format, the computed 
  !     string has a minimum length, that is, does not contain any blank and
  !     begins with "-" only if the value is a strictly negative integer.
  !     The format may be provided either with a character(len=*) type 
  !     or with a t_vstring type.
  !
#define _INTEGER_FORMAT_VSTRING_ROUTINE vstring_format_vstring_integer_kind8
#define _INTEGER_FORMAT_CHAR_ROUTINE vstring_format_char_integer_kind8
#define _INTEGER_FORMAT_AUTO_ROUTINE vstring_format_auto_integer_kind8
#define _INTEGER_COMPUTE_AUTOFORMAT_ROUTINE vstring_compute_autoformat_integer_kind8
#define _INTEGER_COMPUTE_CHARNB vstring_format_intcharnb_kind8
#define _INTEGER_KIND 8
#include "m_vstringformat_integerkind_template.f90"
#undef _INTEGER_FORMAT_AUTO_ROUTINE
#undef _INTEGER_KIND
#undef _INTEGER_COMPUTE_AUTOFORMAT_ROUTINE
#undef _INTEGER_COMPUTE_CHARNB
#undef _INTEGER_FORMAT_CHAR_ROUTINE
#undef _INTEGER_FORMAT_VSTRING_ROUTINE
  !
  ! vstringformat_error --
  !   Generates an error with the given error message
  !
  subroutine vstringformat_error ( this , message )
    implicit none
    type ( t_vstring ) , intent(in), optional :: this
    type ( t_vstring ) , intent (in) :: message
    character ( len = 40 ) :: this_chars
    type ( t_vstring ) :: this_subpart
    integer :: length
    integer, parameter :: length_max = 40
    logical :: isallocated
    character(len=1000) :: message_char
    write ( * , * ) "Internal error in m_vstring"
    if ( present ( this ) ) then
       length =  vstring_length ( this )
       isallocated = vstring_exists ( this )
       if ( isallocated ) then
          write ( * , * ) "Length:" , length
          if ( length < length_max ) then
             call vstring_cast ( this , this_chars )
             write ( * , * ) "Content:", trim(this_chars)
          else
             this_subpart = vstring_range ( this , 1 , 40 )
             call vstring_cast ( this_subpart , this_chars )
             write ( * , * ) "Content:", this_chars, "..."
             call vstring_free ( this_subpart )
          endif
       endif
    endif
    call vstring_cast ( message , message_char )
    write ( * , * ) "Message :", trim( message_char )
    if ( vstringformat_stoponerror ) then
       STOP
    endif
  end subroutine vstringformat_error
  ! 
  ! vstringformat_set_stoponerror --
  !   Configure the behaviour of the component whenever an 
  !   error is met.
  !   If stoponerror is true, then the execution stops if an error is encountered.
  !   If stoponerror is false, then the execution continues if an error is encountered.
  !   In both cases, a message is displayed on standard output.
  ! 
  subroutine vstringformat_set_stoponerror ( stoponerror )
    logical , intent(in) :: stoponerror
    vstringformat_stoponerror = stoponerror
  end subroutine vstringformat_set_stoponerror
end module m_vstringformat
