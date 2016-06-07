!
! m_vstring --
!   This module provides OO services to manage strings of dynamic length.
!   The goal of the current component is to provide higher-level
!   services that the standard fortran currently provides.
!   The component provides methods which mimic the services 
!   available in the Tcl language.
!   It provides string comparison, string search and string 
!   matching methods.
!   See in test_m_vstring to see a complete example of the 
!   services provided.
!
!   Overview
!   A vstring is an array of characters. 
!   The simplest way to create a vstring is with vstring_new 
!   from a "character(len=<something>)" string.
!   The length of the vstring is computed dynamically,
!   depending on the current number of characters, with vstring_length.
!   In the following example, the length is 9.
!  
!     use m_vstring, only : &
!       vstring_new, &
!       vstring_free, &
!       t_vstring, &
!       vstring_length
!     type ( t_vstring,) :: string1
!     integer :: length
!     call vstring_new ( string1 , "my string" )
!     length = vstring_length (string1)
!     call vstring_free( string1 )
!
!   Creating a string
!   With vstring_new, one can also create a new vstring as a copy 
!   of an existing vstring.
!   With vstring_new, one can also create a new vstring with an 
!   array of characters or with a repeated copy of an existing vstring.
!   Destroy the vstring with vstring_free.
!
!   Concatenate two strings
!   Two vstrings can be concatenated in two ways.
!   The vstring_concat method returns a new vstring computed by the 
!   concatenation of the two strings.
!   The vstring_append allows to add the characters of the 2nd vstring
!   at the end of the current string.
!   In the following example, the string3 is "my string is very interesting".
!
!     call vstring_new ( string1 , "my string" )
!     call vstring_new ( string2 , " is very interesting" )
!     string3 = vstring_concat ( string1 , string2 )
!
!   Modify the case
!   The user can modify the case of a vstring.
!   The vstring_tolower creates a new vstring with lower case characters.
!   The vstring_toupper creates a new vstring with upper case characters.
!   The vstring_totitle creates a new vstring with the first letter in
!   upper case and all the other characters to lower case.
!
!   The user can know if two vstrings are equal with vstring_equals.
!   Two vstrings can be compared with vstring_compare, which 
!   is based on the lexicographic order.
!
!   One can transform one vstring into a new one using a map with
!   vstring_map.
!
!   Pattern matching
!   The vstring_match method provides string-matching services in the glob-style.
!   It manages "*" pattern (which matches 0 or more characters), 
!   the "?" pattern (which matches exactly one character),
!   escape sequences and character ranges.
!   The following example show how to compare a file name against a pattern :
!
!     call vstring_new ( string1 , "m_vstring.f90" )
!     call vstring_new ( pattern , 'm_*.f90' )
!     match = vstring_match ( string1 , pattern )
!
!   Validating a string
!   The vstring_is method provides a way of validating data by 
!   computing whether the vstring is in a class of data, for example 
!   integer, real, digit, alphanumeric, etc...
!   In the following example, the user can check whether the 
!   string read on standard input is an integer :
!
!     read ( 5 , * ) charstring
!     call vstring_new ( string1 , charstring )
!     isinteger = call vstring_is ( string1 , "integer" )
!     if ( .NOT. isinteger ) then
!       ! Generate an error
!     endif
!
!   If the character set under use is not in one the pre-defined classes 
!   of vstring_is, the user can directly call vstring_isincharset or 
!   vstring_isinasciirange, which are the basic blocks of vstring_is.
!
!   Second string argument may be character string
!   The design choice has been made to design the subroutines/functions
!   so that their dummy arguments are generally only of type t_vstring.
!   Another choice would have been to allways take as dummy arguments both
!   t_vstring and "character (len=*)" strings, with module procedure 
!   interfaces to make them generic.
!   The last choice ease the work of the client of the current component,
!   which can use directly standard fortran constant strings (for example,
!
!     equals = vstring_equals ( string1 , "toto" )
!
!   instead of
!
!     type ( t_vstring ) :: string2
!     call vstring_new ( string2 , "toto" )
!     equals = vstring_equals ( string1 , string2 )
!     call vstring_free ( string2 )
!
!   that is to say 5 lines instead of 1.
!
!   The main drawback is that the number of interfaces is at least 
!   multiplied by 2, if not 4 or 8 when the number of string arguments is more 
!   than 2. This makes the unit tests multiplied by the same number, if one 
!   want to exercise all the possible interfaces. That way was chosen
!   by the original iso_varying_string module and lead to a heavy component,
!   with a large number of lines and a small number of features, because 
!   all the time was lost in the management of such an heavy module.
!   The other drawback is that is breaks the object oriented design
!   so that the "type bound" procedure of F2003 cannot be used.
!   The current choice is to focus mainly on the services provided,
!   not the ease of use. That allows to provide much more features than
!   in the original component, but complicates a little more the 
!   use in the client code. 
!   The choice done here is that the first argument is allways of type 
!   vstring (and called "this"), while the second argument (if any), mays 
!   by either of type vstring or of type "character (len=*).
!   That solution allows to keep both consistency and ease of use at 
!   the maximum possible level.
!   Several methods are designed this way, for example, vstring_equals, 
!   vstring_compare, vstring_append, vstring_concat and others.
!
!   Allocatable or pointer
!   Two implementation of m_vstring are provided, depending on the compiler used :
!   - the allocatable array of characters with the pre-processing macro _VSTRING_ALLOCATABLE,
!   - the pointer array of characters with the pre-processing macro _VSTRING_POINTER
!   If none of the macros are defined, the default implementation is _VSTRING_ALLOCATABLE.
!   The two implementations provide exactly the same services.
!   But the "allocatable" implementation allows to manage the vstring
!   which are going out of the current scope so that the use of vstring_free
!   is not necessary and memory leaks do not occur.
!   Instead, with the pointer implementation, the call to vstring_free is 
!   strictly necessary (if not, memory is lost each time a new vstring is
!   created).
!   The current version of m_vstring has been tested with the 
!   following compilers and versions !
!   - Intel Visual Fortran 8 : tested with _VSTRING_ALLOCATABLE and _VSTRING_POINTER
!       But the allocatable version allows to debug more easily.
!   - gfortran 2007/04/16 : tested with _VSTRING_POINTER (works fine,
!       except for vstring_match)
!   - g95 May  3 2007 : tested with _VSTRING_POINTER, OK
!
!   Dynamic or static buffer
!   The internal algorithms provided by m_vstrings are based on 
!   basic fortran character strings. In several situations, the 
!   dynamic vstring has to be converted into a basic fortran character
!   buffer string, which size has to be given explicitely in the source 
!   code, with the len = <something> statement (in the 
!   character ( len = <something>) ). Two solutions are provided, 
!   and the user can define the pre-processing macro 
!   _VSTRING_STATIC_BUFFER to configure that :
!   - the first solution is to set the size of the buffer statically,
!     to a constant integer value VSTRING_BUFFER_SIZE.
!   - the second solution is to compute the size 
!     of the buffer dynamicaly, with the fortran 90 len = vstring_length(this)
!     statement,
!   If the _VSTRING_STATIC_BUFFER is defined, then character strings of 
!   constant size are used as buffers.
!   If the _VSTRING_STATIC_BUFFER is not defined (which is the default), 
!   then character strings of dynamic size are used as buffers.
!   The second solution is more efficient, because the strings are not 
!   oversized or undersized, depending on the real number of characters
!   in the dynamic string. But the feature may not be provided 
!   by the compiler at hand. For example, problems with the dynamic 
!   length character string have been experienced with Intel Fortran 8.
!
!   Design
!   This component has been designed with OO principles in mind.
!   This is why the first argument of every method is named "this",
!   which is the current object.
!   If another string is required as a second argument, it may be either 
!   of type dynamic or as a character(len=*) type, to improve
!   usability.
!   This component is meant to evolve following the fortran 2003 standard 
!   and OO type-bound procedures.
!
!   Limitations
!     - No regular expression algorithm is provided.
!       But vstring_match allows to do string matching in glob-style.
!     - The vstring_match does not work with gfortran 2007/04/16 because 
!       of a limitation in gfortran for zero-size arrays
!     - the vstring_adjustl, vstring_adjustr, vstring_scan, 
!       vstring_adjustl, vstring_adjustr, vstring_is methods does not
!       work with IVF8 because strings declared like this :
!         character (len = vstring_length(this) :: character
!       are not consistent strings, probably because of a bug
!       in the implementation of len = pure function value in IVF8.
!     - Fortran does not allow to manage character encodings such as UTF8.
!
!   Preprocessing
!   The following preprocessing macro must be considered :
!   _VSTRING_STATIC_BUFFER : see  the section "Dynamic or static buffer"
!   _VSTRING_ALLOCATABLE or _VSTRING_POINTER : see the section "Allocatable or pointer"
!
!   History
!   This module was originally based on the iso_varying_string.f90 module 
!   by Rich Townsend.
!
!   TODO
!   - Refactor the component and use datastructures/vectors.f90
!
! ******************************************************************************
! *                                                                            *
! * iso_varying_string.f90                                                     *
! *                                                                            *
! * Copyright (c) 2003, Rich Townsend <rhdt@bartol.udel.edu>                   *
! * All rights reserved.                                                       *
! *                                                                            *
! * Redistribution and use in source and binary forms, with or without         *
! * modification, are permitted provided that the following conditions are     *
! * met:                                                                       *
! *                                                                            *
! *  * Redistributions of source code must retain the above copyright notice,  *
! *    this list of conditions and the following disclaimer.                   *
! *  * Redistributions in binary form must reproduce the above copyright       *
! *    notice, this list of conditions and the following disclaimer in the     *
! *    documentation and/or other materials provided with the distribution.    *
! *                                                                            *
! * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS    *
! * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,  *
! * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR     *
! * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR           *
! * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,      *
! * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,        *
! * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR         *
! * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF     *
! * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING       *
! * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS         *
! * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.               *
! *                                                                            *
! ******************************************************************************
!
! Copyright (c) 2008 Michael Baudin michael.baudin@gmail.com
! Copyright (c) 2008 Arjen Markus arjenmarkus@sourceforge.net
!
! $Id: m_vstring.f90,v 1.9 2008/07/16 09:24:42 relaxmike Exp $
!
! Thanks    : Lawrie Schonfelder (bugfixes and design pointers), Walt Brainerd
!             (conversion to F).
! Note  : The current module does NOT conform to the API
!         specified in ISO/IEC 1539-2:2000 (varying-length strings for
!         Fortran 95).
!
module m_vstring
  implicit none
  private
  !
  ! t_vstring --
  !   A vstring is implemented as an array of characters.
  !   The length of the string is not stored. Instead,
  !   it is computed by vstring_length each time it is 
  !   necessary with the intrinsic "size". It the length
  !   was stored explicitely, it should be managed, which 
  !   could lead to bugs.
  !
  ! Choose your dynamic string system between _VSTRING_ALLOCATABLE , _VSTRING_POINTER
  ! Allocatable arrays should be used by default.
  ! But for compatibility of older fortran 90 compilers, pointers are also available.
  ! These are the recommended settings :
  ! _VSTRING_POINTER : gfortran, g95
  ! _VSTRING_ALLOCATABLE : Intel Fortran 8.0
  !
  type, public :: t_vstring
     private
#ifdef _VSTRING_ALLOCATABLE
     character(LEN=1), dimension(:), allocatable :: chars
#endif
#ifdef _VSTRING_POINTER
     character(LEN=1), dimension(:), pointer :: chars => NULL()
#endif
  end type t_vstring
  !
  ! Public methods
  !
  public :: vstring_achar
  public :: vstring_adjustl
  public :: vstring_adjustr
  public :: vstring_exists
  public :: vstring_append
  public :: vstring_char
  public :: vstring_charindex
  public :: vstring_compare
  public :: vstring_concat
  public :: vstring_equals
  public :: vstring_first
  public :: vstring_free
  public :: vstring_iachar
  public :: vstring_ichar
  public :: vstring_index
  public :: vstring_last
  public :: vstring_length
  public :: vstring_map
  public :: vstring_match
  public :: vstring_new
  public :: vstring_random
  public :: vstring_range
  public :: vstring_reference_get
  public :: vstring_replace
  public :: vstring_reverse
  public :: vstring_scan
  public :: vstring_tolower
  public :: vstring_totitle
  public :: vstring_toupper
  public :: vstring_trim
  public :: vstring_trimleft
  public :: vstring_trimright
  public :: vstring_verify
  public :: vstring_is
  public :: vstring_isincharset
  public :: vstring_isinasciirange
  public :: vstring_set_stoponerror
  public :: vstring_cast
  !
  ! vstring_new --
  !   Generic constructor
  !
  interface vstring_new
     module procedure vstring_new_empty
     module procedure vstring_new_from_charstring
     module procedure vstring_new_from_vstring
     module procedure vstring_new_from_chararray
     module procedure vstring_new_from_integer
  end interface vstring_new
  !
  ! vstring_cast --
  !   Generic converter from a vstring to a basic fortran data type 
  !
  interface vstring_cast
     module procedure vstring_cast_charstringfixed
     module procedure vstring_cast_charstringauto
     module procedure vstring_cast_tointeger
     module procedure vstring_cast_toreal
     module procedure vstring_cast_todp
  end interface vstring_cast
  !
  ! vstring_equals --
  !   Generic comparison between two strings
  !
  interface vstring_equals
     module procedure vstring_equals_vstring
     module procedure vstring_equals_charstring
  end interface vstring_equals
  !
  ! vstring_compare --
  !   Generic comparison between two strings.
  !
  interface vstring_compare
     module procedure vstring_compare_vstring
     module procedure vstring_compare_charstring
  end interface vstring_compare
  !
  ! vstring_append --
  !   Generic append to a vstring.
  !
  interface vstring_append
     module procedure vstring_append_vstring
     module procedure vstring_append_charstring
  end interface vstring_append
  !
  ! vstring_concat --
  !   Generic concatenate to a vstring.
  !
  interface vstring_concat
     module procedure vstring_concat_vstring
     module procedure vstring_concat_charstring
  end interface vstring_concat
  !
  ! vstring_match --
  !   Generic string matching.
  !
  interface vstring_match
     module procedure vstring_match_vstring
     module procedure vstring_match_charstring
  end interface vstring_match
  !
  ! vstring_trim --
  !   Generic string trim.
  !
  interface vstring_trim
     module procedure vstring_trim_vstring
     module procedure vstring_trim_charstring
  end interface vstring_trim
  !
  ! vstring_trimleft --
  !   Generic string trim.
  !
  interface vstring_trimleft
     module procedure vstring_trimleft_vstring
     module procedure vstring_trimleft_charstring
  end interface vstring_trimleft
  !
  ! vstring_trimright --
  !   Generic string trim.
  !
  interface vstring_trimright
     module procedure vstring_trimright_vstring
     module procedure vstring_trimright_charstring
  end interface vstring_trimright
  !
  ! vstring_first --
  !   Generic string search first.
  !
  interface vstring_first
     module procedure vstring_first_vstring
     module procedure vstring_first_charstring
  end interface vstring_first
  !
  ! vstring_last --
  !   Generic string search last.
  !
  interface vstring_last
     module procedure vstring_last_vstring
     module procedure vstring_last_charstring
  end interface vstring_last
  !
  ! Constants
  !
  integer, parameter, public :: VSTRING_COMPARE_LOWER = -1
  integer, parameter, public :: VSTRING_COMPARE_EQUAL = 0
  integer, parameter, public :: VSTRING_COMPARE_GREATER = 1
  integer, parameter, public :: VSTRING_INDEX_UNKNOWN = 0
  character(len=*), parameter, public :: VSTRING_DIGITS = "0123456789"
  character(len=*), parameter, public :: VSTRING_UPPER = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  character(len=*), parameter, public :: VSTRING_LOWER = "abcdefghijklmnopqrstuvwxyz"
  character(len=*), parameter, public :: VSTRING_CHARACTERSET = VSTRING_LOWER//VSTRING_DIGITS
  character(len=*), parameter, public :: VSTRING_SPACE           = achar(32)
  ! Ascii char #10 -> corresponds to \n : line_feed
  character(len=*), parameter, public :: VSTRING_NEWLINE         = achar(10)
  ! Ascii char #13 -> corresponds to \r : carriage return
  character(len=*), parameter, public :: VSTRING_CARRIAGE_RETURN = achar(13)
  ! Ascii char #9 -> corresponds to \t : tab
  character(len=*), parameter, public :: VSTRING_TAB             = achar(9)
  ! Ascii char #34 -> corresponds to "
  character(len=*), parameter, public :: VSTRING_DOUBLEQUOTE             = achar(34)
  ! Ascii char #39 -> corresponds to '
  character(len=*), parameter, public :: VSTRING_SINGLEQUOTE             = achar(39)
  character(len=*), parameter, public :: VSTRING_WHITESPACE = VSTRING_SPACE//VSTRING_NEWLINE//VSTRING_CARRIAGE_RETURN//VSTRING_TAB
  character(len=*), parameter, public :: VSTRING_HEXDIGITS = "abcdefABCDEF"//VSTRING_DIGITS
  character(len=*), parameter, public :: VSTRING_PUNCTUATION = "_,;:.?![](){}@"//VSTRING_DOUBLEQUOTE//VSTRING_SINGLEQUOTE
  !
  ! Maximum number of characters in the buffer.
  !
  integer , parameter :: VSTRING_BUFFER_SIZE = 1000
  !
  ! Static parameters
  !
  logical, save :: random_process_initialize = .false.
  !
  ! Total number of currently available (allocated) strings.
  ! Note :
  ! This is mainly for debugging purposes of the vstring module itself or client algorithms.
  ! It allows to check the consistency of vstring_new/vstring_free statements.
  integer, save :: vstring_number_of_strings = 0
  !
  ! Set to true to stop whenever an error comes in the vstring component.
  logical, save :: vstring_stoponerror = .true.
  ! 
  ! TODO : Flags for error management.
  !
  integer, parameter :: VSTRING_ERROR_OK = 0
  integer, parameter :: VSTRING_ERROR_WRONGINDEX = 1
  integer, parameter :: VSTRING_ERROR_STRINGNOTCREATED = 2
contains
  !
  ! vstring_new_empty --
  !   Constructor based on an empty string.
  !   The created vstring has length 0 and no character.
  !
  subroutine vstring_new_empty ( this )
    type ( t_vstring ) , intent(inout) :: this
    call vstring_new_from_charstring ( this , "" )
  end subroutine vstring_new_empty
  !
  ! vstring_new_from_charstring --
  !   Constructor based on a character(len=*).
  ! Arguments
  !   char_string : the new vstring is filled with the 
  !     characters found in char_string.
  !
  subroutine vstring_new_from_charstring ( this , char_string )
    type ( t_vstring ) , intent(inout) :: this
    character(LEN=*), intent(in)    :: char_string
    character ( len = 200) :: message
    integer :: length
    integer :: icharacter
    logical :: isallocated
    integer :: this_length
    !
    ! Check that the data is empty
    !
    isallocated = vstring_exists ( this )
    if ( isallocated ) then
       this_length = vstring_length(this)
       write ( message , * ) "Object is allready associated with size ", this_length
       call vstring_error ( this , message , "vstring_new_from_charstring" )
    endif
    length = LEN ( char_string )
    allocate ( this % chars(length))
    do icharacter = 1, length
       this % chars(icharacter) = char_string (icharacter:icharacter)
    enddo
    !
    ! Update the counter of strings
    !
    call vstring_reference_add ()
  end subroutine vstring_new_from_charstring
  !
  ! vstring_new_from_vstring --
  !   Constructor based on a vstring ( simple copy ).
  !
  subroutine vstring_new_from_vstring ( this , vstring )
    type ( t_vstring ) , intent(inout) :: this
    type ( t_vstring ) , intent(in) :: vstring
    integer :: length
    character ( len = 200) :: message
    integer :: icharacter
    integer :: this_length
    integer :: status
    call vstring_check_string ( vstring , "vstring_new_from_vstring" , status )
    if ( status /= VSTRING_ERROR_OK ) then
       return
    endif
    !
    ! Check that the data is empty
    !
    if ( vstring_exists ( this ) ) then
       this_length = vstring_length(this)
       write ( message , * ) "Object is allready associated with size ", this_length , &
            " in vstring_new_from_charstring"
       call vstring_error ( this , message )
    endif
    length = vstring_length ( vstring )
    allocate ( this % chars ( length ) )
    do icharacter = 1 , length
       this % chars(icharacter) = vstring % chars (icharacter)
    enddo
    !
    ! Update the counter of strings
    !
    call vstring_reference_add ()
  end subroutine vstring_new_from_vstring
  !
  ! vstring_new_from_chararray --
  !   Constructor based on an array of characters
  !
  subroutine vstring_new_from_chararray ( this , chararray )
    type ( t_vstring ) , intent(inout) :: this
    character(len=1), dimension(:), intent(in) :: chararray
    integer :: length
    character ( len = 300 ) :: message
    integer :: icharacter
    integer :: this_length
    !
    ! Check that the data is empty
    !
    if ( vstring_exists ( this ) ) then
       this_length = vstring_length(this)
       write ( message , * ) "Object is allready associated with size ", this_length , &
            " in vstring_new_from_chararray"
       call vstring_error ( this , message )
    endif
    length = size ( chararray )
    allocate ( this % chars ( length ) )
    do icharacter = 1, length
       this % chars(icharacter) = chararray (icharacter)(1:1)
    enddo
    !
    ! Update the counter of strings
    !
    call vstring_reference_add ()
  end subroutine vstring_new_from_chararray
  !
  ! vstring_new_from_integer --
  !   Repeat the string ncount times and concatenate the result to create the new string.
  !   If not given, the default string is the blank space.
  ! Note :
  !   This can be considered as an implementation of "vstring_repeat".
  !
  subroutine vstring_new_from_integer ( this , ncount , string )
    type ( t_vstring ) , intent(inout) :: this
    integer, intent(in) :: ncount
    type ( t_vstring ) , intent(in), optional :: string
    integer :: length
    character ( len = 300 ) :: message
    type(t_vstring) :: string_real
    integer :: icount
    integer :: string_length
    integer :: first
    integer :: last
    integer :: this_length
    !
    ! Check that the data is empty
    !
    if ( vstring_exists ( this ) ) then
       this_length = vstring_length(this)
       write ( message , * ) "Object is allready associated with size ", this_length , &
            " in vstring_new_from_chararray"
       call vstring_error ( this , message )
    endif
    !
    ! Check the value of the integer
    !
    if ( ncount < 0 ) then
       write ( message , * ) "The given number of counts ", ncount , " is inconsistent ", &
            " in vstring_new_from_chararray"
       call vstring_error ( this , message )
    endif
    !
    ! Process options
    !
    if (present ( string ) ) then
       call vstring_new ( string_real , string )
    else
       call vstring_new ( string_real , VSTRING_SPACE )
    endif
    !
    ! Create the object
    !
    string_length = vstring_length ( string_real )
    length = ncount * string_length
    allocate ( this % chars ( length ) )
    do icount = 1, ncount
       first = string_length * ( icount - 1 ) + 1
       last = string_length * icount
       this % chars( first : last ) = string_real % chars ( 1 : string_length )
    enddo
    !
    ! Cleanup
    !
    call vstring_free ( string_real )
    !
    ! Update the counter of strings
    !
    call vstring_reference_add ()
  end subroutine vstring_new_from_integer
  !
  ! vstring_free --
  !   Destructor.
  ! NOTE :
  !   The use of the destructor is OPTIONAL.
  !   See the thread " New ISO_VARYING_STRING implementation 
  !   (without memory leaks)" on comp.lang.fortran :
  !   "On most systems, memory is memory :-).  However, there is a
  !   difference between how ALLOCATABLE variables and POINTER
  !   variables are handled.  ALLOCATABLE variables are always
  !   deallocated automatically when thay go out of scope (unless
  !   they have the SAVE attribute).  POINTER variables usually
  !   are not.  The reason is that the program may have associated
  !   additional pointers, that aren't going out of scope, with the
  !   same target as the one that is."
  !
  subroutine vstring_free ( this )
    type ( t_vstring ) , intent(inout) :: this
    logical :: string_allocated
    character ( len = 300 ) :: message
    integer :: status
    string_allocated = vstring_exists ( this )
    if ( string_allocated ) then
       deallocate ( this % chars , stat=status )
       if ( status /= 0 ) then
          write ( message , * ) "There was an error while deallocating the string."
          call vstring_error ( this , message , "vstring_free" )
       endif
#ifdef _VSTRING_POINTER
       nullify ( this % chars )
#endif
       !
       ! Update the counter of strings
       !
       call vstring_reference_remove ()
    else
       write ( message , * ) "The current varying string is not allocated ", &
            " in vstring_free"
       call vstring_error ( this , message , "vstring_free" )
    endif
  end subroutine vstring_free
  !
  ! vstring_reference_add --
  !   Static method.
  !   Increase the counter of currently referenced strings.
  !
  subroutine vstring_reference_add ( )
    implicit none
    vstring_number_of_strings = vstring_number_of_strings + 1
  end subroutine vstring_reference_add
  !
  ! vstring_reference_remove --
  !   Static method.
  !   Decrease the counter of currently referenced strings.
  !
  subroutine vstring_reference_remove ( )
    implicit none
    vstring_number_of_strings = vstring_number_of_strings - 1
  end subroutine vstring_reference_remove
  !
  ! vstring_reference_get --
  !   Static method.
  !   Returns the number of currently referenced strings.
  !
  integer function vstring_reference_get ( )
    implicit none
    vstring_reference_get = vstring_number_of_strings
  end function vstring_reference_get
  !
  ! vstring_exists --
  !   Returns .true. if the string is allocated.
  !
  pure function vstring_exists ( this ) result ( exists )
    type ( t_vstring ) , intent(in) :: this
    logical :: exists
#ifdef _VSTRING_ALLOCATABLE
    exists = allocated ( this%chars )
#endif
#ifdef _VSTRING_POINTER
    exists = associated ( this%chars)
#endif
  end function vstring_exists
  !
  ! vstring_equals_vstring --
  !   Perform a character-by-character comparison of strings this and string2.
  !   Returns true if this and string2 are identical, or .false when not.
  !   If nocase is set to true, the case of the characters is not taken into account.
  !   The default behaviour is to take into account for case of characters.
  !   If length is specified, then only the first length characters are used in the comparison.
  !
  function vstring_equals_vstring ( this , string2 , nocase , length ) result (equals)
    type ( t_vstring ) , intent(in) :: this
    type ( t_vstring ) , intent(in) :: string2
    logical , intent (in), optional :: nocase
    integer , intent ( in ), optional :: length
    logical :: equals
    logical :: nocase_real
    integer :: length1
    integer :: length2
    integer :: icharacter
    type(t_vstring) :: char1
    type(t_vstring) :: char2
    type(t_vstring) :: char1_case
    type(t_vstring) :: char2_case
    integer :: length_real
    integer :: length_min
    integer :: last
    character ( len = 300 ) :: message
    integer :: status
    call vstring_check_string ( this , "vstring_equals" , status )
    if ( status /= VSTRING_ERROR_OK ) then
       return
    endif
    call vstring_check_string ( string2 , "vstring_equals" , status )
    if ( status /= VSTRING_ERROR_OK ) then
       return
    endif
    !
    ! Process options
    !
    if ( present ( nocase ) ) then
       nocase_real = nocase
    else
       nocase_real = .false.
    endif
    !
    ! If the length parameter is here, it must be consistent with both strings lengths
    !
    length1 = vstring_length ( this )
    length2 = vstring_length ( string2 )
    length_min = min ( length1 , length2 )
    if ( present ( length ) ) then
       if ( length > length_min ) then
          write ( message , * ) "The given number of characters to compare ", length ,&
               " does not match the number of characters present in both strings : ", length_min, &
               " in vstring_equals."
          call vstring_error ( this , message )
       endif
       length_real = length
    else
       length_real = vstring_length ( this )
    endif
    !
    ! Compare lengths, if the length optional parameter is not here.
    !
    equals = .true.
    if ( .NOT. present ( length ) ) then
       if (length1/=length2) then
          equals = .false.
       endif
    endif
    !
    ! Compare characters.
    !
    if ( equals ) then
       last = min ( length_real , length_min )
       do icharacter = 1 , last
          !
          ! Compute the character at index #icharacter, with modified case if necessary.
          !
          char1 = vstring_index ( this , icharacter )
          char2 = vstring_index ( string2 , icharacter )
          if ( nocase_real ) then
             char1_case = vstring_tolower ( char1 )
             char2_case = vstring_tolower ( char2 )
          else
             call vstring_new ( char1_case , char1 )
             call vstring_new ( char2_case , char2 )
          endif
          !
          ! Make the comparison
          !
          if ( char1_case % chars ( 1 )/=char2_case % chars ( 1 ) ) then
             equals = .false.
          endif
          call vstring_free ( char1 )
          call vstring_free ( char2 )
          call vstring_free ( char1_case )
          call vstring_free ( char2_case )
          if (.NOT.equals) then
             exit
          endif
       enddo
    endif
  end function vstring_equals_vstring
  !
  ! vstring_equals_charstring --
  !   Interface to vstring_equals_vstring to manage character string.
  !
  function vstring_equals_charstring ( this , string2 , nocase , length ) result (equals)
    type ( t_vstring ) , intent(in) :: this
    character(len=*), intent(in) :: string2
    logical , intent ( in ), optional :: nocase
    integer , intent ( in ), optional :: length
    logical                          :: equals
    type(t_vstring) :: vstring2
    call vstring_new ( vstring2 , string2 )
    equals = vstring_equals_vstring ( this , vstring2 , nocase , length )
    call vstring_free ( vstring2 )
  end function vstring_equals_charstring
  !
  ! vstring_cast_charstringfixed --
  !   Convert a varying string into a character string
  !   (fixed length)
  !   If the number of characters in the target charstring 
  !   is not large enough, the target charstring is truncated, that is, 
  !   contains only the first characters of the current dynamic string.
  !
  subroutine vstring_cast_charstringfixed ( this , length , char_string )
    type ( t_vstring ) , intent(in) :: this
    integer, intent(in)              :: length
    character ( LEN = length ) , intent(out) :: char_string
    integer :: length_this
    integer :: icharacter
    length_this = vstring_length ( this )
    do icharacter = 1, min ( length_this , length )
       char_string ( icharacter : icharacter ) = this % chars ( icharacter )
    end do
    !
    ! Pad with white spaces
    !
    do icharacter = length_this + 1 , length
       char_string ( icharacter : icharacter ) = VSTRING_SPACE
    end do
  end subroutine vstring_cast_charstringfixed
  !
  ! vstring_cast_charstringauto --
  !   Convert a varying string into a character string
  !   (automatic length)
  !   If the number of characters in the target charstring 
  !   is not large enough, the target charstring is truncated, that is, 
  !   contains only the first characters of the current dynamic string.
  !
  subroutine vstring_cast_charstringauto ( this , char_string )
    type ( t_vstring ) , intent(in) :: this
    character ( LEN = * ) , intent(out) :: char_string
    integer :: length_this
    integer :: icharacter
    integer :: charlength
    !
    ! Compute lengths
    !
    length_this = vstring_length ( this )
    charlength = len ( char_string )
    !
    ! Fill with characters
    !
    do icharacter = 1, min ( length_this , charlength )
       char_string ( icharacter : icharacter ) = this % chars ( icharacter )
    end do
    !
    ! Pad with white spaces
    !
    do icharacter = length_this + 1 , charlength
       char_string ( icharacter : icharacter ) = VSTRING_SPACE
    end do
  end subroutine vstring_cast_charstringauto
  !
  ! vstring_cast_tointeger --
  !   Returns the integer stored in the current string.
  !
  subroutine vstring_cast_tointeger ( this , value )
    type ( t_vstring ) , intent(in) :: this
    integer, intent(out) :: value
    character ( len = 200 ) :: message
#ifdef _VSTRING_STATIC_BUFFER
    character ( len = VSTRING_BUFFER_SIZE ) :: char_string
#else
    character ( len = vstring_length ( this ) ) :: char_string
#endif
    logical :: hastype
    hastype = vstring_is ( this , "integer" )
    if ( .NOT. hastype ) then
       write ( message , * ) "Current string is not an integer in vstring_cast_tointeger"
       call vstring_error ( this , message )
    else
       call vstring_cast ( this , char_string )
       read ( char_string , * , err = 100 , end = 100) value
    endif
    return
100 continue
    !
    ! An error was generated while reading in buffer.
    !
    write ( message , * ) "The string could not be converted to integer in vstring_cast_tointeger"
    call vstring_error ( this , message )
  end subroutine vstring_cast_tointeger
  !
  ! vstring_cast_toreal --
  !   Returns the real stored in the current string.
  !
  subroutine vstring_cast_toreal ( this , value )
    type ( t_vstring ) , intent(in) :: this
    real, intent(out) :: value
    character ( len = 200 ) :: message
#ifdef _VSTRING_STATIC_BUFFER
    character ( len = VSTRING_BUFFER_SIZE ) :: char_string
#else
    character ( len = vstring_length ( this ) ) :: char_string
#endif
    logical :: hastype
    hastype = vstring_is ( this , "real" )
    if ( .NOT. hastype ) then
       write ( message , * ) "Current string is not a real in vstring_cast_toreal"
       call vstring_error ( this , message )
    else
       call vstring_cast ( this , char_string )
       read ( char_string , * , err = 100 , end = 100) value
    endif
    return
100 continue
    !
    ! An error was generated while reading in buffer.
    !
    write ( message , * ) "The string could not be converted to real in vstring_cast_toreal"
    call vstring_error ( this , message )
  end subroutine vstring_cast_toreal
  !
  ! vstring_cast_todp --
  !   Returns the double precision stored in the current string.
  !
  subroutine vstring_cast_todp ( this , value )
    type ( t_vstring ) , intent(in) :: this
    double precision, intent(out) :: value
    character ( len = 500 ) :: message
#ifdef _VSTRING_STATIC_BUFFER
    character ( len = VSTRING_BUFFER_SIZE ) :: char_string
#else
    character ( len = vstring_length ( this ) ) :: char_string
#endif
    logical :: hastype
    hastype = vstring_is ( this , "real" )
    if ( .NOT. hastype ) then
       write ( message , * ) "Current string is not a real in vstring_cast_toreal"
       call vstring_error ( this , message )
    else
       call vstring_cast ( this , char_string )
       read ( char_string , * , err = 100 , end = 100) value
    endif
    return
100 continue
    !
    ! An error was generated while reading in buffer.
    !
    write ( message , * ) "The string could not be converted to real in vstring_cast_toreal"
    call vstring_error ( this , message )
  end subroutine vstring_cast_todp
  !
  ! vstring_length --
  !   Returns the length of the current dynamic string.
  !
  pure function vstring_length ( this ) result ( length )
    type ( t_vstring ) , intent(in) :: this
    integer                          :: length
    logical :: string_allocated
    string_allocated = vstring_exists ( this )
    if ( string_allocated ) then
       length = SIZE(this%chars)
    else
       length = 0
       ! One cannot use the error management system provided by vstring_error
       ! because it is convenient that vstring_length is pure,
       ! to set the dimension of automatic-length character strings.
       !write ( message , * ) "Object is not created in vstring_length"
       !call vstring_error ( this , message )
    endif
  end function vstring_length
  !
  ! vstring_concat_vstring --
  !   Returns a new string made by the concatenation of two dynamic strings.
  !
  function vstring_concat_vstring ( this , string2 ) result ( concat_string )
    type ( t_vstring ) , intent(in) :: this
    type ( t_vstring ) , intent(in) :: string2
    type(t_vstring)             :: concat_string
    integer                          :: len_string_a
    integer :: concat_length
    integer :: status
    call vstring_check_string ( this , "vstring_concat" , status )
    if ( status /= VSTRING_ERROR_OK ) then
       return
    endif
    call vstring_check_string ( string2 , "vstring_concat" , status )
    if ( status /= VSTRING_ERROR_OK ) then
       return
    endif
    len_string_a = vstring_length(this)
    concat_length = len_string_a+vstring_length(string2)
    call vstring_new ( concat_string , concat_length )
    concat_string % chars(:len_string_a) = this%chars
    concat_string % chars(len_string_a+1:) = string2%chars
  end function vstring_concat_vstring
  !
  ! vstring_concat_charstring --
  !   Interface to vstring_concat_vstring to manage character strings
  !
  function vstring_concat_charstring ( this , string2 ) result (concat_string)
    type ( t_vstring ) , intent(in) :: this
    character(len=*), intent(in) :: string2
    type(t_vstring)             :: concat_string
    type(t_vstring) :: vstring2
    call vstring_new ( vstring2 , string2 )
    concat_string = vstring_concat_vstring ( this , vstring2 )
    call vstring_free ( vstring2 )
  end function vstring_concat_charstring
  !
  ! vstring_compare_vstring --
  !   Perform a character-by-character comparison of strings this and string2.
  !   Returns -1, 0, or 1, depending on whether this is lexicographically less
  !   than, equal to, or greater than string2.
  !   If nocase is set to true, the case of the characters is not taken into account.
  !   The default behaviour is to take into account for case of characters.
  !   If length is specified, then only the first length characters are used in the comparison.
  !
  function vstring_compare_vstring ( this , string2 , nocase , length ) result ( compare )
    type ( t_vstring ) , intent(in) :: this
    type ( t_vstring ) , intent(in) :: string2
    logical , intent (in), optional :: nocase
    integer , intent ( in ), optional :: length
    integer                     :: compare
    integer :: common_length
    integer :: length_this , length2
    integer :: icharacter
    logical :: comparison_done
    logical :: nocase_real
    type(t_vstring) :: char1
    type(t_vstring) :: char2
    type(t_vstring) :: char1_case
    type(t_vstring) :: char2_case
    character (len=300) :: message
    integer :: status
    !
    ! Process options
    !
    if ( present ( nocase ) ) then
       nocase_real = nocase
    else
       nocase_real = .false.
    endif
    call vstring_check_string ( this , "vstring_compare" , status )
    if ( status /= VSTRING_ERROR_OK ) then
       return
    endif
    call vstring_check_string ( string2 , "vstring_compare" , status )
    if ( status /= VSTRING_ERROR_OK ) then
       return
    endif
    !
    ! Initialize
    !
    length_this = vstring_length ( this )
    length2 = vstring_length ( string2 )
    common_length = min ( length_this , length2 )
    comparison_done = .false.
    !
    ! If the length optional parameter is given, reduce the number of characters which are compared.
    !
    if ( present ( length ) ) then
       if ( length > common_length ) then
          write ( message , * ) "The given number of characters to compare ", length ,&
               " does not match the number of characters present in both strings : ", common_length , &
               " in vstring_equals."
          call vstring_error ( this , message )
       endif
       common_length = length
    endif    !
    ! Compare character by character until there is one difference
    !
    do icharacter = 1 , common_length
       !
       ! Compute the character at index #icharacter, with modified case if necessary.
       !
       char1 = vstring_index ( this , icharacter )
       char2 = vstring_index ( string2 , icharacter )
       if ( nocase_real ) then
          char1_case = vstring_tolower ( char1 )
          char2_case = vstring_tolower ( char2 )
       else
          call vstring_new ( char1_case , char1 )
          call vstring_new ( char2_case , char2 )
       endif
       !
       ! Make the comparison of the modified characters
       !
       if ( char1_case % chars ( 1 ) < char2_case % chars ( 1 ) ) then
          comparison_done = .true.
          compare = VSTRING_COMPARE_LOWER
       elseif ( char1_case % chars ( 1 ) > char2_case % chars ( 1 ) ) then
          comparison_done = .true.
          compare = VSTRING_COMPARE_GREATER
       endif
       call vstring_free ( char1 )
       call vstring_free ( char2 )
       call vstring_free ( char1_case )
       call vstring_free ( char2_case )
       !
       ! Note : the "exit" is done afterwards, in order to let the system free the characters.
       !
       if ( comparison_done ) then
          exit
       endif
    enddo
    !
    ! If the common part is the same, compare the lengths
    !
    if ( .NOT. comparison_done ) then
       if ( present ( length ) ) then
          ! If the length argument is provided, then the previous algorithm has
          ! proved that all characters from 1 to length are equal.
          compare = VSTRING_COMPARE_EQUAL
       else
          if ( length_this == length2 ) then
             compare = VSTRING_COMPARE_EQUAL
          elseif ( length_this > length2 ) then
             compare = VSTRING_COMPARE_GREATER
          else
             compare = VSTRING_COMPARE_LOWER
          endif
       endif
    endif
  end function vstring_compare_vstring
  !
  ! vstring_compare_charstring --
  !   Interface to vstring_compare_vstring to manage character string.
  !
  function vstring_compare_charstring ( this , string2 , nocase , length ) result ( compare )
    type ( t_vstring ) , intent(in) :: this
    character (len=*), intent(in) :: string2
    logical , intent ( in ), optional :: nocase
    integer , intent ( in ), optional :: length
    integer                     :: compare
    type(t_vstring) :: vstring2
    call vstring_new ( vstring2 , string2 )
    compare = vstring_compare_vstring ( this , vstring2 , nocase , length )
    call vstring_free ( vstring2 )
  end function vstring_compare_charstring
  !
  ! vstring_trim_vstring --
  !   Returns a new string except that any leading or trailing characters 
  !   from the set given by chars are removed.
  !   If chars is not specified then white space is removed (spaces, tabs, 
  !   newlines, and carriage returns).
  !
  function vstring_trim_vstring ( this , chars ) result ( trim_string )
    type ( t_vstring ) , intent(in) :: this
    type ( t_vstring ) , intent(in), optional :: chars
    type(t_vstring)             :: trim_string
    type(t_vstring)             :: chars_real
    type(t_vstring) :: trimmedLeft
    integer :: status
    call vstring_check_string ( this , "vstring_trim_vstring" , status )
    if ( status /= VSTRING_ERROR_OK ) then
       return
    endif
    if (present ( chars )) then
       call vstring_check_string ( chars , "vstring_trim_vstring" , status )
       if ( status /= VSTRING_ERROR_OK ) then
          return
       endif
       call vstring_new ( chars_real , chars )
    else
       call vstring_new ( chars_real , VSTRING_WHITESPACE )
    endif
    !
    ! 1. Trim left
    !
    trimmedLeft = vstring_trimleft ( this , chars_real )
    !
    ! 2. Trim right
    !
    trim_string = vstring_trimright ( trimmedLeft , chars_real )
    !
    ! 3. Cleanup
    !
    call vstring_free ( chars_real )
    call vstring_free ( trimmedLeft )
  end function vstring_trim_vstring
  !
  ! vstring_trim_charstring --
  !   Interface to vstring_trim_vstring to manage character strings.
  !
  function vstring_trim_charstring ( this , chars ) result ( trim_string )
    type ( t_vstring ) , intent(in) :: this
    character (len=*), intent(in) :: chars
    type(t_vstring)             :: trim_string
    type(t_vstring) :: vchars
    call vstring_new ( vchars , chars )
    trim_string = vstring_trim_vstring ( this , vchars )
    call vstring_free ( vchars )
  end function vstring_trim_charstring
  !
  ! vstring_trimleft_vstring --
  !   Returns a new string except that any leading characters 
  !   from the set given by chars are removed.
  !   If chars is not specified then white space is removed 
  !   (spaces, tabs, newlines, and carriage returns).
  !
  function vstring_trimleft_vstring ( this , chars ) result ( trim_string )
    type ( t_vstring ) , intent(in) :: this
    type ( t_vstring ) , intent(in), optional :: chars
    type(t_vstring)             :: trim_string
    type(t_vstring)             :: chars_real
    integer :: length
    integer :: icharacter
    integer :: searched_index
    type(t_vstring) :: current_char
    integer :: first
    logical :: first_found
    integer :: status
    call vstring_check_string ( this , "vstring_trimleft_vstring" , status )
    if ( status /= VSTRING_ERROR_OK ) then
       return
    endif
    if (present ( chars )) then
       call vstring_check_string ( chars , "vstring_trimleft_vstring" , status )
       if ( status /= VSTRING_ERROR_OK ) then
          return
       endif
       call vstring_new ( chars_real , chars )
    else
       call vstring_new ( chars_real , VSTRING_WHITESPACE )
    endif
    length = vstring_length ( this )
    !
    ! 1. Compute the first character index which is not in the set of characters to remove
    !
    first_found = .false.
    do icharacter = 1 , length
       current_char = vstring_index ( this , icharacter )
       searched_index = vstring_first ( chars_real , current_char )
       call vstring_free ( current_char )
       if ( searched_index == VSTRING_INDEX_UNKNOWN ) then
          first = icharacter
          first_found = .true.
          exit
       endif
    enddo
    !
    ! 2. If there are characters not to remove, the result is the string range from first to length.
    ! If all the characters of the current string are to remove, create an empty string.
    !
    if ( first_found ) then
       trim_string = vstring_range ( this , first , length )
    else
       call vstring_new ( trim_string , "" )
    endif
    !
    ! 3. Cleanup
    !
    call vstring_free ( chars_real )
  end function vstring_trimleft_vstring
  !
  ! vstring_trimleft_charstring --
  !   Interface to vstring_trimleft_vstring to manage character strings.
  !
  function vstring_trimleft_charstring ( this , chars ) result ( trim_string )
    type ( t_vstring ) , intent(in) :: this
    character (len=*), intent(in) :: chars
    type(t_vstring)             :: trim_string
    type(t_vstring) :: vchars
    call vstring_new ( vchars , chars )
    trim_string = vstring_trimleft_vstring ( this , vchars )
    call vstring_free ( vchars )
  end function vstring_trimleft_charstring
  !
  ! vstring_trimright --
  !   Returns a value equal to string except that any trailing characters from the set given by chars are removed.
  !   If chars is not specified then white space is removed (spaces, tabs, newlines, and carriage returns).
  !
  function vstring_trimright_vstring ( this , chars ) result ( trim_string )
    type ( t_vstring ) , intent(in) :: this
    type ( t_vstring ) , intent(in), optional :: chars
    type(t_vstring)             :: trim_string
    type(t_vstring)             :: chars_real
    integer :: length
    integer :: icharacter
    integer :: searched_index
    type(t_vstring) :: current_char
    integer :: last
    logical :: last_found
    integer :: status
    call vstring_check_string ( this , "vstring_trimright_vstring" , status )
    if ( status /= VSTRING_ERROR_OK ) then
       return
    endif
    if (present ( chars )) then
       call vstring_check_string ( chars , "vstring_trimright_vstring" , status )
       if ( status /= VSTRING_ERROR_OK ) then
          return
       endif
       call vstring_new ( chars_real , chars )
    else
       call vstring_new ( chars_real , VSTRING_WHITESPACE )
    endif
    length = vstring_length ( this )
    !
    ! 1. Compute the last character index which is not in the set of characters to remove
    !
    last_found = .false.
    do icharacter = length , 1 , -1
       current_char = vstring_index ( this , icharacter )
       searched_index = vstring_last ( chars_real , current_char )
       call vstring_free ( current_char )
       if ( searched_index == VSTRING_INDEX_UNKNOWN ) then
          last = icharacter
          last_found = .true.
          exit
       endif
    enddo
    !
    ! 2. If there are characters not to remove, the result is the string range from 1 to last.
    ! If all the characters of the current string are to remove, create an empty string.
    !
    if ( last_found ) then
       trim_string = vstring_range ( this , 1 , last )
    else
       call vstring_new ( trim_string , "" )
    endif
    !
    ! 3. Cleanup
    !
    call vstring_free ( chars_real )
  end function vstring_trimright_vstring
  !
  ! vstring_trimright_charstring --
  !   Interface to vstring_trimright_vstring to manage character strings.
  !
  function vstring_trimright_charstring ( this , chars ) result ( trim_string )
    type ( t_vstring ) , intent(in) :: this
    character (len=*), intent(in) :: chars
    type(t_vstring)             :: trim_string
    type(t_vstring) :: vchars
    call vstring_new ( vchars , chars )
    trim_string = vstring_trimright_vstring ( this , vchars )
    call vstring_free ( vchars )
  end function vstring_trimright_charstring
  !
  ! vstring_first_vstring --
  !   Search in the current string for a sequence of characters that exactly match the characters in string2.
  !   If found, return the index of the first character in the first such match within the current string.
  !   If not found, return 0.
  !   If first is specified, then the search is constrained to start with the character 
  !   in the current string specified by the index.
  !
  function vstring_first_vstring ( this , string2 , first ) result ( foundIndex )
    type ( t_vstring ) , intent(in)   :: this
    type ( t_vstring ) , intent(in)   :: string2
    integer, intent(in), optional :: first
    integer                       :: foundIndex
    integer :: startIndex
    integer :: icharacter
    integer :: length
    integer :: length_searched
    integer :: rangeEndIndex
    type(t_vstring)   :: substring
    logical :: equals
    integer :: status
    call vstring_check_string ( this , "vstring_first_vstring" , status )
    if ( status /= VSTRING_ERROR_OK ) then
       return
    endif
    call vstring_check_string ( string2 , "vstring_first_vstring" , status )
    if ( status /= VSTRING_ERROR_OK ) then
       return
    endif
    if (present( first )) then
       call vstring_check_index ( this , first )
       startIndex = first
    else
       startIndex = 1
    endif
    length = vstring_length ( this )
    length_searched = vstring_length ( string2 )
    foundIndex = VSTRING_INDEX_UNKNOWN
    !
    ! The length of the string to search for is 0, which never matches.
    !
    if ( length_searched /= 0 ) then
       do icharacter = startIndex , length
          rangeEndIndex = icharacter + length_searched - 1
          ! The number of characters to search for is greater than the number
          ! of characters which are left to compare.
          if ( rangeEndIndex > length ) then
             exit
          endif
          substring = vstring_range ( this , icharacter , rangeEndIndex )
          equals = vstring_equals ( substring , string2 )
          call vstring_free ( substring )
          if ( equals ) then
             foundIndex = icharacter
             exit
          endif
       enddo
    endif
  end function vstring_first_vstring
  !
  ! vstring_first_charstring --
  !   Interface to vstring_first_vstring to manage character strings
  !
  function vstring_first_charstring ( this , string2 , first ) result ( foundIndex )
    type ( t_vstring ) , intent(in)   :: this
    character(len=*), intent(in)   :: string2
    integer, intent(in), optional :: first
    integer                       :: foundIndex
    type(t_vstring) :: vstring2
    call vstring_new ( vstring2 , string2 )
    foundIndex = vstring_first_vstring ( this , vstring2 , first )
    call vstring_free ( vstring2 )
  end function vstring_first_charstring
  !
  ! vstring_last_vstring --
  !   Search in the current string for a sequence of characters that exactly match the characters in string2.
  !   If found, return the index of the last character in the first such match within the current string.
  !   If not found, return 0.
  !   If last is specified, then the search is constrained to start with the character in the current 
  !   string specified by the index.
  !
  function vstring_last_vstring ( this , string2 , last ) result ( foundIndex )
    type ( t_vstring ) , intent(in)   :: this
    type ( t_vstring ) , intent(in)   :: string2
    integer, intent(in), optional     :: last
    integer                           :: foundIndex
    integer :: endIndex
    integer :: icharacter
    integer :: length
    integer :: length_searched
    integer :: rangeEndIndex
    type(t_vstring)   :: substring
    logical :: equals
    integer :: status
    call vstring_check_string ( this , "vstring_last_vstring" , status )
    if ( status /= VSTRING_ERROR_OK ) then
       return
    endif
    call vstring_check_string ( string2  , "vstring_last_vstring" , status )
    if ( status /= VSTRING_ERROR_OK ) then
       return
    endif
    length = vstring_length ( this )
    if (present( last )) then
       call vstring_check_index ( this , last )
       endIndex = last
    else
       endIndex = length
    endif
    length_searched = vstring_length ( string2 )
    foundIndex = VSTRING_INDEX_UNKNOWN
    !
    ! The length of the string to search for is 0, which never matches.
    !
    if ( length_searched /= 0 ) then
       do icharacter = endIndex , 1 , -1
          rangeEndIndex = icharacter + length_searched - 1
          ! The number of characters to search for is greater than the number
          ! of characters which are left to compare.
          if ( rangeEndIndex > length ) then
             exit
          endif
          substring = vstring_range ( this , icharacter , rangeEndIndex )
          equals = vstring_equals ( substring , string2 )
          call vstring_free ( substring )
          if ( equals ) then
             foundIndex = icharacter
             exit
          endif
       enddo
    endif
  end function vstring_last_vstring
  !
  ! vstring_last_charstring --
  !   Interface to vstring_last_vstring to manage character strings
  !
  function vstring_last_charstring ( this , string2 , first ) result ( foundIndex )
    type ( t_vstring ) , intent(in)   :: this
    character(len=*), intent(in)   :: string2
    integer, intent(in), optional :: first
    integer                       :: foundIndex
    type(t_vstring) :: vstring2
    call vstring_new ( vstring2 , string2 )
    foundIndex = vstring_last_vstring ( this , vstring2 , first )
    call vstring_free ( vstring2 )
  end function vstring_last_charstring
  !
  ! vstring_range --
  !   Returns a range of consecutive characters from string,
  !   starting with the character whose index is first and ending with the character whose
  !   index is last. An index of 1 refers to the first character of the string.
  !   If first is less than 1 then an error is generated.
  !   If last is greater than or equal to the length of the string then an error is generated.
  !   If first is greater than last then an error is generated.
  ! TODO : first and last may be specified as for the index method.
  !
  function vstring_range ( this , first , last ) result ( string_range )
    type ( t_vstring ) , intent(in) :: this
    integer, intent(in)         :: first , last
    type(t_vstring)             :: string_range
    character ( len = 200) :: message
    integer :: status
    call vstring_check_string ( this , "vstring_range" , status )
    if ( status /= VSTRING_ERROR_OK ) then
       return
    endif
    call vstring_check_index ( this , first , "vstring_range" , status )
    if ( status /= VSTRING_ERROR_OK ) then
       return
    endif
    call vstring_check_index ( this , last , "vstring_range" , status )
    if ( status /= VSTRING_ERROR_OK ) then
       return
    endif
    if ( first > last ) then
       write ( message , * ) "First index ", first , " is greater than last ", last, &
            " in vstring_range"
       call vstring_error ( this , message )
    endif
    call vstring_new ( string_range , this % chars ( first : last ) )
  end function vstring_range
  !
  ! vstring_index --
  !   Returns the charIndex'th character of the string argument.
  !   A charIndex of 1 corresponds to the first character of the string.
  !   If charIndex is less than 1 or greater than or equal to the length of the string
  !   then an error is generated.
  ! TODO : index can be given as the string "end" and corresponds to the last char of the string.
  !
  function vstring_index ( this , charIndex ) result ( string_index )
    type ( t_vstring ) , intent(in) :: this
    integer, intent(in)         :: charIndex
    type(t_vstring)             :: string_index
    integer :: status
    call vstring_check_string ( this , "vstring_index" , status )
    if ( status /= VSTRING_ERROR_OK ) then
       return
    endif
    call vstring_check_index ( this , charIndex , "vstring_index" , status )
    if ( status /= VSTRING_ERROR_OK ) then
       return
    endif
    call vstring_new ( string_index , this % chars ( charIndex : charIndex ) )
  end function vstring_index
  !
  ! vstring_toupper --
  !   Returns a vstring except that all lower (or title) case letters have been 
  !   converted to upper case. If first is specified, it refers to the first char index in the string 
  !   to start modifying. If last is specified, it refers to the char index in the string to stop 
  !   at (inclusive).
  !
  function vstring_toupper ( this , first , last ) result ( new_upper )
    type ( t_vstring ) , intent(in) :: this
    integer , intent ( in ), optional :: first
    integer , intent ( in ), optional :: last
    type(t_vstring) :: new_upper
    integer :: first_real
    integer :: last_real
    integer :: icharacter
    integer :: firstindex
    integer :: status
    !
    ! Get options
    !
    call vstring_check_string ( this , "vstring_toupper" , status )
    if ( status /= VSTRING_ERROR_OK ) then
       return
    endif
    if (present( first )) then
       call vstring_check_index ( this , first , "vstring_toupper" , status )
       if ( status /= VSTRING_ERROR_OK ) then
          return
       endif
       first_real = first
    else
       first_real = 1
    endif
    if (present( last )) then
       call vstring_check_index ( this , last , "vstring_toupper" , status )
       if ( status /= VSTRING_ERROR_OK ) then
          return
       endif
       last_real = last
    else
       last_real = vstring_length ( this )
    endif
    !
    ! Compute the upper case string.
    !
    call vstring_new ( new_upper , this )
    do icharacter = first_real , last_real
       firstindex = index( VSTRING_LOWER , this % chars ( icharacter)  )
       if ( firstindex > 0 ) then
          new_upper % chars ( icharacter ) = VSTRING_UPPER ( firstindex : firstindex )
       endif

    enddo
  end function vstring_toupper
  !
  ! vstring_tolower --
  !   Returns a vstring except that all upper (or title) case letters have been 
  !   converted to lower case. If first is specified, it refers to the first char index in the string 
  !   to start modifying. If last is specified, it refers to the char index in the string to stop 
  !   at (inclusive).
  !
  function vstring_tolower ( this , first , last ) result ( new_lower )
    type ( t_vstring ) , intent(in) :: this
    integer , intent ( in ), optional :: first
    integer , intent ( in ), optional :: last
    type(t_vstring) :: new_lower
    integer :: first_real
    integer :: last_real
    integer :: icharacter
    integer :: firstindex
    integer :: length
    integer :: status
    !
    ! Get options
    !
    call vstring_check_string ( this , "vstring_tolower" , status )
    if ( status /= VSTRING_ERROR_OK ) then
       return
    endif
    length = vstring_length ( this )
    if (present( first )) then
       call vstring_check_index ( this , first , "vstring_tolower" , status )
       if ( status /= VSTRING_ERROR_OK ) then
          return
       endif
       first_real = first
    else
       first_real = 1
    endif
    if (present( last )) then
       call vstring_check_index ( this , last , "vstring_tolower" , status )
       if ( status /= VSTRING_ERROR_OK ) then
          return
       endif
       last_real = last
    else
       last_real = length
    endif
    !
    ! Compute the lower case string.
    !
    call vstring_new ( new_lower , this )
    do icharacter = first_real , last_real
       firstindex = index( VSTRING_UPPER , this % chars ( icharacter)  )
       if ( firstindex > 0 ) then
          new_lower % chars ( icharacter ) = VSTRING_LOWER ( firstindex : firstindex )
       endif
    enddo
  end function vstring_tolower
  !
  ! vstring_totitle --
  !   Returns a vstring except that the first character in string is converted 
  !   to upper case, and the rest of the string is converted to lower case. If first is specified, it refers 
  !   to the first char index in the string to start modifying. If last is specified, it refers 
  !   to the char index in the string to stop at (inclusive).
  !
  function vstring_totitle ( this , first , last ) result ( new_title )
    type ( t_vstring ) , intent(in) :: this
    integer , intent ( in ), optional :: first
    integer , intent ( in ), optional :: last
    type(t_vstring) :: new_title
    integer :: first_real
    integer :: last_real
    integer :: length
    integer :: status
    !
    ! The new string is made of 4 parts :
    ! - before the first letter to update (left unchanged)
    ! - the letter to upper case
    ! - the sub-part to lower case
    ! - after the last letter to update (left unchanged)
    !
    type(t_vstring) :: subpart1
    type(t_vstring) :: subpart2
    type(t_vstring) :: subpart3
    type(t_vstring) :: subpart4
    type(t_vstring) :: subpartToUpper
    type(t_vstring) :: subpartToLower
    call vstring_check_string ( this , "vstring_totitle" , status )
    if ( status /= VSTRING_ERROR_OK ) then
       return
    endif
    !
    ! Get options
    !
    if (present( first )) then
       call vstring_check_index ( this , first , "vstring_totitle" , status )
    if ( status /= VSTRING_ERROR_OK ) then
       return
    endif
       first_real = first
    else
       first_real = 1
    endif
    length = vstring_length ( this )
    if (present( last )) then
       call vstring_check_index ( this , last , "vstring_totitle" , status )
    if ( status /= VSTRING_ERROR_OK ) then
       return
    endif
       last_real = last
    else
       last_real = length
    endif
    !
    ! Compute subpart1
    !
    if ( first_real > 1 ) then
       subpart1 = vstring_range ( this , 1 , first_real - 1 )
    else
       call vstring_new ( subpart1 , "" )
    endif
    !
    ! Compute subpart2
    !
    if ( length > 0 ) then
       subpartToUpper = vstring_index ( this , first_real )
       subpart2 = vstring_toupper ( subpartToUpper )
       call vstring_free ( subpartToUpper )
    else
       call vstring_new ( subpart2 , "" )
    endif
    !
    ! Compute subpart3
    !
    if ( first_real + 1 <= last_real ) then
       subpartToLower = vstring_range ( this , first_real + 1, last_real )
       subpart3 = vstring_tolower ( subpartToLower )
       call vstring_free ( subpartToLower )
    else
       call vstring_new ( subpart3 , "" )
    endif
    !
    ! Compute subpart4
    !
    if ( last_real + 1 <= length ) then
       subpart4 = vstring_range ( this , last_real + 1 , length )
    else
       call vstring_new ( subpart4 , "" )
    endif
    !
    ! Concatenate the sub-parts
    !
    call vstring_new ( new_title , subpart1 )
    call vstring_append ( new_title , subpart2 )
    call vstring_append ( new_title , subpart3 )
    call vstring_append ( new_title , subpart4 )
    !
    ! Cleanup
    !
    call vstring_free ( subpart1 )
    call vstring_free ( subpart2 )
    call vstring_free ( subpart3 )
    call vstring_free ( subpart4 )
  end function vstring_totitle
  !
  ! vstring_reverse --
  !   Return a string that has all characters in reverse order
  ! Arguments:
  !   this     The current object
  ! Result:
  !   Reversed string
  !
  function vstring_reverse ( this ) result ( new_reverse )
    type ( t_vstring ) , intent(in) :: this
    type(t_vstring) :: new_reverse
    integer :: icharacter
    integer :: length
    integer :: status
    call vstring_check_string ( this , "vstring_reverse" , status )
    if ( status /= VSTRING_ERROR_OK ) then
       return
    endif
    length = vstring_length ( this )
    call vstring_new ( new_reverse , this )
    do icharacter = 1 , length
       new_reverse % chars ( icharacter ) = this % chars ( length - icharacter + 1 )
    enddo
  end function vstring_reverse
  !
  ! vstring_random --
  !   Fill a string with required length and randomized characters.
  ! Arguments:
  !   length : the length of the random string to compute.
  ! Result:
  !   String with random letters
  ! Note :
  !   This is a static method.
  !
  function vstring_random ( length ) result ( new_random )
    integer, intent(in) :: length
    type(t_vstring) :: new_random
    integer :: icharacter
    integer :: random_integer
    integer :: setsize
    real :: alea
    type(t_vstring) :: characterset
    !
    ! Initialize
    !
    setsize = len ( VSTRING_CHARACTERSET )
    call vstring_new ( characterset , VSTRING_CHARACTERSET )
    call vstring_new ( new_random , length )
    if ( .NOT.random_process_initialize ) then
       call random_seed()
       random_process_initialize = .true.
    endif
    !
    ! Compute each random character
    !
    do icharacter = 1 , length
       call random_number ( alea )
       random_integer = nint( alea * setsize + 1 - alea )
       new_random % chars ( icharacter ) = characterset % chars ( random_integer )
    enddo
    !
    ! Cleanup
    !
    call vstring_free ( characterset )
  end function vstring_random
  !
  ! vstring_match_vstring --
  !   See if pattern matches string; return 1 if it does, 0 if it doesn't. 
  !   If -nocase is specified, then the pattern attempts to match against the 
  !   string in a case insensitive manner. 
  !   For the two strings to match, their contents must be identical except 
  !   that the following special sequences may appear in pattern:
  !     *
  !       Matches any sequence of characters in string, including a null string.
  !     ?
  !       Matches any single character in string.
  !     [chars]
  !       Matches any character in the set given by chars. 
  !       If a sequence of the form x-y appears in chars, then any character 
  !       between x and y, inclusive, will match. 
  !       When used with -nocase, the characters of the range are converted to lower case first. 
  !       Whereas {[A-z]} matches '_' when matching case-sensitively ('_' falls between the 'Z' 
  !       and 'a'), with -nocase this is considered like {[A-Za-z]} (and probably what was 
  !       meant in the first place).
  !     \x
  !       Matches the single character x. 
  !       This provides a way of avoiding the special interpretation of the characters *?[]\ in pattern.
  !
  recursive function vstring_match_vstring ( this , pattern , nocase ) result ( match )
    type ( t_vstring ) , intent(in) :: this
    type ( t_vstring ) , intent(in) :: pattern
    logical , intent(in) , optional :: nocase
    logical :: match
    logical :: equals
    type(t_vstring) :: backslash
    type(t_vstring) :: question
    type(t_vstring) :: this_char1
    type(t_vstring) :: pattern_char1
    type(t_vstring) :: charstar
    type(t_vstring) :: leftbracket
    type(t_vstring) :: rightbracket
    integer :: this_length
    integer :: pattern_length
    type(t_vstring) :: this_substring
    type(t_vstring) :: pattern_substring
    type(t_vstring) :: pattern_char2
    type(t_vstring) :: pattern_substring2
    integer :: first
    integer :: firstrightbracket
    type(t_vstring) :: chars
    type(t_vstring) :: expanded
    logical :: nocase_real
    type(t_vstring) :: this_char1lower
    type(t_vstring) :: chars_lower
    integer :: status
    !
    ! Process options
    !
    if ( present ( nocase ) ) then
       nocase_real = nocase
    else
       nocase_real = .false.
    endif
    !
    ! Initialize
    !
    call vstring_check_string ( this , "Check string in vstring_match_vstring." , status )
    if ( status /= VSTRING_ERROR_OK ) then
       return
    endif
    call vstring_check_string ( pattern , "Check pattern in vstring_match_vstring." , status )
    if ( status /= VSTRING_ERROR_OK ) then
       return
    endif
    call vstring_new ( charstar , "*" )
    call vstring_new ( backslash , "\" )
    call vstring_new ( question , "?" )
    call vstring_new ( leftbracket , "[" )
    call vstring_new ( rightbracket , "]" )
    this_length = vstring_length ( this )
    pattern_length = vstring_length ( pattern )
    !
    ! Get various parts of the current string and the pattern :
    ! - first char,
    ! - everything from the 2 to the end.
    !
    if ( this_length > 0 ) then
       this_char1 = vstring_index ( this , 1 )
    else
       call vstring_new ( this_char1 , "" )
    endif
    if ( pattern_length > 0 ) then
       pattern_char1 = vstring_index ( pattern , 1 )
    else
       call vstring_new ( pattern_char1, "" )
    endif
    if ( this_length > 1 ) then
       this_substring = vstring_range ( this , 2 , this_length )
    else
       call vstring_new ( this_substring , "" )
    endif
    if ( pattern_length > 1 ) then
       pattern_substring = vstring_range ( pattern , 2 , pattern_length )
    else
       call vstring_new ( pattern_substring , "" )
    endif
    !
    ! This is a chain of tests :
    ! the first test which returns .true. breaks the chain.
    !
    match = .false.
    do
       !
       ! Process the strict equality between string and pattern
       !
       match = vstring_equals ( this , pattern , nocase = nocase_real )
       if ( match ) then
          exit
       endif
       !
       ! Process the case where the first character of the pattern is a "\".
       !
       equals = vstring_equals ( backslash , pattern_char1 )
       if ( equals ) then
          !
          ! Compare the second character of the pattern against the first character of the string.
          !
          if ( pattern_length > 1 ) then
             pattern_char2 = vstring_index ( pattern , 2 )
             equals = vstring_equals ( this_char1 , pattern_char2 , nocase = nocase_real )
             call vstring_free ( pattern_char2 )
             if ( equals ) then
                if ( pattern_length > 2 ) then
                   pattern_substring2 = vstring_range ( pattern , 3 , pattern_length )
                else
                   call vstring_new ( pattern_substring2 , "" )
                endif
                match = vstring_match_vstring ( this_substring , pattern_substring2 , nocase = nocase_real )
                call vstring_free ( pattern_substring2 )
                if ( match ) then
                   exit
                endif
             endif
          endif
       endif
       !
       ! Process the case where the first character of the pattern is a "*"
       !
       equals = vstring_equals ( charstar , pattern_char1 )
       if ( equals ) then
          !
          ! Solution #1 : Compare the string against the end of the pattern
          !
          match = vstring_match_vstring ( this , pattern_substring , nocase = nocase_real )
          if ( match ) then
             exit
          endif
          !
          ! Solution #2 : The string is empty and the subpattern after the "*" does not match.
          ! => There is no match at all.
          ! Note: this_substring is therefore empty and solution #3 would generate a infinite loop.
          !
          if ( this_length == 0 ) then
             exit
          endif
          !
          ! Solution #3 : Compare the end of the string against the pattern
          !
          match = vstring_match_vstring ( this_substring , pattern , nocase = nocase_real )
          if ( match ) then
             exit
          endif
       endif
       !
       ! Process the case where the first character of the pattern is a "?"
       ! If the current string is of length 0, there is no match.
       !
       equals = vstring_equals ( question , pattern_char1 )
       if ( equals .AND. this_length > 0 ) then
          !
          ! Compare the end of the string against the end of the pattern.
          !
          match = vstring_match_vstring ( this_substring , pattern_substring , nocase = nocase_real )
          if ( match ) then
             exit
          endif
       endif
       !
       ! Process the case where the first character of the pattern is a "[".
       ! It is followed by a list of characters that are acceptable, or by a range
       ! (two characters separated by "-").
       ! It is ended by a "]".
       !
       equals = vstring_equals ( leftbracket , pattern_char1 )
       if ( equals ) then
          !
          ! Search for the corresponding right bracket "]"
          !
          firstrightbracket = vstring_first ( pattern , rightbracket )
          if ( firstrightbracket > 2 ) then
             !
             ! Get the characters in the set.
             !
             chars = vstring_range ( pattern , 2 , firstrightbracket - 1 )
             if ( nocase_real ) then
                chars_lower = vstring_tolower ( chars )
                call vstring_free ( chars )
                call vstring_new ( chars , chars_lower )
                call vstring_free ( chars_lower )
             endif
             !
             ! Expand the character set, by taking into account for character ranges defined with "-".
             !
             expanded = vstring_expandcharset ( chars )
             call vstring_free ( chars )
             !
             ! Search the first char of the current string in the expanded set of characters
             !
             if ( nocase_real ) then
                this_char1lower = vstring_tolower ( this_char1 )
                first = vstring_first ( expanded , this_char1lower )
                call vstring_free ( this_char1lower )
             else
                first = vstring_first ( expanded , this_char1 )
             endif
             call vstring_free ( expanded )
             if ( first > 0 ) then
                !
                ! Compare the end of the string to the end of the pattern
                !
                if ( pattern_length > firstrightbracket ) then
                   pattern_substring2 = vstring_range ( pattern , firstrightbracket + 1 , pattern_length )
                else
                   call vstring_new ( pattern_substring2 , "" )
                endif
                match = vstring_match_vstring ( this_substring , pattern_substring2 , nocase = nocase_real )
                call vstring_free ( pattern_substring2 )
                if ( match ) then
                   exit
                endif
             endif
          endif
       endif
       !
       ! The first letter is not a special character.
       ! There is a match if the first letters are the same
       ! and the end of both string and pattern match.
       !
       equals = vstring_equals ( this_char1 , pattern_char1 , nocase = nocase_real )
       if ( equals ) then
          !
          ! Compare the end of the string against the end of the pattern
          !
          match = vstring_match_vstring ( this_substring , pattern_substring , nocase = nocase_real )
          if ( match ) then
             exit
          endif
       endif
       !
       ! No test match
       !
       exit
    enddo
    call vstring_free ( charstar )
    call vstring_free ( backslash )
    call vstring_free ( question )
    call vstring_free ( this_char1 )
    call vstring_free ( pattern_char1 )
    call vstring_free ( this_substring )
    call vstring_free ( pattern_substring )
    call vstring_free ( leftbracket )
    call vstring_free ( rightbracket )
  end function vstring_match_vstring
  !
  ! vstring_match_charstring --
  !   Interface to vstring_match_vstring to manage character strings.
  !
  recursive function vstring_match_charstring ( this , pattern , nocase ) result ( match )
    type ( t_vstring ) , intent(in) :: this
    character(len=*), intent(in) :: pattern
    logical , intent(in) , optional :: nocase
    logical :: match
    type(t_vstring) :: vpattern
    call vstring_new ( vpattern , pattern )
    match = vstring_match_vstring ( this , vpattern , nocase )
    call vstring_free ( vpattern )
  end function vstring_match_charstring
  !
  ! vstring_expandcharset --
  !   Consider that the current string is a character set and returns 
  !   the expanded form of that set.
  !   The expanded form of the character set "abc" is "abc".
  !   If a sequence of the form x-y appears in the current string, then the expanded form
  !   contains all characters between x and y, inclusive.
  !   For example, if the current string is "a-z", the returned expanded set is made 
  !   of all the lower case letters.
  !   For example, if the current string is "a-zA-Z", the returned expanded set is made 
  !   of all the lower case letters and upper case letters.
  !   For example, if the current string is "a-zA-Z_", the returned expanded set is made 
  !   of all the lower case letters and upper case letters and underscore.
  !
  recursive function vstring_expandcharset ( this ) result ( expanded )
    implicit none
    type ( t_vstring ) , intent(in) :: this
    type(t_vstring) :: expanded
    type(t_vstring) :: minus
    integer :: firstminus
    type(t_vstring) :: startchar
    type(t_vstring) :: endchar
    integer :: this_length
    integer :: startascii
    integer :: endascii
    integer :: iascii
    type(t_vstring) :: asciichar
    type(t_vstring) :: this_substring
    type(t_vstring) :: expanded_end
    integer :: status
    !
    ! Initialize
    !
    call vstring_check_string ( this , "vstring_expandcharset" , status )
    if ( status /= VSTRING_ERROR_OK ) then
       return
    endif
    call vstring_new ( minus , "-" )
    this_length = vstring_length ( this )
    !
    ! Compute expanded character set
    !
    firstminus = vstring_first ( this , minus )
    if ( firstminus == 2 .AND. this_length >= 3) then
       !
       ! Add the current range to the expanded set.
       !
       startchar = vstring_index ( this , 1 )
       endchar = vstring_index ( this , 3 )
       startascii = vstring_iachar ( startchar )
       endascii = vstring_iachar ( endchar )
       !
       ! Add the the character set all character which ascii index is between start and end.
       !
       call vstring_new ( expanded , "" )
       do iascii = startascii , endascii
          asciichar = vstring_char ( iascii )
          call vstring_append ( expanded , asciichar )
          call vstring_free ( asciichar )
       enddo
       call vstring_free ( startchar )
       call vstring_free ( endchar )
       !
       ! Expand the end of the character set.
       !
       if ( this_length > 3 ) then
          this_substring = vstring_range ( this , 4 , this_length )
          expanded_end = vstring_expandcharset ( this_substring )
          call vstring_append ( expanded , expanded_end )
          call vstring_free ( expanded_end )
          call vstring_free ( this_substring )
       endif
    else
       call vstring_new ( expanded , this )
    endif
    !
    ! Clean-up
    !
    call vstring_free ( minus )
  end function vstring_expandcharset
  !
  ! vstring_is --
  !   Returns .true. if string is a valid member of the specified character class, 
  !   otherwise returns .false.. 
  !   If strict is provided and .true., then an empty string returns .false..
  !   If strict is provided and .false., or not provided, an empty string returns .true..
  !   If failindex is provided, then if the function returns .false., the index in the 
  !   string where the class was no longer valid will be stored in the variable failindex. 
  !   The following character classes are recognized (the class name can be abbreviated):
  !     alpha
  !         Any alphabet character, that is [a-zA-Z].
  !     alnum
  !         Any alphabet or digit character, that is [a-zA-Z0-9].
  !     ascii
  !         Any character with a value less than 128 (those that are in the 7-bit ascii range).
  !     control
  !         Any control character. Control chars are in the 
  !         ranges 00..1F and 7F..9F, that is from ascii #0 to #31 and from #127 to #159
  !     digit
  !         Any digit character. Note that this includes characters outside of the [0-9] range.
  !     false
  !         Any of the forms allowed where the logical is false.
  !     graph
  !         Any printing character, except space that is from ascii #33 to #126.
  !     integer
  !         Any of the valid forms for an ordinary integer in Fortran, with optional surrounding whitespace. 
  !     logical
  !         Any valid Fortran logical
  !     lower
  !         Any lower case alphabet character, that is [a-z].
  !     punct
  !         Any punctuation character, that is _,;:.?![](){}@"'
  !     print
  !         Any printing character, including space that is from ascii #32 to #126.
  !     real
  !         Any of the valid forms for a real in Fortran, with optional surrounding whitespace. 
  !     space
  !         Any space character, that is white space, tab, newline or carriage return.
  !     true
  !         Any of the forms allowed where the logical is true.
  !     upper
  !         Any upper case alphabet character, that is [A-Z].
  !     xdigit
  !         Any hexadecimal digit character ([0-9A-Fa-f]). 
  !     wordchar
  !         Any word character. That is any alphanumeric character (upper case,
  !         lower case, or digit), or any connector punctuation characters (e.g. underscore).
  ! Note
  !   This implementation is based on vstring_isincharset and vstring_isinasciirange.
  !
   function vstring_is ( this , class , strict , failindex ) result ( isinclass )
    implicit none
    type ( t_vstring ) , intent(in) :: this
    character(len=*), intent(in) :: class
    logical, intent(in) , optional :: strict
    integer, intent(out) , optional :: failindex
    logical :: isinclass
    character(len=200) :: message
    logical :: strict_real
    integer :: length
    integer :: failindex_real
    !
    ! Process options
    !
    if ( present ( strict ) ) then
       strict_real = strict
    else
       strict_real = .false.
    endif
    !
    ! By default, it is of no class
    !
    isinclass = .false.
    !
    ! Now search a class
    !
    if ( class == "digit" ) then
       isinclass = vstring_isdigit ( this , failindex_real )
    elseif ( class == "integer" ) then
       isinclass = vstring_isinteger ( this , failindex_real )
    elseif ( class == "alpha" ) then
       isinclass = vstring_isalpha ( this , failindex_real )
    elseif ( class == "alnum" ) then
       isinclass = vstring_isalnum ( this , failindex_real )
    elseif ( class == "logical" ) then
       isinclass = vstring_islogical ( this , failindex_real )
    elseif ( class == "real" ) then
       isinclass = vstring_isreal ( this , failindex_real )
    elseif ( class == "true" ) then
       isinclass = vstring_istrue ( this , failindex_real )
    elseif ( class == "false" ) then
       isinclass = vstring_isfalse ( this , failindex_real )
    elseif ( class == "lower" ) then
       isinclass = vstring_islower ( this , failindex_real )
    elseif ( class == "upper" ) then
       isinclass = vstring_isupper ( this , failindex_real )
    elseif ( class == "space" ) then
       isinclass = vstring_isspace ( this , failindex_real )
    elseif ( class == "punct" ) then
       isinclass = vstring_ispunct ( this , failindex_real )
    elseif ( class == "xdigit" ) then
       isinclass = vstring_isxdigit ( this , failindex_real )
    elseif ( class == "ascii" ) then
       isinclass = vstring_isascii ( this , failindex_real )
    elseif ( class == "control" ) then
       isinclass = vstring_iscontrol ( this , failindex_real )
    elseif ( class == "print" ) then
       isinclass = vstring_isprint ( this , failindex_real )
    elseif ( class == "graph" ) then
       isinclass = vstring_isgraph ( this , failindex_real )
    elseif ( class == "wordchar" ) then
       isinclass = vstring_iswordchar ( this , failindex_real )
    else
       write ( message , * ) "Unknown class:" , class
       call vstring_error ( this , message , "vstring_is" )
    endif
    !
    ! Process the special case where the string is empty.
    ! Caution !
    !   This is done after all other tests, to make sure that the class is known.
    !
    length = vstring_length ( this )
    if ( length == 0 ) then
       if ( strict_real ) then
          isinclass = .false.
       else
          isinclass = .true.
       endif
    endif
    !
    ! Get failing index
    !
    if ( present ( failindex ) ) then
       failindex = failindex_real
    endif
       
  end function vstring_is
  !
  ! vstring_isdigit --
  !   Returns 1 if string is a valid digit. 
  !
  logical function vstring_isdigit ( this , failindex )
    implicit none
    type ( t_vstring ) , intent(in) :: this
    integer, intent(out) :: failindex
    type(t_vstring) :: characterset
    call vstring_new ( characterset , VSTRING_DIGITS )
    vstring_isdigit = vstring_isincharset ( this , characterset , failindex )
    call vstring_free ( characterset )
  end function vstring_isdigit
  !
  ! vstring_isinteger --
  !   Returns 1 if string is a valid integer, with optional surrounding whitespace. 
  !
  logical function vstring_isinteger ( this , failindex )
    implicit none
    type ( t_vstring ) , intent(in) :: this
    integer, intent(out) :: failindex
    type(t_vstring) :: characterset
    call vstring_new ( characterset , VSTRING_DIGITS )
    call vstring_append ( characterset , VSTRING_SPACE )
    vstring_isinteger = vstring_isincharset ( this , characterset , failindex )
    call vstring_free ( characterset )
  end function vstring_isinteger
  !
  ! vstring_isalpha --
  !   Returns 1 if string is a alphabet character.
  !
  logical function vstring_isalpha ( this , failindex )
    implicit none
    type ( t_vstring ) , intent(in) :: this
    integer, intent(out) :: failindex
    type(t_vstring) :: characterset
    call vstring_new ( characterset , VSTRING_LOWER )
    call vstring_append ( characterset , VSTRING_UPPER )
    vstring_isalpha = vstring_isincharset ( this , characterset , failindex )
    call vstring_free ( characterset )
  end function vstring_isalpha
  !
  ! vstring_isalnum --
  !   Returns 1 if string is a alphabet or digit character.
  !
  logical function vstring_isalnum ( this , failindex )
    implicit none
    type ( t_vstring ) , intent(in) :: this
    integer, intent(out) :: failindex
    type(t_vstring) :: characterset
    call vstring_new ( characterset , VSTRING_LOWER )
    call vstring_append ( characterset , VSTRING_UPPER )
    call vstring_append ( characterset , VSTRING_DIGITS )
    vstring_isalnum = vstring_isincharset ( this , characterset , failindex )
    call vstring_free ( characterset )
  end function vstring_isalnum
  !
  ! vstring_islogical --
  !   Returns .true. if string is a valid logical
  !
  logical function vstring_islogical ( this , failindex )
    implicit none
    type ( t_vstring ) , intent(in) :: this
    integer, intent(out) :: failindex
    logical :: mylogical
    ! 7 is the largest possible length of a logical.
    character ( len = 7 ) :: logicalstring
    call vstring_cast ( this , logicalstring )
    read ( logicalstring , * , err = 100 ) mylogical
    vstring_islogical = .true.
    return
100 continue
    ! This is not a logical
    vstring_islogical = .false.
    ! TODO : compute a finer value for failindex
    failindex = 1
  end function vstring_islogical
  !
  ! vstring_isreal --
  !   Returns .true. if string is a valid real
  !
  logical function vstring_isreal ( this , failindex )
    implicit none
    type ( t_vstring ) , intent(in) :: this
    integer, intent(out) :: failindex
    real :: mydata
#ifdef _VSTRING_STATIC_BUFFER
    character ( len = VSTRING_BUFFER_SIZE ) :: charstring
#else
    character ( len = vstring_length ( this ) ) :: charstring
#endif
    call vstring_cast ( this , charstring )
    read ( charstring , * , err = 100 ) mydata
    vstring_isreal = .true.
    failindex = VSTRING_INDEX_UNKNOWN
    return
100 continue
    ! This is not a real
    vstring_isreal = .false.
    ! TODO : compute a finer value for failindex
    failindex = 1
  end function vstring_isreal
  !
  ! vstring_istrue --
  !   Returns .true. if string is a valid logical true value
  !
  logical function vstring_istrue ( this , failindex )
    implicit none
    type ( t_vstring ) , intent(in) :: this
    integer, intent(out) :: failindex
    logical :: mylogical
    character ( len = 7 ) :: logicalstring
    call vstring_cast ( this , logicalstring )
    read ( logicalstring , * , err = 100 ) mylogical
    vstring_istrue = mylogical
    ! TODO : compute a finer value for failindex
    if ( vstring_istrue ) then
       failindex = VSTRING_INDEX_UNKNOWN
    else
       failindex = 1
    endif
    return
100 continue
    ! This is not a logical
    vstring_istrue = .false.
    ! TODO : compute a finer value for failindex
    failindex = 1
  end function vstring_istrue
  !
  ! vstring_isfalse --
  !   Returns .true. if string is a valid logical false value
  !
  logical function vstring_isfalse ( this , failindex )
    implicit none
    type ( t_vstring ) , intent(in) :: this
    integer, intent(out) :: failindex
    logical :: mylogical
    character ( len = 7 ) :: logicalstring
    call vstring_cast ( this , logicalstring )
    read ( logicalstring , * , err = 100 ) mylogical
    vstring_isfalse = .NOT.mylogical
    if ( vstring_isfalse ) then
       failindex = VSTRING_INDEX_UNKNOWN
    else
       failindex = 1
    endif
    return
100 continue
    ! This is not a logical
    vstring_isfalse = .false.
    ! TODO : compute a finer value for failindex
    failindex = 1
  end function vstring_isfalse
  !
  ! vstring_islower --
  !   Returns .true. if string is a valid lower case character
  !
  logical function vstring_islower ( this , failindex )
    implicit none
    type ( t_vstring ) , intent(in) :: this
    integer, intent(out) :: failindex
    type(t_vstring) :: characterset
    call vstring_new ( characterset , VSTRING_LOWER )
    vstring_islower = vstring_isincharset ( this , characterset , failindex )
    call vstring_free ( characterset )
  end function vstring_islower
  !
  ! vstring_isupper --
  !   Returns .true. if string is a valid upper case character
  !
  logical function vstring_isupper ( this , failindex )
    implicit none
    type ( t_vstring ) , intent(in) :: this
    integer, intent(out) :: failindex
    type(t_vstring) :: characterset
    call vstring_new ( characterset , VSTRING_UPPER )
    vstring_isupper = vstring_isincharset ( this , characterset , failindex )
    call vstring_free ( characterset )
  end function vstring_isupper
  !
  ! vstring_isspace --
  !   Returns .true. if string is a valid space character
  !
  logical function vstring_isspace ( this , failindex )
    implicit none
    type ( t_vstring ) , intent(in) :: this
    integer, intent(out) :: failindex
    type(t_vstring) :: characterset
    call vstring_new ( characterset , VSTRING_WHITESPACE )
    vstring_isspace = vstring_isincharset ( this , characterset , failindex )
    call vstring_free ( characterset )
  end function vstring_isspace
  !
  ! vstring_ispunct --
  !   Returns .true. if string is a valid punctuation character
  !
  logical function vstring_ispunct ( this , failindex )
    implicit none
    type ( t_vstring ) , intent(in) :: this
    integer, intent(out) :: failindex
    type(t_vstring) :: characterset
    call vstring_new ( characterset , VSTRING_PUNCTUATION )
    vstring_ispunct = vstring_isincharset ( this , characterset , failindex )
    call vstring_free ( characterset )
  end function vstring_ispunct
  !
  ! vstring_isxdigit --
  !   Returns .true. if string is a valid hexadecimal string
  !
  logical function vstring_isxdigit ( this , failindex )
    implicit none
    type ( t_vstring ) , intent(in) :: this
    integer, intent(out) :: failindex
    type(t_vstring) :: characterset
    call vstring_new ( characterset , VSTRING_HEXDIGITS )
    vstring_isxdigit = vstring_isincharset ( this , characterset , failindex )
    call vstring_free ( characterset )
  end function vstring_isxdigit
  !
  ! vstring_isascii --
  !   Returns .true. if string is a valid ascii string
  !
  logical function vstring_isascii ( this , failindex )
    implicit none
    type ( t_vstring ) , intent(in) :: this
    integer, intent(out) :: failindex
    vstring_isascii = vstring_isinasciirange ( this , 0 , 127 , failindex )
  end function vstring_isascii
  !
  ! vstring_iscontrol --
  !   Returns .true. if string is a valid control string
  ! Note
  !   From ascii #0 to #31 and from #127 to #159.
  !
  logical function vstring_iscontrol ( this , failindex )
    implicit none
    type ( t_vstring ) , intent(in) :: this
    integer, intent(out) :: failindex
    logical :: isinrange1
    logical :: isinrange2
    isinrange1 = vstring_isinasciirange ( this , 0 , 31 , failindex )
    isinrange2 = vstring_isinasciirange ( this , 127 , 159 , failindex )
    if ( isinrange1 ) then
       ! This is control
       vstring_iscontrol = .true.
    elseif ( isinrange2 ) then
       ! This is control
       vstring_iscontrol = .true.
    else
       ! This is not control
       vstring_iscontrol = .false.
    endif
  end function vstring_iscontrol
  !
  ! vstring_isprint --
  !   Returns .true. if string is a valid print string, including white space
  ! Note
  !   From ascii #32 to #126
  !
  logical function vstring_isprint ( this , failindex )
    implicit none
    type ( t_vstring ) , intent(in) :: this
    integer, intent(out) :: failindex
    vstring_isprint = vstring_isinasciirange ( this , 32 , 126 , failindex )
  end function vstring_isprint
  !
  ! vstring_isgraph --
  !   Returns .true. if string is a valid print string, excluding white space
  ! Note
  !   From ascii #33 to #126
  !
  logical function vstring_isgraph ( this , failindex )
    implicit none
    type ( t_vstring ) , intent(in) :: this
    integer, intent(out) :: failindex
    vstring_isgraph = vstring_isinasciirange ( this , 33 , 126 , failindex )
  end function vstring_isgraph
  !
  ! vstring_iswordchar --
  !   Returns .true. if string is a valid wordchar character
  !
  logical function vstring_iswordchar ( this , failindex )
    implicit none
    type ( t_vstring ) , intent(in) :: this
    integer, intent(out) :: failindex
    type(t_vstring) :: characterset
    call vstring_new ( characterset )
    call vstring_append ( characterset , VSTRING_LOWER )
    call vstring_append ( characterset , VSTRING_UPPER )
    call vstring_append ( characterset , VSTRING_DIGITS )
    call vstring_append ( characterset , VSTRING_PUNCTUATION )
    vstring_iswordchar = vstring_isincharset ( this , characterset , failindex )
    call vstring_free ( characterset )
  end function vstring_iswordchar
  !
  ! vstring_isincharset --
  !   Returns .true. if string is made of characters which all are in the 
  !   given character set.
  !   If failingindex is provided and the string in not in the character set,
  !   the integer failingindex is the index in the string where the
  !   character is not in the set.
  ! Note
  !   This is an alternative implementation for vstring_verify.
  ! Arguments
  !   characterset The character set.
  !   failindex The index of the first character not in the set.
  !
  logical function vstring_isincharset ( this , characterset , failindex )
    implicit none
    type ( t_vstring ) , intent(in) :: this
    type ( t_vstring ) , intent(in) :: characterset
    integer, intent(out), optional :: failindex
    integer :: icharacter
    integer :: length
    type(t_vstring) :: currentchar
    integer :: charindex
    !
    ! Initialize
    !
    length = vstring_length ( this )
    !
    ! By default, it is in the character set.
    !
    vstring_isincharset = .true.
    !
    ! Now find the cases when it is not in the character set.
    !
    do icharacter = 1 , length
       currentchar = vstring_index ( this , icharacter )
       charindex = vstring_first ( characterset , currentchar )
       call vstring_free ( currentchar )
       if ( charindex == VSTRING_INDEX_UNKNOWN ) then
          vstring_isincharset = .false.
          exit
       endif
    enddo
    !
    ! Process failing index
    !
    if ( present ( failindex ) ) then
       if ( vstring_isincharset ) then
          failindex = VSTRING_INDEX_UNKNOWN
       else
          failindex = icharacter
       endif
    endif
  end function vstring_isincharset
  !
  ! vstring_isinasciirange --
  !   Returns .true. if string is in the given ascii range from min to max.
  !
  logical function vstring_isinasciirange ( this , asciimin , asciimax , failindex )
    implicit none
    type ( t_vstring ) , intent(in) :: this
    integer , intent(in) :: asciimin , asciimax
    integer, intent(out), optional :: failindex
    integer :: icharacter
    integer :: length
    type(t_vstring) :: currentchar
    integer :: charindex
    !
    ! Initialize
    !
    length = vstring_length ( this )
    !
    ! By default, it is in the ascii character set.
    !
    vstring_isinasciirange = .true.
    !
    ! Now find the cases when it is not in the character set.
    !
    do icharacter = 1 , length
       currentchar = vstring_index ( this , icharacter )
       charindex = vstring_iachar ( currentchar )
       call vstring_free ( currentchar )
       if ( charindex < asciimin .OR. charindex > asciimax ) then
          ! This is not in the range
          vstring_isinasciirange = .false.
          exit
       endif
    enddo
    !
    ! Process failindex
    !
    if ( present ( failindex ) ) then
       if ( vstring_isinasciirange ) then 
          failindex = VSTRING_INDEX_UNKNOWN
       else
          failindex = icharacter
       endif
    endif
  end function vstring_isinasciirange

  ! vstring_append_vstring --
  !   Append the given string at the end of the current string.
  !   If the given string (string2) is of length greater than zero,
  !   that means that the length of the current string will be greater
  !   after the call to vstring_append.
  ! Note
  !   That method can be called as a convenient alternative to vstring_concat,
  !   when the concat is to be done "in place".
  !
  subroutine vstring_append_vstring ( this , string2 )
    type ( t_vstring ) , intent(inout) :: this
    type ( t_vstring ) , intent(in) :: string2
    type(t_vstring) :: old_string
    integer :: status
    call vstring_check_string ( this , "vstring_append_vstring" , status )
    if ( status /= VSTRING_ERROR_OK ) then
       return
    endif
    call vstring_check_string ( string2 , "vstring_append_vstring" , status )
    if ( status /= VSTRING_ERROR_OK ) then
       return
    endif
    call vstring_new ( old_string , this )
    call vstring_free ( this )
    this = vstring_concat ( old_string , string2 )
    call vstring_free ( old_string )
  end subroutine vstring_append_vstring
  !
  ! vstring_append_charstring --
  !   Interface to manage character strings
  !
  subroutine vstring_append_charstring ( this , string2 )
    type ( t_vstring ) , intent(inout) :: this
    character(len=*), intent(in) :: string2
    type(t_vstring) :: vstring2
    call vstring_new ( vstring2 , string2 )
    call vstring_append_vstring ( this , vstring2 )
    call vstring_free ( vstring2 )
  end subroutine vstring_append_charstring
  !
  ! vstring_map --
  !   Replaces substrings in string based on the mapping defined by the couple (map_old , map_new). 
  !   map_old and map_new are arrays of vstrings and are of the same size so that
  !   if imap is an index no greater than the size of map_old, 
  !   map_old ( imap ) is the old string and map_new ( imap ) is the new string.
  !   Each instance of a key in the string will be replaced with its corresponding value. 
  !   Both old and new strings may be multiple characters. 
  !   If nocase is set to .true., then matching is done without regard to case differences. 
  !   Replacement is done in an ordered manner, so the old string appearing first 
  !   in the list will be checked first, and so on. The current string is only iterated over once, 
  !   so earlier replacements will have no affect for later matches.
  !   For example,
  !     vstring_map 1abcaababcabababc [abc,ab,a,1] [1,2,3,0]
  !   will return the string 01321221.
  !   Note that if an earlier key is a prefix of a later one, it will completely 
  !   mask the later one. So if the previous example is reordered like this,
  !     vstring_map 1abcaababcabababc [1,ab,a,abc] [0,2,3,1]
  !   it will return the string 02c322c222c.
  !
  function vstring_map ( this , map_old , map_new , nocase ) result ( stringmap )
    type ( t_vstring ) , intent(inout) :: this
    type ( t_vstring ) , dimension( : ), intent(in) :: map_old
    type ( t_vstring ) , dimension( : ), intent(in) :: map_new
    logical, intent(in), optional :: nocase
    type(t_vstring) :: stringmap
    integer :: imap
    integer :: map_length
    integer :: map_length_new
    character ( len = 200) :: message
    logical :: mapping_done
    integer :: start
    integer :: first
    integer :: last
    type(t_vstring) :: replaced_string
    integer :: imap_to_apply
    logical :: is_map_left_to_apply
    integer :: map_old_first
    integer :: map_new_length
    logical :: nocase_real
    type(t_vstring) :: stringmap_lower
    type(t_vstring) :: mapold_lower
    integer :: status
    !
    ! Check input data
    !
    call vstring_check_string ( this , "vstring_map" , status )
    if ( status /= VSTRING_ERROR_OK ) then
       return
    endif
    map_length = size ( map_old )
    map_length_new = size ( map_new )
    if ( map_length /= map_length_new ) then
       write ( message , * ) "Map for old and new strings are not of the same length. ", &
            " Length map_old:", map_length , &
            " Length map_new:", map_length_new , &
            " in vstring_map"
       call vstring_error ( this , message , "vstring_map" )
    endif
    !
    ! Process options
    !
    if ( present ( nocase ) ) then
       nocase_real = nocase
    else
       nocase_real = .false.
    endif
    !
    ! Check map content
    !
    do imap = 1 , map_length
       call vstring_check_string ( map_old ( imap ) , "vstring_map" , status )
       if ( status /= VSTRING_ERROR_OK ) then
          return
       endif
       call vstring_check_string ( map_new ( imap ) , "vstring_map" , status )
       if ( status /= VSTRING_ERROR_OK ) then
          return
       endif
    enddo
    !
    ! Apply map
    !
    start = 1
    mapping_done = .false.
    call vstring_new ( stringmap , this )
    !
    ! Do a loop over the characters of the string.
    !
    do
       !
       ! Computes the matches for all maps and 
       ! store the map which is the most at the left of the string.
       !
       first = vstring_length ( stringmap ) + 1
       imap_to_apply = 0
       is_map_left_to_apply = .false.
       do imap = 1, map_length
          if (nocase_real) then
             stringmap_lower = vstring_tolower ( stringmap )
             mapold_lower = vstring_tolower ( map_old ( imap ) )
             map_old_first = vstring_first ( stringmap_lower , mapold_lower , first = start )
             call vstring_free ( stringmap_lower )
             call vstring_free ( mapold_lower )
          else
             map_old_first = vstring_first ( stringmap , map_old ( imap ) , first = start )
          endif
          if ( map_old_first > 0 .AND. map_old_first < first ) then
             is_map_left_to_apply = .true.
             imap_to_apply = imap
             first = map_old_first
          endif
       enddo
       !
       ! Apply the match which is the left most.
       !
       if ( is_map_left_to_apply ) then
          last = first + vstring_length ( map_old ( imap_to_apply ) ) - 1
          replaced_string = vstring_replace ( stringmap , first , last ,  map_new ( imap_to_apply ) )
          call vstring_free ( stringmap )
          call vstring_new ( stringmap , replaced_string )
          call vstring_free ( replaced_string )
          map_new_length = vstring_length ( map_new ( imap_to_apply ) )
          start = start + map_new_length
          if ( start > vstring_length ( stringmap ) ) then
             !
             ! There are no characters left to map.
             !
             mapping_done = .true.
          endif
       else
          mapping_done = .true.
       endif
       if ( mapping_done ) then
          exit
       endif
    enddo
    !
    ! Cleanup
    !

  end function vstring_map
  !
  ! vstring_replace --
  !   Removes a range of consecutive characters from string, starting with the character whose 
  !   index is first and ending with the character whose index is last. An index of 1 refers to 
  !   the first character of the string. 
  !   If newstring is specified, then it is placed in the removed character range.
  !
  function vstring_replace ( this , first , last , newstring ) result ( stringreplace )
    type ( t_vstring ) , intent(inout) :: this
    integer, intent(in) :: first
    integer, intent(in) :: last
    type ( t_vstring ) , intent(in), optional :: newstring
    type(t_vstring) :: stringreplace
    type(t_vstring) :: part1
    type(t_vstring) :: part2
    type(t_vstring) :: newstring_real
    integer :: length
    integer :: status
    call vstring_check_string ( this , "vstring_replace" , status )
    if ( status /= VSTRING_ERROR_OK ) then
       return
    endif
    call vstring_check_index ( this , first , "vstring_replace" , status )
    if ( status /= VSTRING_ERROR_OK ) then
       return
    endif
    call vstring_check_index ( this , last , "vstring_replace" , status )
    if ( status /= VSTRING_ERROR_OK ) then
       return
    endif
    !
    ! Get optional arguments
    !
    if ( present ( newstring ) ) then
       call vstring_new ( newstring_real , newstring )
    else
       call vstring_new ( newstring_real , "" )
    endif
    !
    ! Compute part #1
    !
    if ( first > 1 ) then
       part1 = vstring_range ( this , 1 , first - 1 )
    else
       call vstring_new ( part1 , "" )
    endif
    !
    ! Compute part #2
    !
    length = vstring_length ( this )
    if ( last < length ) then
       part2 = vstring_range ( this , last + 1 , length )
    else
       call vstring_new ( part2 , "" )
    endif
    !
    ! Concatenate the result
    !
    call vstring_new ( stringreplace , part1 )
    call vstring_append ( stringreplace , newstring_real )
    call vstring_append ( stringreplace , part2 )
    !
    ! Cleanup
    !
    call vstring_free ( part1 )
    call vstring_free ( part2 )
    call vstring_free ( newstring_real )
  end function vstring_replace
  !
  ! TODO :
  ! vstring_read --
  !   Returns the vstring which is read on the given unit number,
  !   or on the standard input if no unit is given.
  ! Arguments
  !   unitnumber : the unit number where the string is to read
  ! Note
  !   Possible implementation : see get_unit_set_CH 
  !   in iso_varying_string.f90
  !

  !
  ! TODO :
  ! vstring_write --
  !   Writes the current vstring on the given unit number,
  !   or on the standard output if no unit is given.
  ! Arguments
  !   unitnumber : the unit number where the string is to write
  !   iostat, optional : the I/O status of the write statement
  ! Note
  !   Possible implementation : see put_unit_CH 
  !   in iso_varying_string.f90
  !    if(PRESENT(iostat)) then
  !       write(*, FMT='(A,/)', ADVANCE='NO', IOSTAT=iostat) string
  !    else
  !       write(*, FMT='(A,/)', ADVANCE='NO') string
  !    endif
  !
  
  !
  ! TODO :
  ! vstring_wordend --
  !   Returns the index of the character just after the last one in the word 
  !   containing character charIndex of string. A word is considered to be any contiguous range of 
  !   alphanumeric (letters or decimal digits) or underscore (connector punctuation) 
  !   characters, or any single character other than these.
  ! Arguments
  !   charIndex : the string containing the separator
  !

  !
  ! TODO :
  ! vstring_wordstart --
  !   Returns the index of the first character in the word containing character charIndex 
  !   of string. charIndex may be specified as for the index method. A word is considered 
  !   to be any contiguous range of alphanumeric (letters or decimal digits) or underscore 
  !   (connector punctuation) characters, or any single character other than these.
  ! Arguments
  !   charIndex : the string containing the separator
  !


  !
  ! vstring_check_index --
  !   Check that the given index is correct and generates an error if not.
  !
  subroutine vstring_check_index ( this , charIndex , origin , status )
    implicit none
    type ( t_vstring ) , intent(in) :: this
    integer, intent(in)         :: charIndex
    character ( len = * ), intent(in), optional :: origin
    character ( len = 200 ) :: message
    integer, intent(out), optional :: status
    integer :: length
    integer :: local_status
    if ( present ( status ) ) then
       status = VSTRING_ERROR_OK
    endif
    call vstring_check_string ( this , "vstring_check_index" , local_status )
    if ( local_status /= VSTRING_ERROR_OK ) then
       if ( present ( status ) ) then
          status = local_status
       endif
       return
    endif
    length = vstring_length ( this )
    if ( charIndex < 1 ) then
       if ( present ( status ) ) then
          status = VSTRING_ERROR_WRONGINDEX
       endif
       write ( message , * ) "Character index ", charIndex , " is less that 1 in vstring_check_index"
       if ( present ( origin ) ) then
          call vstring_error ( this , message , origin )
       else
          call vstring_error ( this , message )
       endif
    elseif ( charIndex > length ) then
       if ( present ( status ) ) then
          status = VSTRING_ERROR_WRONGINDEX
       endif
       write ( message , * ) "Character index ", charIndex , " is greater that the length ", length , &
            " in vstring_check_index"
       if ( present ( origin ) ) then
          call vstring_error ( this , message , origin )
       else
          call vstring_error ( this , message )
       endif
    endif
  end subroutine vstring_check_index
  !
  ! vstring_check_string --
  !   Check that the given string is correct and generates an error if not.
  !
  subroutine vstring_check_string ( this , origin , status )
    type ( t_vstring ) , intent(in) :: this
    character ( len = 200) :: message
    character ( len = * ), intent(in), optional :: origin
    integer, intent(out), optional :: status
    logical :: stringallocated
    if ( present ( status ) ) then
       status = VSTRING_ERROR_OK
    endif
    stringallocated = vstring_exists ( this )
    if ( .NOT.stringallocated ) then
       if ( present ( status ) ) then
          status = VSTRING_ERROR_STRINGNOTCREATED
       endif
       write ( message , * ) "String is not allocated, size :", size ( this % chars ) , &
            " in vstring_check_string"
       if ( present ( origin ) ) then
          call vstring_error ( this , message , origin )
       else
          call vstring_error ( this , message )
       endif
    endif
  end subroutine vstring_check_string
  !
  ! vstring_error --
  !   Generates an error with the given error message
  !
  subroutine vstring_error ( this , message , origin )
    implicit none
    type ( t_vstring ) , intent(in) :: this
    character ( len = * ), intent (in) :: message
    character ( len = * ), intent(in), optional :: origin
    integer :: length
    integer, parameter :: length_max = 40
    logical :: isallocated
    write ( * , * ) "Internal error in m_vstring"
    if ( present ( origin ) ) then
       write ( * , * ) "Origin : ", origin
    endif
    length =  vstring_length ( this )
    isallocated = vstring_exists ( this )
    if ( isallocated ) then
       write ( * , * ) "Length:" , length
       if ( length < length_max ) then
          write ( * , * ) "Content:", this % chars
       else
          write ( * , * ) "Content:", this % chars(1:length_max), "..."
       endif
    endif
#ifdef _VSTRING_ALLOCATABLE
    write ( * , * ) "Version : allocatable"
#endif
#ifdef _VSTRING_POINTER
    write ( * , * ) "Version : pointer"
#endif
    write ( * , * ) "Message :", trim( message )
    if ( vstring_stoponerror ) then
       STOP
    endif
  end subroutine vstring_error
  ! 
  ! vstring_set_stoponerror --
  !   Configure the behaviour of the component whenever an 
  !   error is met.
  !   If stoponerror is true, then the execution stops if an error is encountered.
  !   If stoponerror is false, then the execution continues if an error is encountered.
  !   In both cases, a message is displayed on standard output.
  ! 
  subroutine vstring_set_stoponerror ( stoponerror )
    logical , intent(in) :: stoponerror
    vstring_stoponerror = stoponerror
  end subroutine vstring_set_stoponerror
  !
  !
  ! INTERFACE TO STANDARD FORTRAN
  !
  ! These methods or routines are simply an interface of standard fortran
  ! string processing routines.
  !
  !
  ! vstring_iachar --
  !   Returns the code for the ASCII character in the character position of C.
  ! Arguments:
  !   this : the current string
  !   This is an interface to the standard fortran.
  ! Example :
  !   If this is "@", then iachar is 64.
  !
  function vstring_iachar ( this ) result ( new_iachar )
    type ( t_vstring ) , intent(in) :: this
    integer :: new_iachar
    integer :: length
    character ( len = 200 ) :: message
    character :: thechar
    length = vstring_length ( this )
    if ( length/=1 ) then
       write ( message , * ) "The length of the current string is not 1 :", length , &
            " in vstring_iachar"
       call vstring_error ( this , message )
    endif
    call vstring_cast ( this , 1 , thechar )
    new_iachar = iachar ( thechar )
  end function vstring_iachar
  !
  ! vstring_ichar --
  !   Returns the code for the character in the first character position of
  !   in the system’s native character set.
  ! Note
  !   This is an interface to the standard fortran.
  ! Arguments:
  !   this : the current string
  !
  function vstring_ichar ( this ) result ( new_ichar )
    type ( t_vstring ) , intent(in) :: this
    integer :: new_ichar
    integer :: length
    character ( len = 200 ) :: message
    character :: thechar
    length = vstring_length ( this )
    if ( length/=1 ) then
       write ( message , * ) "The length of the current string is not 1 :", length , &
            " in vstring_ichar"
       call vstring_error ( this , message )
    endif
    call vstring_cast ( this , 1 , thechar)
    new_ichar = ichar ( thechar )
  end function vstring_ichar
  !
  ! vstring_charindex --
  !   Returns the position of the start of the first occurrence of string substring
  !   as a sub-string in the current string, counting from one. If substring is not present
  !   in the current string, zero is returned. If the back argument is present and true, the
  !   return value is the start of the last occurrence rather than the first.
  ! Note :
  !   This is a simple interface to the standard fortran intrinsic "index".
  !
  function vstring_charindex ( this , substring , back ) result ( charindex )
    type ( t_vstring ) , intent(in)   :: this
    type ( t_vstring ) , intent(in)   :: substring
    logical, intent(in), optional :: back
    integer :: charindex
#ifdef _VSTRING_STATIC_BUFFER
    character ( len = VSTRING_BUFFER_SIZE ) :: this_charstring
    character ( len = VSTRING_BUFFER_SIZE ) :: substring_charstring
    integer :: this_length
    integer :: substring_length
#else
    character ( len = vstring_length ( this ) ) :: this_charstring
    character ( len = vstring_length ( substring ) ) :: substring_charstring
#endif
    logical :: back_real
    if ( present ( back ) ) then
       back_real = back
    else
       back_real = .false.
    endif
    call vstring_cast ( this , this_charstring )
    call vstring_cast ( substring , substring_charstring )
#ifdef _VSTRING_STATIC_BUFFER
    this_length = vstring_length ( this )
    substring_length = vstring_length ( substring )
    charindex = index ( this_charstring(1:this_length) , substring_charstring(1:substring_length) , back_real )
#else
    charindex = index ( this_charstring , substring_charstring , back_real )
#endif
  end function vstring_charindex
  !
  ! vstring_scan --
  !   Returns the position of a character of the current string that is in set, or zero
  !   if there is no such character. If the logical back is absent or present with value
  !   false, the position of the leftmost such character is returned. If back is present
  !   with value true, the position of the rightmost such character is returned.
  ! Note :
  !   This is a simple interface to the standard fortran intrinsic "scan".
  !
  function vstring_scan ( this , substring , back ) result ( charindex )
    type ( t_vstring ) , intent(in)   :: this
    type ( t_vstring ) , intent(in)   :: substring
    logical, intent(in), optional :: back
    integer :: charindex
#ifdef _VSTRING_STATIC_BUFFER
    character ( len = VSTRING_BUFFER_SIZE ) :: this_charstring
    character ( len = VSTRING_BUFFER_SIZE ) :: substring_charstring
    integer :: this_length
    integer :: substring_length
#else
    character ( len = vstring_length ( this ) ) :: this_charstring
    character ( len = vstring_length ( substring ) ) :: substring_charstring
#endif
    logical :: back_real
    if ( present ( back ) ) then
       back_real = back
    else
       back_real = .false.
    endif
    call vstring_cast ( this , this_charstring )
    call vstring_cast ( substring , substring_charstring )
#ifdef _VSTRING_STATIC_BUFFER
    this_length = vstring_length ( this )
    substring_length = vstring_length ( substring )
    charindex = scan ( this_charstring(1:this_length) , substring_charstring(1:substring_length) , back_real )
#else
    charindex = scan ( this_charstring , substring_charstring , back_real )
#endif
  end function vstring_scan
  !
  ! vstring_verify --
  !   Returns the default integer value 0 if each character in the current 
  !   string appears in set, or the position of a character of the current string
  !   that is not in set. If the logical back is absent or present with value false,
  !   the position of the left-most such character is returned. If back is 
  !   present with value true, the position of the rightmost such character is returned.
  ! Note :
  !   This is a simple interface to the standard fortran intrinsic "verify".
  !
  function vstring_verify ( this , substring , back ) result ( charindex )
    type ( t_vstring ) , intent(in)   :: this
    type ( t_vstring ) , intent(in)   :: substring
    logical, intent(in), optional :: back
    integer :: charindex
#ifdef _VSTRING_STATIC_BUFFER
    character ( len = VSTRING_BUFFER_SIZE ) :: this_charstring
    character ( len = VSTRING_BUFFER_SIZE ) :: substring_charstring
    integer :: this_length
    integer :: substring_length
#else
    character ( len = vstring_length ( this ) ) :: this_charstring
    character ( len = vstring_length ( substring ) ) :: substring_charstring
#endif
    logical :: back_real
    if ( present ( back ) ) then
       back_real = back
    else
       back_real = .false.
    endif
    call vstring_cast ( this , this_charstring )
    call vstring_cast ( substring , substring_charstring )
#ifdef _VSTRING_STATIC_BUFFER
    this_length = vstring_length ( this )
    substring_length = vstring_length ( substring )
    charindex = verify ( this_charstring(1:this_length) , substring_charstring(1:substring_length) , back_real )
#else
    charindex = verify ( this_charstring , substring_charstring , back_real )
#endif
  end function vstring_verify
  !
  ! vstring_adjustl --
  !   Adjusts left to return a string of the same length by removing 
  !   all leading blanks and inserting the same number of trailing blanks.
  ! Note :
  !   This is a simple interface to the standard fortran intrinsic "adjustl".
  !
  function vstring_adjustl ( this ) result ( newstring )
    type ( t_vstring ) , intent(in)   :: this
    type(t_vstring) :: newstring
#ifdef _VSTRING_STATIC_BUFFER
    character ( len = VSTRING_BUFFER_SIZE ) :: this_charstring
    character ( len = VSTRING_BUFFER_SIZE ) :: adjusted
    integer :: this_length
#else
    character ( len = vstring_length ( this ) ) :: this_charstring
    character ( len = vstring_length ( this ) ) :: adjusted
#endif
    call vstring_cast ( this , this_charstring )
    adjusted = adjustl ( this_charstring (:) )
#ifdef _VSTRING_STATIC_BUFFER
    this_length = vstring_length ( this )
    call vstring_new ( newstring , adjusted ( 1 : this_length ) )
#else
    call vstring_new ( newstring , adjusted )
#endif
  end function vstring_adjustl
  !
  ! vstring_adjustr --
  !   Adjusts right to return a string of the same length by removing 
  !   all trailing blanks and inserting the same number of leading blanks.
  ! Note :
  !   This is a simple interface to the standard fortran intrinsic "adjustr".
  !
  function vstring_adjustr ( this ) result ( newstring )
    type ( t_vstring ) , intent(in)   :: this
    type(t_vstring) :: newstring
#ifdef _VSTRING_STATIC_BUFFER
    character ( len = VSTRING_BUFFER_SIZE ) :: this_charstring
    character ( len = VSTRING_BUFFER_SIZE ) :: adjusted
    integer :: this_length
#else
    character ( len = vstring_length ( this ) ) :: this_charstring
    character ( len = vstring_length ( this ) ) :: adjusted
#endif
    call vstring_cast ( this , this_charstring )
#ifdef _VSTRING_STATIC_BUFFER
    this_length = vstring_length ( this )
    adjusted = adjustr ( this_charstring ( 1 : this_length ) )
    call vstring_new ( newstring , adjusted ( 1 : this_length ) )
#else
    adjusted = adjustr ( this_charstring )
    call vstring_new ( newstring , adjusted )
#endif
  end function vstring_adjustr
  !
  ! STATIC METHODS
  !
  !
  ! vstring_achar --
  !   Static method
  !   Returns the character located at position I in the ASCII collating sequence.
  !   This is an interface to the standard fortran.
  !
  function vstring_achar ( i ) result ( new_achar )
    integer, intent (in) :: i
    character(len=1 ) :: thechar
    type (t_vstring) :: new_achar
    thechar = achar(i)
    call vstring_new ( new_achar , thechar )
  end function vstring_achar
  !
  ! vstring_char --
  !   Static method
  !   Returns the character represented by the integer I.
  !   This is an interface to the standard fortran.
  ! Example :
  !   If i is 64, then char is "@".
  !
  function vstring_char ( i ) result ( new_achar )
    integer, intent (in) :: i
    character(len=1 ) :: thechar
    type (t_vstring) :: new_achar
    thechar = char ( i )
    call vstring_new ( new_achar , thechar )
  end function vstring_char
end module m_vstring

