!
! m_format --
!   
!   This component allows to manage fortran strings formats.
!
module m_format
  !
  ! Abstraction of a format specification.
  !
  type :: t_format
     private
     ! The format itself
     type ( t_vstring ) :: fmt
     ! The length of the format
     integer :: fmt_length
     ! The edit descriptor : L, B, I, etc...
     integer :: descriptor
     ! The total length of the edit string (the "w" of the fortran standard)
     integer :: edit_length
  end type t_format
  !
  ! Tags for all possible formats in fortran
  !
  integer, parameter :: FORMAT_DESCRIPTOR_UNKNOWN = 0
  integer, parameter :: FORMAT_DESCRIPTOR_I = 1
  integer, parameter :: FORMAT_DESCRIPTOR_B = 2
  integer, parameter :: FORMAT_DESCRIPTOR_O = 3
  integer, parameter :: FORMAT_DESCRIPTOR_Z = 4
  integer, parameter :: FORMAT_DESCRIPTOR_F = 5
  integer, parameter :: FORMAT_DESCRIPTOR_E = 6
  integer, parameter :: FORMAT_DESCRIPTOR_EN = 7
  integer, parameter :: FORMAT_DESCRIPTOR_ES = 8
  integer, parameter :: FORMAT_DESCRIPTOR_D = 9
  integer, parameter :: FORMAT_DESCRIPTOR_G = 10
  integer, parameter :: FORMAT_DESCRIPTOR_L = 11
  integer, parameter :: FORMAT_DESCRIPTOR_A = 12
contains
  !
  ! format_new --
  !   Creates a format description of the given format.
  !
  function format_new ( this , fmt )
    type ( t_format ) , intent(inout) :: this
    character(len=*) , intent(in) :: fmt
    type ( t_vstring ) :: fmtpart
    integer :: fmt_length
    logical :: equals
    character (len=2) :: edit_descriptor
    !
    ! Store the format
    !
    call vstring_new ( this % fmt , fmt )
    this % fmt_length = vstring_length ( this % fmt )
    !
    ! Analyse the first letter to get the edit descriptor :
    ! it must be an opening parenthesis.
    !
    fmtpart = vstring_index ( this % fmt , 1 )
    equals = vstring_equals ( fmtpart , "(" )
    call vstring_free ( fmtpart )
    if ( .NOT. equals ) then
       call format_error ( this , "The first letter is expected to be an opening parenthesis" )
    endif
    !
    ! Analyse the last letter to get the edit descriptor :
    ! it must be a closing parenthesis.
    !
    fmtpart = vstring_index ( this % fmt , this % fmt_length )
    equals = vstring_equals ( fmtpart , ")" )
    call vstring_free ( fmtpart )
    if ( .NOT. equals ) then
       call format_error ( this , "The first letter is expected to be a closing parenthesis" )
    endif
    !
    ! Analyse the second letter to get the edit descriptor :
    ! it must be the letter of the edit descriptor
    !
    this % descriptor = FORMAT_DESCRIPTOR_UNKNOWN
    !
    ! Case 1 : the format is made of 1 letter only
    !
    fmtpart = vstring_index ( this % fmt , 2 )
    call vstring_cast ( fmtpart , edit_descriptor )
    call vstring_free ( fmtpart )
    select case ( edit_descriptor )
    case ( "I" )
       this % descriptor = FORMAT_DESCRIPTOR_I
    case ( "B" )
       this % descriptor = FORMAT_DESCRIPTOR_B
    case ( "O" )
       this % descriptor = FORMAT_DESCRIPTOR_O
    case ( "Z" )
       this % descriptor = FORMAT_DESCRIPTOR_Z
    case ( "F" )
       this % descriptor = FORMAT_DESCRIPTOR_F
    case ( "E" )
       this % descriptor = FORMAT_DESCRIPTOR_E
    case ( "D" )
       this % descriptor = FORMAT_DESCRIPTOR_D
    case ( "G" )
       this % descriptor = FORMAT_DESCRIPTOR_G
    case ( "L" )
       this % descriptor = FORMAT_DESCRIPTOR_L
    case ( "A" )
       this % descriptor = FORMAT_DESCRIPTOR_A
    case default
       ! Nothing to do
    end select
    !
    ! Case 2 : the format is made of 2 letter only
    !
    fmtpart = vstring_range ( this % fmt , 2 , 3)
    call vstring_cast ( fmtpart , edit_descriptor )
    call vstring_free ( fmtpart )
    select case ( edit_descriptor )
    case ( "EN" )
       this % descriptor = FORMAT_DESCRIPTOR_EN
    case ( "ES" )
       this % descriptor = FORMAT_DESCRIPTOR_ES
    case default
       ! Nothing to do
    end select
    !
    ! Check that the edit descriptor has been found
    !
    if ( this % descriptor == FORMAT_DESCRIPTOR_UNKNOWN ) then
       call format_error ( this , "The edit descriptor has not been found" )
    endif
    
  end function format_new
  !
  ! format_free --
  !   Destroy a format description.
  !
  subroutine format_free ( this )
    type ( t_format ) , intent(inout) :: this
    call vstring_free ( this % fmt )
  end subroutine format_free
  !
  ! format_error --
  !   Generates an error with the given error message
  !
  subroutine format_error ( this , message )
    implicit none
    type ( t_format ) , intent(in) :: this
    character ( len = * ), intent (in) :: message
    integer, parameter :: length_max = 1000
    character(len=length_max) :: fmt_charstring
    write ( * , * ) "Internal error in m_stringformat"
    length =  vstring_length ( this % fmt )
    write ( * , * ) "Length:" , length
    call vstring_cast ( this % fmt , fmt_charstring )
    write ( * , * ) "Content:", trim(fmt_charstring)
    write ( * , * ) "Message :", trim( message )
    STOP
  end subroutine format_error
end module m_format

