!
! This program is a testing component for the module m_vstring.
!
program test_m_vstringformat
  use m_vstring, only : &
       t_vstring ,&
       vstring_new, &
       vstring_free, &
       vstring_length, &
       vstring_equals , &
       vstring_concat , &
       vstring_compare , &
       vstring_trim , &
       vstring_first , &
       vstring_range , &
       vstring_index , &
       vstring_last, &
       vstring_trimleft , &
       vstring_trimright , &
       vstring_cast , &
       vstring_toupper , &
       vstring_tolower , &
       vstring_totitle , &
       vstring_reverse ,&
       vstring_random , &
       vstring_reference_get, &
       VSTRING_INDEX_UNKNOWN , &
       vstring_achar, &
       vstring_char , &
       vstring_iachar, &
       vstring_ichar , &
       vstring_append, &
       vstring_map, &
       vstring_replace, &
       vstring_charindex, &
       vstring_scan, &
       vstring_verify, &
       vstring_match, &
       vstring_adjustl ,&
       vstring_adjustr ,&
       vstring_is , &
       vstring_set_stoponerror
  use m_vstringformat, only : &
       vstring_format
  implicit none
  integer :: assertTotalTestSuccess
  integer :: assertTotalTestFail
  integer :: assertTestIndex
  integer , parameter :: log_unit = 12
  call test_main ()
contains
  !
  ! Include support for unit tests.
  !
  include "test_support.f90"
  !
  ! test_main --
  !   Main subroutine for tests.
  !
  subroutine test_main ()
    implicit none
    call log_startup ( "test_m_vstringformat.log" )
    call assert_startup ( )
    call vstring_set_stoponerror ( .false. )
    !
    ! Test #0
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! Test vstring_format logical
    !
    call test_m_vstring_format_logical ()
    !
    ! Test vstring_format integer
    !
    call test_m_vstring_format_integer ()
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! Shutdown the tests
    !
    call logmsg ( "Test suite completed" )
    call assert_shutdown ()
    call log_shutdown ()
  end subroutine test_main
  !
  ! Check that there are no memory leaks.
  !
  subroutine string_reference_check ()
    implicit none
    integer :: vstring_reference
    character (len=200) :: msg
    vstring_reference = vstring_reference_get()
    write ( msg , * ) "Number of strings :", vstring_reference
    call logmsg (msg)
    if (vstring_reference /= 0) then
       continue
    endif
    call assert ( vstring_reference == 0, "Wrong number of strings references.")
  end subroutine string_reference_check
  !
  ! Format a string from an integer
  !
  subroutine test_m_vstring_format_integer
    type ( t_vstring ) :: mystring
    ! 64 = 2**(8-1) = 2**7
    integer (kind=1) :: integerkind1 = 64
    ! 16384 = 2**15
    integer (kind=2) :: integerkind2 = 16384
    ! 1073741824 = 2**31
    integer (kind=4) :: integerkind4 = 1073741824
    ! 4611686018427387904 = 2**63
    integer (kind=8) :: integerkind8 = 4611686018427387904
    integer (kind=1) :: maxkind1
    integer (kind=2) :: maxkind2
    integer (kind=4) :: maxkind4
    integer (kind=8) :: maxkind8
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! Initial tests
    !
    maxkind1 = test_maxint_kind1 ( )
    maxkind2 = test_maxint_kind2 ( )
    maxkind4 = test_maxint_kind4 ( )
    maxkind8 = test_maxint_kind8 ( )
    !
    ! Format integer with automatic format
    !
    call logmsg ( "Test #format : format an integer 0" )
    mystring = vstring_format ( 0 )
    call assertVstring_charstring ( mystring , "0" , "Wrong vstring_format" )
    call vstring_free ( mystring )
    !
    call logmsg ( "Test #format : format an integer 1" )
    mystring = vstring_format ( 1 )
    call assertVstring_charstring ( mystring , "1" , "Wrong vstring_format" )
    call vstring_free ( mystring )
    !
    call logmsg ( "Test #format : format an integer -1" )
    mystring = vstring_format ( -1 )
    call assertVstring_charstring ( mystring , "-1" , "Wrong vstring_format" )
    call vstring_free ( mystring )
    !
    call logmsg ( "Test #format : format an integer 2008" )
    mystring = vstring_format ( 2008 )
    call assertVstring_charstring ( mystring , "2008" , "Wrong vstring_format" )
    call vstring_free ( mystring )
    !
    call logmsg ( "Test #format : format an integer -2008" )
    mystring = vstring_format ( -2008 )
    call assertVstring_charstring ( mystring , "-2008" , "Wrong vstring_format" )
    call vstring_free ( mystring )
    !
    call logmsg ( "Test #format : format aan integer with kind=1" )
    mystring = vstring_format ( integerkind1 )
    call assertVstring_charstring ( mystring , "64" , "Wrong vstring_format" )
    call vstring_free ( mystring )
    !
    call logmsg ( "Test #format : format an integer with kind=2" )
    mystring = vstring_format ( integerkind2 )
    call assertVstring_charstring ( mystring , "16384" , "Wrong vstring_format" )
    call vstring_free ( mystring )
    !
    call logmsg ( "Test #format : format an integer with kind=4" )
    mystring = vstring_format ( integerkind4 )
    call assertVstring_charstring ( mystring , "1073741824" , "Wrong vstring_format" )
    call vstring_free ( mystring )
    !
    call logmsg ( "Test #format : format an integer with kind=8" )
    mystring = vstring_format ( integerkind8 )
    call assertVstring_charstring ( mystring , "4611686018427387904" , "Wrong vstring_format" )
    call vstring_free ( mystring )
    !
    ! Format integer with given format
    !
    call logmsg ( "Test #format : format an integer 0 with given format" )
    mystring = vstring_format ( 0 , "(I5)")
    call assertVstring_charstring ( mystring , "    0" , "Wrong vstring_format" )
    call vstring_free ( mystring )
    !
    call logmsg ( "Test #format : format an integer 0 with given format" )
    mystring = vstring_format ( 0 , "(I5.3)")
    call assertVstring_charstring ( mystring , "  000" , "Wrong vstring_format" )
    call vstring_free ( mystring )
    !
    call logmsg ( "Test #format : format an integer 0 with given format" )
    mystring = vstring_format ( 0 , "(I0)")
    call assertVstring_charstring ( mystring , "0" , "Wrong vstring_format" )
    call vstring_free ( mystring )
    !
    call logmsg ( "Test #format : format an integer 0 with given format" )
    mystring = vstring_format ( 0 , "(SPI2)")
    call assertVstring_charstring ( mystring , "+0" , "Wrong vstring_format" )
    call vstring_free ( mystring )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
  end subroutine test_m_vstring_format_integer
  !
  ! Format a string from a logical
  !
  subroutine test_m_vstring_format_logical
    type ( t_vstring ) :: mystring
    type ( t_vstring ) :: myformat
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! Format logical with automatic format
    !
    call logmsg ( "Test #format : format a logical" )
    mystring = vstring_format ( .true. )
    call assertVstring_charstring ( mystring , "T" , "Wrong vstring_format" )
    call vstring_free ( mystring )
    !
    call logmsg ( "Test #format : format a logical" )
    mystring = vstring_format ( .false. )
    call assertVstring_charstring ( mystring , "F" , "Wrong vstring_format" )
    call vstring_free ( mystring )
    !
    ! Format logical with given character format
    !
    call logmsg ( "Test #format : format a logical" )
    mystring = vstring_format ( .true. , "(L3)" )
    call assertVstring_charstring ( mystring , "  T" , "Wrong vstring_format" )
    call vstring_free ( mystring )
    !
    ! Format logical with given string format
    !
    call logmsg ( "Test #format : format a logical" )
    call vstring_new ( myformat , "(L3)" )
    mystring = vstring_format ( .true. , myformat )
    call assertVstring_charstring ( mystring , "  T" , "Wrong vstring_format" )
    call vstring_free ( mystring )
    call vstring_free ( myformat )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
  end subroutine test_m_vstring_format_logical
  !
  ! assertString --
  !   Check that the computed string is equal to the expected string
  !   and updates the assertion system.
  !
  subroutine assertString ( computedString , expectedString , message )
    implicit none
    character(len=*), intent(in) :: computedString
    character(len=*), intent(in) :: expectedString
    character(len=*), intent(in) :: message
    integer, parameter :: MSG_LEN = 200
    integer :: stringlength
    integer :: icharacter
    character :: computedCharacter
    character :: expectedCharacter
    integer :: computedLength
    integer :: expectedLength
    logical :: test
    character(len=300) :: msg
    !
    ! By default the test pass
    !
    test = .true.
    !
    ! Now compute the cases where the test does not pass.
    ! 1. Check string length
    !
    computedLength = len(computedString)
    expectedLength = len(expectedString)
    if (computedLength/=expectedLength ) then
       test = .false.
    endif
    if (test) then
       stringlength = computedLength
       do icharacter = 1 , stringlength
          computedCharacter (1:)= computedString (icharacter:icharacter)
          expectedCharacter (1:)= expectedString (icharacter:icharacter)
          if (computedCharacter/=expectedCharacter) then
             test = .false.
             exit
          endif
       enddo
    endif
    if ( .NOT.test) then
       write(msg,*) "Expected string length:", expectedLength
       call logmsg ( msg )
       write(msg,*) "Computed string length:", computedLength
       call logmsg ( msg )
       write(msg,*) "Expected string:", expectedString
       call logmsg ( msg )
       write(msg,*) "Computed string:", computedString
       call logmsg ( msg )
    endif
    !
    ! Assert that test is true.
    !
    call assert ( test , message )
  end subroutine assertString

  !
  ! assertString --
  !   Check that the computed character is equal to the expected character
  !   and updates the assertion system.
  !
  subroutine assertCharacter ( computedCharacter , expectedCharacter , message )
    implicit none
    character, intent(in) :: computedCharacter
    character, intent(in) :: expectedCharacter
    character(len=*), intent(in) :: message
    logical :: test
    character(len=300) :: msg
    if (computedCharacter/=expectedCharacter) then
       test = .false.
    else
       test = .true.
    endif
    if ( .NOT.test) then
       write(msg,*) "Expected character:", expectedCharacter
       call logmsg ( msg )
       write(msg,*) "Computed character:", computedCharacter
       call logmsg ( msg )
    endif
    call assert ( test , message )
  end subroutine assertCharacter
  !
  ! log_vstring --
  !   Log a vstring
  !
  subroutine log_vstring ( string )
    implicit none
    type ( t_vstring ), intent(in) :: string
    character ( len = 200 ) :: msg
    call vstring_cast ( string , len ( msg ) , msg )
    call logmsg ( msg )
  end subroutine log_vstring
  !
  ! assertVstring_vstring --
  !   Check that the computed vstring is equal to the expected string
  !   and updates the assertion system.
  !
  subroutine assertVstring_vstring ( computedString , expectedString , message )
    implicit none
    type ( t_vstring ), intent(in) :: computedString
    type ( t_vstring ), intent(in) :: expectedString
    character(len=*), intent(in) :: message
    character ( len = 200 ) :: msg
    character ( len = 200 ) :: char_string
    logical :: equals
    equals = vstring_equals ( computedString , expectedString )
    call vstring_cast ( expectedString , len ( char_string ) , char_string )
    ! CAUTION !
    ! The trim removes the blanks so that the blanks of the real string are not displayed !
    if ( .NOT.equals ) then
       write ( msg , * ) "String expected :-", trim ( char_string ), "-"
       call logmsg ( msg )
       call vstring_cast ( computedString , len ( char_string ) , char_string )
       write ( msg , * ) "String computed :-", trim ( char_string ) , "-"
       call logmsg ( msg )
    endif
    call assert ( equals , message )
  end subroutine assertVstring_vstring
  !
  ! assertVstring_charstring --
  !   Interface to assertVstring_vstring.
  !
  subroutine assertVstring_charstring ( computedString , expectedString , message )
    implicit none
    type ( t_vstring ), intent(in) :: computedString
    character(len=*), intent(in) :: expectedString
    character(len=*), intent(in) :: message
    type ( t_vstring ) :: expectedVString
    call vstring_new ( expectedVString , expectedString )
    call assertVstring_vstring ( computedString , expectedVString , message )
    call vstring_free ( expectedVString )
  end subroutine assertVstring_charstring
  !
  ! Compute the maximum number of type kind=4
  ! The expected value is 2**31.
  !
  function test_maxint_kind4 ( ) result ( maxkind )
    integer (kind=4) :: integerkind
    integer (kind=4) :: maxkind
    integer :: power
    integer , parameter :: powermax = 128
    !
    ! Multiplies by 2, until there is an overflow
    !
    integerkind = 1
    do power = 1 , powermax
       maxkind = integerkind
       integerkind = integerkind * 2
       if ( integerkind <= 0 ) then
          exit
       endif
    enddo
  end function test_maxint_kind4
  !
  ! Compute the maximum number of type kind=8.
  ! The expected value is 2**63.
  !
  function test_maxint_kind8 ( ) result ( maxkind )
    integer (kind=8) :: integerkind
    integer (kind=8) :: maxkind
    integer :: power
    integer , parameter :: powermax = 128
    !
    ! Multiplies by 2, until there is an overflow
    !
    integerkind = 1
    do power = 1 , powermax
       maxkind = integerkind
       integerkind = integerkind * 2
       if ( integerkind <= 0 ) then
          exit
       endif
    enddo
  end function test_maxint_kind8
  !
  ! Compute the maximum number of type kind=2.
  ! The expected value is 2**15.
  !
  function test_maxint_kind2 ( ) result ( maxkind )
    integer (kind=2) :: integerkind
    integer (kind=2) :: maxkind
    integer :: power
    integer , parameter :: powermax = 128
    !
    ! Multiplies by 2, until there is an overflow
    !
    integerkind = 1
    do power = 1 , powermax
       maxkind = integerkind
       integerkind = integerkind * 2
       if ( integerkind <= 0 ) then
          exit
       endif
    enddo
  end function test_maxint_kind2
  !
  ! Compute the maximum number of type kind=1.
  ! The expected value is 2**7.
  !
  function test_maxint_kind1 ( ) result ( maxkind )
    integer (kind=1) :: integerkind
    integer (kind=1) :: maxkind
    integer :: power
    integer , parameter :: powermax = 128
    !
    ! Multiplies by 2, until there is an overflow
    !
    integerkind = 1
    do power = 1 , powermax
       maxkind = integerkind
       integerkind = integerkind * 2
       if ( integerkind <= 0 ) then
          exit
       endif
    enddo
  end function test_maxint_kind1
end program test_m_vstringformat


