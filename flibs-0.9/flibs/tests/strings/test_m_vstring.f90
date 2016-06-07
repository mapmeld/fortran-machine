!
! This program is a testing component for the module m_vstring.
!
program test_m_vstring
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
    call log_startup ( "test_m_vstring.log" )
    call assert_startup ( )
    call vstring_set_stoponerror ( .false. )
    !
    ! Test #0
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! Test vstring_new
    !
    call test_m_vstring_new ()
    !
    ! Test everything else...
    !
    call test_m_vstring_all ()
    !
    ! Test string case management.
    !
    call test_m_vstring_case ()
    !
    ! Test vstring_first , vstring_last
    !
    call test_m_vstring_firstlast ()
    !
    ! Test string trim, left, right, both.
    ! Note:
    ! Test vstring_trim after vstring_first, because vstring_trim calls vstring_first.
    !
    call test_m_vstring_trim ()
    !
    ! Test interfaces to standard fortran
    !
    call test_m_vstring_stdfortran ()
    !
    ! Test string matching
    ! Fails with gfortran 2007
    !
    call test_m_vstring_match ()
    !
    ! Test string convert
    !
    call test_m_vstring_cast ()
    !
    ! Test string is
    !
    call test_m_vstring_is ()
    !
    ! TODO :
    ! Extend to match Tcl string:
    ! string wordend, 
    ! string wordstart,
    ! string bytelength
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
  ! Test the vstring_new command
  !
  subroutine test_m_vstring_new ()
    implicit none
    type ( t_vstring ) :: string1
    type ( t_vstring ) :: string2
    integer :: length
    character ( len = 1) , dimension( 1:5), parameter :: mychararray = (/"m","y"," ","s","t"/)
    !
    ! test #1.0
    !
    call logmsg ( "Test #1.0 : new emty" )
    call vstring_new ( string1 )
    call vstring_free ( string1 )
    !
    ! test #1.1
    !
    call logmsg ( "Test #1.1 : new from charstring" )
    call vstring_new ( string1 , "my string" )
    call vstring_free ( string1 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #1.2
    !
    call logmsg ( "Test #1.2 : new from vstring" )
    call vstring_new ( string1 , "my string" )
    call vstring_new ( string2 , string1 )
    call assertVstring_vstring ( string1 , string2 , "Wrong string content. (4)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #1.3
    !
    call logmsg ( "Test #1.3 : new from char array" )
    call vstring_new ( string1 , mychararray )
    length = vstring_length ( string1 )
    call assert ( length==5 , "Wrong string length. (1)" )
    call vstring_free ( string1 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #1.4
    !
    call logmsg ( "Test #1.4 : new from integer" )
    call vstring_new ( string1 , 10 )
    call assertVstring_charstring ( string1 , "          " , "Wrong vstring_new. (4)" )
    call vstring_free ( string1 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #1.5
    !
    call logmsg ( "Test #1.5 : new from integer and string" )
    call vstring_new ( string1 , "to" )
    call vstring_new ( string2 , 2 , string = string1 )
    call assertVstring_charstring ( string2 , "toto" , "Wrong vstring_new. (4)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #1.6
    !
    call logmsg ( "Test #1.4 : new from zero integer" )
    call vstring_new ( string1 , 0 )
    call assertVstring_charstring ( string1 , "" , "Wrong vstring_new. (4)" )
    call vstring_free ( string1 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #1.7
    !
    call logmsg ( "Test #1.7 : new from integer and string" )
    call vstring_new ( string1 , "to" )
    call vstring_new ( string2 , 3 , string = string1 )
    call assertVstring_charstring ( string2 , "tototo" , "Wrong vstring_new. (4)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
  end subroutine test_m_vstring_new
  !
  ! Test vstring_match
  !
  subroutine test_m_vstring_match ()
    implicit none
    type ( t_vstring ) :: string1
    type ( t_vstring ) :: pattern
    logical :: match
    !
    call logmsg ( "Test #40.1 vstring_match with star" )
    call vstring_new ( string1 , "1abcaababcabababc" )
    call vstring_new ( pattern , "*" )
    match = vstring_match ( string1 , pattern )
    call assert ( match , "Wrong vstring_match" )
    call vstring_free ( string1 )
    call vstring_free ( pattern )
    !
    call logmsg ( "Test #40.1.b vstring_match with char string pattern" )
    call vstring_new ( string1 , "1abcaababcabababc" )
    match = vstring_match ( string1 , "*" )
    call assert ( match , "Wrong vstring_match" )
    call vstring_free ( string1 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    call logmsg ( "Test #40.2 vstring_match without special characters" )
    call vstring_new ( string1 , "1abcaababcabababc" )
    call vstring_new ( pattern , "1abcaababcabababc" )
    match = vstring_match ( string1 , pattern )
    call assert ( match , "Wrong vstring_match" )
    call vstring_free ( string1 )
    call vstring_free ( pattern )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    call logmsg ( "Test #40.3 vstring_match with question" )
    call vstring_new ( string1 , "1" )
    call vstring_new ( pattern , '?' )
    match = vstring_match ( string1 , pattern )
    call assert ( match , "Wrong vstring_match" )
    call vstring_free ( string1 )
    call vstring_free ( pattern )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    call logmsg ( "Test #40.4 vstring_match with question" )
    call vstring_new ( string1 , "12" )
    call vstring_new ( pattern , "1?" )
    match = vstring_match ( string1 , pattern )
    call assert ( match , "Wrong vstring_match" )
    call vstring_free ( string1 )
    call vstring_free ( pattern )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    call logmsg ( "Test #40.5 vstring_match with star" )
    call vstring_new ( string1 , "12" )
    call vstring_new ( pattern , "1*" )
    match = vstring_match ( string1 , pattern )
    call assert ( match , "Wrong vstring_match" )
    call vstring_free ( string1 )
    call vstring_free ( pattern )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    call logmsg ( "Test #40.6 vstring_match with star" )
    call vstring_new ( string1 , "" )
    call vstring_new ( pattern , "*" )
    match = vstring_match ( string1 , pattern )
    call assert ( match , "Wrong vstring_match" )
    call vstring_free ( string1 )
    call vstring_free ( pattern )
    !
    call logmsg ( "vstring_match with star pattern (2)" )
    call vstring_new ( string1 , "" )
    call vstring_new ( pattern , "*.txt" )
    match = vstring_match ( string1 , pattern )
    call assert ( .NOT.match , "Wrong vstring_match" )
    call vstring_free ( string1 )
    call vstring_free ( pattern )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! Test #40.7
    !
    call logmsg ( "Test #40.7 vstring_match with question" )
    call vstring_new ( string1 , "" )
    call vstring_new ( pattern , "?" )
    match = vstring_match ( string1 , pattern )
    call assert ( .NOT.match , "Wrong vstring_match" )
    call vstring_free ( string1 )
    call vstring_free ( pattern )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! Test #40.7b
    !
    call logmsg ( "Test #40.7b vstring_match with question" )
    call vstring_new ( string1 , "a" )
    call vstring_new ( pattern , "?" )
    match = vstring_match ( string1 , pattern )
    call assert ( match , "Wrong vstring_match" )
    call vstring_free ( string1 )
    call vstring_free ( pattern )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! Test #40.8
    !
    call logmsg ( "Test #40.8 vstring_match with star" )
    call vstring_new ( string1 , "___292" )
    call vstring_new ( pattern , "*292" )
    match = vstring_match ( string1 , pattern )
    call assert ( match , "Wrong vstring_match" )
    call vstring_free ( string1 )
    call vstring_free ( pattern )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! Test #40.9
    !
    call logmsg ( "Test #40.9 vstring_match with backslash" )
    call vstring_new ( string1 , "?292" )
    call vstring_new ( pattern , "\?292" )
    match = vstring_match ( string1 , pattern )
    call assert ( match , "Wrong vstring_match" )
    call vstring_free ( string1 )
    call vstring_free ( pattern )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! Test #40.10
    !
    call logmsg ( "Test #40.10 vstring_match with backslash" )
    call vstring_new ( string1 , "?" )
    call vstring_new ( pattern , "\?" )
    match = vstring_match ( string1 , pattern )
    call assert ( match , "Wrong vstring_match" )
    call vstring_free ( string1 )
    call vstring_free ( pattern )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! Test #41.1
    !
    call logmsg ( "Test #41.1 vstring_match with chars set" )
    call vstring_new ( string1 , "a" )
    call vstring_new ( pattern , "[]" )
    match = vstring_match ( string1 , pattern )
    call assert ( .NOT.match , "Wrong vstring_match" )
    call vstring_free ( string1 )
    call vstring_free ( pattern )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! Test #41.2
    !
    call logmsg ( "Test #41.2 vstring_match with chars set" )
    call vstring_new ( string1 , "a" )
    call vstring_new ( pattern , "[a]" )
    match = vstring_match ( string1 , pattern )
    call assert ( match , "Wrong vstring_match" )
    call vstring_free ( string1 )
    call vstring_free ( pattern )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! Test #41.3
    !
    call logmsg ( "Test #41.3 vstring_match with chars set" )
    call vstring_new ( string1 , "a" )
    call vstring_new ( pattern , "[c]" )
    match = vstring_match ( string1 , pattern )
    call assert ( .NOT.match , "Wrong vstring_match" )
    call vstring_free ( string1 )
    call vstring_free ( pattern )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! Test #41.4
    !
    call logmsg ( "Test #41.4 vstring_match with chars set" )
    call vstring_new ( string1 , "dc" )
    call vstring_new ( pattern , "[abcde]c" )
    match = vstring_match ( string1 , pattern )
    call assert ( match , "Wrong vstring_match" )
    call vstring_free ( string1 )
    call vstring_free ( pattern )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! Test #41.5
    !
    call logmsg ( "Test #41.5 vstring_match with chars set range" )
    call vstring_new ( string1 , "dc" )
    call vstring_new ( pattern , "[a-e]c" )
    match = vstring_match ( string1 , pattern )
    call assert ( match , "Wrong vstring_match" )
    call vstring_free ( string1 )
    call vstring_free ( pattern )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! Test #41.6
    !
    call logmsg ( "Test #41.6 vstring_match with chars set range" )
    call vstring_new ( string1 , "Vc" )
    call vstring_new ( pattern , "[a-eU-Z]c" )
    match = vstring_match ( string1 , pattern )
    call assert ( match , "Wrong vstring_match" )
    call vstring_free ( string1 )
    call vstring_free ( pattern )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
#ifndef _IVF8
    !
    ! Test #41.7
    !
    call logmsg ( "Test #41.7 vstring_match with chars set range" )
    call vstring_new ( string1 , "_c" )
    call vstring_new ( pattern , "[a-eU-Z_]c" )
    match = vstring_match ( string1 , pattern )
    call assert ( match , "Wrong vstring_match" )
    call vstring_free ( string1 )
    call vstring_free ( pattern )
#endif
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! Test #41.8
    !
    call logmsg ( "Test #41.8 vstring_match with nocase" )
    call vstring_new ( string1 , "Ac" )
    call vstring_new ( pattern , "[a-e]c" )
    match = vstring_match ( string1 , pattern , nocase = .true. )
    call assert ( match , "Wrong vstring_match" )
    call vstring_free ( string1 )
    call vstring_free ( pattern )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! Test #41.9
    !
    call logmsg ( "Test #41.9 vstring_match with nocase" )
    call vstring_new ( string1 , "_" )
    call vstring_new ( pattern , "[A-z]" )
    match = vstring_match ( string1 , pattern )
    call assert ( match , "Wrong vstring_match" )
    call vstring_free ( string1 )
    call vstring_free ( pattern )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! Test #41.10
    !
    call logmsg ( "Test #41.10 vstring_match with nocase" )
    call vstring_new ( string1 , "_" )
    call vstring_new ( pattern , "[A-z]" )
    match = vstring_match ( string1 , pattern , nocase = .true. )
    call assert ( .NOT.match , "Wrong vstring_match" )
    call vstring_free ( string1 )
    call vstring_free ( pattern )
    !
    call logmsg ( "Typical use" )
    call vstring_new ( string1 , "m_vstring.f90" )
    call vstring_new ( pattern , 'm_*.f90' )
    match = vstring_match ( string1 , pattern )
    call assert ( match , "Wrong vstring_match" )
    call vstring_free ( string1 )
    call vstring_free ( pattern )
    !
    call logmsg ( "Typical use" )
    call vstring_new ( string1 , "D:/dir1/dir2/dir3/dirname" )
    call vstring_new ( pattern , '*.txt' )
    match = vstring_match ( string1 , pattern )
    call assert ( .NOT.match , "Wrong vstring_match" )
    call vstring_free ( string1 )
    call vstring_free ( pattern )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
  end subroutine test_m_vstring_match
  !
  ! Test all other vstring methods. 
  !
  subroutine test_m_vstring_all ()
    implicit none
    type ( t_vstring ) :: string1
    type ( t_vstring ) :: string2
    type ( t_vstring ) :: string3
    type ( t_vstring ) :: string4
    logical :: equals
    integer :: length
    integer :: compare
    integer :: charindex
    character ( len = 1) , dimension( 1:5), parameter :: mychararray = (/"m","y"," ","s","t"/)
    type ( t_vstring ), dimension(1:4) :: map_old1
    type ( t_vstring ), dimension(1:4) :: map_new1
    integer :: imap
    
    !
    ! test #2.1 -> generates an error, as expected
    !
    !!$  call logmsg ( "Test #2.1 : length of an empty string" )
    !!$  length = vstring_length(string1)
    !!$  call assert ( length==0 , "Wrong string length. (1)" )
    !
    ! test #2.2
    !
    call logmsg ( "Test #2.2 : length of a non-empty string" )
    call vstring_new ( string1 , "my string" )
    length = vstring_length(string1)
    call assert ( length==9 , "Wrong string length. (2)" )
    call vstring_free ( string1 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #5.1
    !
    call logmsg ( "Test #5.1 : equals vstring == vstring " )
    call vstring_new ( string1 , "my string" )
    call vstring_new ( string2 , "my string" )
    call assertVstring_vstring ( string1 , string2 , "Wrong string content. (3)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #5.2
    !
    call logmsg ( "Test #5.2 : equals vstring == vstring with a blank" )
    call vstring_new ( string1 , "my string" )
    call vstring_new ( string2 , "my string " )
    equals = vstring_equals( string1, string2 )
    call assert ( .NOT.equals , "Wrong string content. (5)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! test #5.2.b
    !
    call logmsg ( "Test #5.2.b : equals with a constant string" )
    call vstring_new ( string1 , "my string" )
    equals = vstring_equals ( string1, "my string " )
    call assert ( .NOT.equals , "Wrong string content. (5)" )
    call vstring_free ( string1 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #5.3
    !
    call logmsg ( "Test #5.3 : equals vstring == vstring with nocase" )
    call vstring_new ( string1 , "my sTring" )
    call vstring_new ( string2 , "my stRing" )
    call logmsg ( "String #1" )
    call log_vstring ( string1 )
    call logmsg ( "String #2" )
    call log_vstring ( string2 )
    equals = vstring_equals( string1, string2 , nocase = .true. )
    call assert ( equals , "Wrong string content. (5)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #5.4
    !
    call logmsg ( "Test #5.4 : equals vstring == vstring with nocase and length" )
    call vstring_new ( string1 , "my sTring  " )
    call vstring_new ( string2 , "my stRing" )
    call logmsg ( "String #1" )
    call log_vstring ( string1 )
    call logmsg ( "String #2" )
    call log_vstring ( string2 )
    equals = vstring_equals( string1, string2 , nocase = .true. , length = 6 )
    call assert ( equals , "Wrong string content. (5)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #5.5
    !
    call logmsg ( "Test #5.5 : equals vstring == vstring with nocase and length" )
    call vstring_new ( string1 , "my sTring  " )
    call vstring_new ( string2 , "my stRing" )
    call logmsg ( "String #1" )
    call log_vstring ( string1 )
    call logmsg ( "String #2" )
    call log_vstring ( string2 )
    equals = vstring_equals( string1, string2 , nocase = .false. , length = 4 )
    call assert ( equals , "Wrong string content. (5)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #5.6
    !
    call logmsg ( "Test #5.6 : equals vstring == vstring with nocase and length" )
    call vstring_new ( string1 , "my sTring  " )
    call vstring_new ( string2 , "my stRing" )
    call logmsg ( "String #1" )
    call log_vstring ( string1 )
    call logmsg ( "String #2" )
    call log_vstring ( string2 )
    equals = vstring_equals( string1, string2 , nocase = .false. , length = 5 )
    call assert ( .NOT.equals , "Wrong string content. (5)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #8
    !
    call logmsg ( "Test #8 : concat" )
    call vstring_new ( string1 , "my string" )
    call vstring_new ( string2 , " is very interesting" )
    string3 = vstring_concat ( string1 , string2 )
    call vstring_new ( string4 , "my string is very interesting" )
    call assertVstring_vstring ( string3 , string4 , "Wrong string content. (3)" )
    length = vstring_length( string3 )
    call assert ( length==29 , "Wrong string length. (2)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    call vstring_free ( string3 )
    call vstring_free ( string4 )
    !
    ! test #8.b
    !
    call logmsg ( "Test #8.b : concat with char string" )
    call vstring_new ( string1 , "my string" )
    string3 = vstring_concat ( string1 , " is very interesting" )
    call assertVstring_charstring ( string3 , "my string is very interesting" , "Wrong string content. (3)" )
    length = vstring_length ( string3 )
    call assert ( length==29 , "Wrong string length. (2)" )
    call vstring_free ( string1 )
    call vstring_free ( string3 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #9.1
    !
    call logmsg ( "Test #9.1 : compare" )
    call vstring_new ( string1 , "my string" )
    call vstring_new ( string2 , "is very interesting" )
    compare = vstring_compare ( string1 , string2 )
    call assert ( compare == 1 , "Wrong compare. (1)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! test #9.1.b
    !
    call logmsg ( "Test #9.1.b : compare with char string" )
    call vstring_new ( string1 , "my string" )
    compare = vstring_compare ( string1 , "is very interesting" )
    call assert ( compare == 1 , "Wrong compare. (1)" )
    call vstring_free ( string1 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #9.2
    !
    call logmsg ( "Test #9.2 : compare" )
    call vstring_new ( string1 , "is very interesting" )
    call vstring_new ( string2 , "my string" )
    compare = vstring_compare ( string1 , string2 )
    call assert ( compare == -1 , "Wrong compare. (1)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #9.3
    !
    call logmsg ( "Test #9.3 : compare" )
    call vstring_new ( string1 , "my string" )
    call vstring_new ( string2 , "my string " )
    compare = vstring_compare ( string1 , string2 )
    call assert ( compare == -1 , "Wrong compare. (1)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #9.4
    !
    call logmsg ( "Test #9.4 : compare" )
    call vstring_new ( string1 , "my string" )
    call vstring_new ( string2 , "my string" )
    compare = vstring_compare ( string1 , string2 )
    call assert ( compare == 0 , "Wrong compare. (1)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #9.5
    !
    call logmsg ( "Test #9.5 : compare" )
    call vstring_new ( string1 , "my string " )
    call vstring_new ( string2 , "my string" )
    compare = vstring_compare ( string1 , string2 )
    call assert ( compare == 1 , "Wrong compare. (1)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #9.6
    !
    call logmsg ( "Test #9.6 : compare with encoding" )
    call vstring_new ( string1 , "my stringë" )
    call vstring_new ( string2 , "my stringé" )
    compare = vstring_compare ( string1 , string2 )
    call assert ( compare == 1 , "Wrong compare. (1)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #9.7
    !
    call logmsg ( "Test #9.7 : compare with no case" )
    call vstring_new ( string1 , "MY STRING      " )
    call vstring_new ( string2 , "my string" )
    compare = vstring_compare ( string1 , string2 , nocase = .true. )
    call assert ( compare == 1 , "Wrong compare. (1)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #9.8
    !
    call logmsg ( "Test #9.8 : compare with no case and maximal length" )
    call vstring_new ( string1 , "MY STRING      " )
    call vstring_new ( string2 , "my string" )
    compare = vstring_compare ( string1 , string2 , nocase = .true. , length = 9 )
    call assert ( compare == 0 , "Wrong compare. (1)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #9.9
    !
    call logmsg ( "Test #9.9 : compare with no case" )
    call vstring_new ( string1 , "MY STRING" )
    call vstring_new ( string2 , "my string" )
    compare = vstring_compare ( string1 , string2 , nocase = .true. )
    call assert ( compare == 0 , "Wrong compare. (1)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #17.1
    !
    call logmsg ( "Test #17.1 : string range" )
    call vstring_new ( string1 , "my string" )
    string2 = vstring_range ( string1 , 2 , 3 )
    call vstring_new ( string3 , "y " )
    equals = vstring_equals ( string2 , string3 )
    call assert ( equals , "Wrong string_range. (1)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    call vstring_free ( string3 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #17.2 -> generates an expected error and stops as expected
    !
    !!$  call logmsg ( "Test #17.2 : string range" )
    !!$  call vstring_new ( string1 , "my string" )
    !!$  string2 = vstring_range ( string1 , -12 , 3 )
    !!$  call vstring_new ( string3 , "my " )
    !!$  equals = vstring_equals ( string2 , string3 )
    !!$  call assert ( equals , "Wrong string_range. (1)" )
    !!$  call vstring_free ( string1 )
    !!$  call vstring_free ( string2 )
    !!$  call vstring_free ( string3 )
    !
    ! test #17.3 -> generates an expected error and stops as expected
    !
    !!$  call logmsg ( "Test #17.3 : string range" )
    !!$  call vstring_new ( string1 , "my string" )
    !!$  string2 = vstring_range ( string1 , -12 , 53 )
    !!$  call vstring_new ( string3 , "my string" )
    !!$  equals = vstring_equals ( string2 , string3 )
    !!$  call assert ( equals , "Wrong string_range. (1)" )
    !!$  call vstring_free ( string1 )
    !!$  call vstring_free ( string2 )
    !!$  call vstring_free ( string3 )
    !
    ! test #17.4 -> generates an expected error and stops as expected
    !
    !!$  call logmsg ( "Test #17.4 : string range" )
    !!$  call vstring_new ( string1 , "my string" )
    !!$  string2 = vstring_range ( string1 , 53 , -12 )
    !!$  call vstring_new ( string3 , "" )
    !!$  equals = vstring_equals ( string2 , string3 )
    !!$  call assert ( equals , "Wrong string_range. (1)" )
    !!$  call vstring_free ( string1 )
    !!$  call vstring_free ( string2 )
    !!$  call vstring_free ( string3 )
    !
    ! test #18
    !
    call logmsg ( "Test #18 : string index" )
    call vstring_new ( string1 , "my string" )
    string2 = vstring_index ( string1 , 4 )
    call vstring_new ( string3 , "s" )
    call assertVstring_vstring ( string2 , string3 , "Wrong string_index. (1)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    call vstring_free ( string3 )
    !
    ! test #27.1
    !
    call logmsg ( "Test #27.1 : vstring_reverse" )
    call vstring_new ( string1 , "fortran" )
    string2 = vstring_reverse ( string1 )
    call vstring_new ( string3 , "nartrof" )
    call assertVstring_vstring ( string2 , string3 , "Wrong vstring_reverse" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    call vstring_free ( string3 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    
    !
    ! test #28.1
    !
    call logmsg ( "Test #28.1 : vstring_random" )
    string1 = vstring_random ( 10 )
    string2 = vstring_random ( 10 )
    call logmsg ( "String #1" )
    call log_vstring ( string1 )
    call logmsg ( "String #2" )
    call log_vstring ( string2 )
    equals = vstring_equals ( string1 , string2 )
    call assert ( .NOT.equals , "Wrong vstring_random" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    
    !
    ! test #29.1
    !
    call logmsg ( "Test #29.1 : vstring_replace" )
    call vstring_new ( string1 , "fortran" )
    string2 = vstring_replace ( string1 , 2 , 3 )
    call vstring_new ( string3 , "ftran" )
    call assertVstring_vstring ( string2 , string3 , "Wrong vstring_replace" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    call vstring_free ( string3 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    
    !
    ! test #29.2
    !
    call logmsg ( "Test #29.2 : vstring_replace with replacement string" )
    call vstring_new ( string1 , "fortran" )
    call vstring_new ( string2 , "java" )
    string3 = vstring_replace ( string1 , 1 , 7 , string2 )
    call vstring_new ( string4 , "java" )
    call assertVstring_vstring ( string3 , string4 , "Wrong vstring_replace" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    call vstring_free ( string3 )
    call vstring_free ( string4 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! Test #29.3
    !
    call logmsg ( "Test #29.3 vstring_replace" )
    call vstring_new ( string1 , "My favorite language is fortran, not Tcl." )
    call vstring_new ( string2 , "Java" )
    string3 = vstring_replace ( string1 , 25 , 31 , string2 )
    call vstring_new ( string4 , "My favorite language is Java, not Tcl." )
    call assertVstring_vstring ( string3 , string4 , "Wrong vstring_replace" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    call vstring_free ( string3 )
    call vstring_free ( string4 )
    !
    ! Test #29.4
    !
    call logmsg ( "Test #29.4 vstring_replace" )
    call vstring_new ( string1 , "My favorite language is fortran, not Tcl." )
    string2 = vstring_replace ( string1 , 1 , 41 )
    call vstring_new ( string3 , "" )
    call assertVstring_vstring ( string2 , string3 , "Wrong vstring_replace" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    call vstring_free ( string3 )
    !
    ! test #30.
    !
    call logmsg ( "Test #30. : vstring_achar" )
    string1 = vstring_achar ( 64 )
    call vstring_new ( string2 , "@" )
    call assertVstring_vstring ( string1 , string1 , "Wrong vstring_achar" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #31.
    !
    call logmsg ( "Test #31. : vstring_char" )
    string1 = vstring_char ( 64 )
    call vstring_new ( string2 , "@" )
    call assertVstring_vstring ( string1 , string1 , "Wrong vstring_char" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! test #34
    !
    call logmsg ( "Test #34 vstring_append" )
    call vstring_new ( string1 , "fortran" )
    call vstring_new ( string2 , " is evil" )
    call vstring_append ( string1 , string2 )
    call vstring_new ( string3 , "fortran is evil" )
    call assertVstring_vstring ( string1 , string3 , "Wrong vstring_append" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    call vstring_free ( string3 )
    !
    ! test #34.b
    !
    call logmsg ( "Test #34.b vstring_append charstring" )
    call vstring_new ( string1 , "fortran" )
    call vstring_append ( string1 , " is evil" )
    call assertVstring_charstring ( string1 , "fortran is evil" , "Wrong vstring_append" )
    call vstring_free ( string1 )
    !
    ! Test #36.1
    !
    call logmsg ( "Test #36.1 vstring_map" )
    call vstring_new ( string1 , "1abcaababcabababc" )
    call vstring_new ( map_old1 ( 1 ) , "abc" )
    call vstring_new ( map_old1 ( 2 ) , "ab" )
    call vstring_new ( map_old1 ( 3 ) , "a" )
    call vstring_new ( map_old1 ( 4 ) , "1" )
    call vstring_new ( map_new1 ( 1 ) , "1" )
    call vstring_new ( map_new1 ( 2 ) , "2" )
    call vstring_new ( map_new1 ( 3 ) , "3" )
    call vstring_new ( map_new1 ( 4 ) , "0" )
    string2 = vstring_map( string1 , map_old1 , map_new1 )
    call vstring_new ( string3 , "01321221" )
    call assertVstring_vstring ( string2 , string3 , "Wrong vstring_map" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    call vstring_free ( string3 )
    do imap = 1 , 4
       call vstring_free ( map_old1 ( imap ) )
       call vstring_free ( map_new1 ( imap ) )
    enddo
    !
    ! Test #36.2
    !
    call logmsg ( "Test #36.2 vstring_map" )
    call vstring_new ( string1 , "1abcaababcabababc" )
    call vstring_new ( map_old1 ( 1 ) , "1" )
    call vstring_new ( map_old1 ( 2 ) , "ab" )
    call vstring_new ( map_old1 ( 3 ) , "a" )
    call vstring_new ( map_old1 ( 4 ) , "abc" )
    call vstring_new ( map_new1 ( 1 ) , "0" )
    call vstring_new ( map_new1 ( 2 ) , "2" )
    call vstring_new ( map_new1 ( 3 ) , "3" )
    call vstring_new ( map_new1 ( 4 ) , "1" )
    string2 = vstring_map( string1 , map_old1 , map_new1 )
    call vstring_new ( string3 , "02c322c222c" )
    call assertVstring_vstring ( string2 , string3 , "Wrong vstring_map" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    call vstring_free ( string3 )
    do imap = 1 , 4
       call vstring_free ( map_old1 ( imap ) )
       call vstring_free ( map_new1 ( imap ) )
    enddo
    !
    ! Test #36.3
    !
    call logmsg ( "Test #36.3 vstring_map with no case" )
    call vstring_new ( string1 , "1abcaababcabababc" )
    call vstring_new ( map_old1 ( 1 ) , "1" )
    call vstring_new ( map_old1 ( 2 ) , "Ab" )
    call vstring_new ( map_old1 ( 3 ) , "a" )
    call vstring_new ( map_old1 ( 4 ) , "aBc" )
    call vstring_new ( map_new1 ( 1 ) , "0" )
    call vstring_new ( map_new1 ( 2 ) , "2" )
    call vstring_new ( map_new1 ( 3 ) , "3" )
    call vstring_new ( map_new1 ( 4 ) , "1" )
    string2 = vstring_map ( string1 , map_old1 , map_new1 , nocase = .true.)
    call vstring_new ( string3 , "02c322c222c" )
    call assertVstring_vstring ( string2 , string3 , "Wrong vstring_map" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    call vstring_free ( string3 )
    do imap = 1 , 4
       call vstring_free ( map_old1 ( imap ) )
       call vstring_free ( map_new1 ( imap ) )
    enddo
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! Test #37.1
    !
    call logmsg ( "Test #37.1 vstring_charindex" )
    call vstring_new ( string1 , "1abcaababcabababc" )
    call vstring_new ( string2 , "c" )
    charindex = vstring_charindex ( string1 , string2 )
    call assert ( charindex == 4 , "Wrong vstring_charindex" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! Test #37.2
    !
    call logmsg ( "Test #37.2 vstring_charindex with back" )
    call vstring_new ( string1 , "1abcaababcabababc" )
    call vstring_new ( string2 , "c" )
    charindex = vstring_charindex ( string1 , string2 , back = .true. )
    call assert ( charindex == 17 , "Wrong vstring_charindex" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
  end subroutine test_m_vstring_all
  !
  ! Test vstring_toupper, vstring_tolower, vstring_totitle
  !
  subroutine test_m_vstring_case ()
    type ( t_vstring ) :: string1
    type ( t_vstring ) :: string2
    type ( t_vstring ) :: string3
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #24.1
    !
    call logmsg ( "Test #24.1 : vstring_toupper with no argument" )
    call vstring_new ( string1 , "toto" )
    string2 = vstring_toupper ( string1 )
    call vstring_new ( string3 , "TOTO" )
    call assertVstring_vstring ( string2 , string3 , "Wrong vstring_toupper" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    call vstring_free ( string3 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #24.2
    !
    call logmsg ( "Test #24.2 : vstring_toupper with 1 arguments" )
    call vstring_new ( string1 , "toto" )
    string2 = vstring_toupper ( string1 , 2)
    call vstring_new ( string3 , "tOTO" )
    call assertVstring_vstring ( string2 , string3 , "Wrong vstring_toupper" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    call vstring_free ( string3 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #24.3
    !
    call logmsg ( "Test #24.3 : vstring_toupper with 2 arguments" )
    call vstring_new ( string1 , "toto" )
    string2 = vstring_toupper ( string1 , 2, 3)
    call vstring_new ( string3 , "tOTo" )
    call assertVstring_vstring ( string2 , string3 , "Wrong vstring_toupper" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    call vstring_free ( string3 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #25.1
    !
    call logmsg ( "Test #25.1 : vstring_tolower with no argument" )
    call vstring_new ( string1 , "TOTO" )
    string2 = vstring_tolower ( string1 )
    call vstring_new ( string3 , "toto" )
    call assertVstring_vstring ( string2 , string3 , "Wrong vstring_tolower" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    call vstring_free ( string3 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #25.2
    !
    call logmsg ( "Test #25.2 : vstring_tolower with 1 arguments" )
    call vstring_new ( string1 , "TOTO" )
    string2 = vstring_tolower ( string1 , 2)
    call vstring_new ( string3 , "Toto" )
    call assertVstring_vstring ( string2 , string3 , "Wrong vstring_tolower" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    call vstring_free ( string3 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #25.3
    !
    call logmsg ( "Test #25.3 : vstring_tolower with 2 arguments" )
    call vstring_new ( string1 , "TOTO" )
    string2 = vstring_tolower ( string1 , 2, 3)
    call vstring_new ( string3 , "TotO" )
    call assertVstring_vstring ( string2 , string3 , "Wrong vstring_tolower" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    call vstring_free ( string3 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #26.1
    !
    call logmsg ( "Test #26.1 : vstring_totitle with no argument" )
    call vstring_new ( string1 , "toto" )
    string2 = vstring_totitle ( string1 )
    call vstring_new ( string3 , "Toto" )
    call assertVstring_vstring ( string2 , string3 , "Wrong vstring_totitle" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    call vstring_free ( string3 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #26.2
    !
    call logmsg ( "Test #26.2 : vstring_totitle with 1 arguments" )
    call vstring_new ( string1 , "tOTO" )
    string2 = vstring_totitle ( string1 , 2)
    call vstring_new ( string3 , "tOto" )
    call assertVstring_vstring ( string2 , string3 , "Wrong vstring_totitle" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    call vstring_free ( string3 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #26.3
    !
    call logmsg ( "Test #26.3 : vstring_totitle with 2 arguments" )
    call vstring_new ( string1 , "TOTO" )
    string2 = vstring_totitle ( string1 , 2, 3)
    call vstring_new ( string3 , "TOtO" )
    call assertVstring_vstring ( string2 , string3 , "Wrong vstring_totitle" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    call vstring_free ( string3 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #26.4
    !
    call logmsg ( "Test #26.4 : vstring_totitle with empty string" )
    call vstring_new ( string1 , "" )
    string2 = vstring_totitle ( string1 )
    call vstring_new ( string3 , "" )
    call assertVstring_vstring ( string2 , string3 , "Wrong vstring_totitle" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    call vstring_free ( string3 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #26.5
    !
    call logmsg ( "Test #26.5 : vstring_totitle with 1 character" )
    call vstring_new ( string1 , "t" )
    string2 = vstring_totitle ( string1 )
    call vstring_new ( string3 , "T" )
    call assertVstring_vstring ( string2 , string3 , "Wrong vstring_totitle" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    call vstring_free ( string3 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    
    !
    ! test #26.6
    !
    call logmsg ( "Test #26.6 : vstring_totitle with first = 1" )
    call vstring_new ( string1 , "t" )
    string2 = vstring_totitle ( string1 , 1 )
    call vstring_new ( string3 , "T" )
    call assertVstring_vstring ( string2 , string3 , "Wrong vstring_totitle" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    call vstring_free ( string3 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
  end subroutine test_m_vstring_case
  !
  ! Test trimming left, right, both
  !
  subroutine test_m_vstring_trim ()
    type ( t_vstring ) :: string1
    type ( t_vstring ) :: string2
    type ( t_vstring ) :: string3
    type ( t_vstring ) :: string4
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #20.1
    !
    call logmsg ( "Test #20.1 : trim left with no arguments" )
    call vstring_new ( string1 , "  my string   " )
    string2 = vstring_trimleft ( string1 )
    call vstring_new ( string3 , "my string   " )
    call assertVstring_vstring ( string2 , string3 ,"Wrong trim. (1)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    call vstring_free ( string3 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #20.2
    !
    call logmsg ( "Test #20.2 : trim left with chars argument" )
    call vstring_new ( string1 , "*/*/*/my string***" )
    call vstring_new ( string2 , "*/" )
    string3 = vstring_trimleft ( string1 , chars = string2 )
    call vstring_new ( string4 , "my string***" )
    call assertVstring_vstring ( string3 , string4 ,"Wrong trim. (1)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    call vstring_free ( string3 )
    call vstring_free ( string4 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #20.2.b
    !
    call logmsg ( "Test #20.2.b : trim left with chars argument" )
    call vstring_new ( string1 , "*/*/*/my string***" )
    string3 = vstring_trimleft ( string1 , chars = "*/" )
    call assertVstring_charstring ( string3 , "my string***" ,"Wrong trim. (1)" )
    call vstring_free ( string1 )
    call vstring_free ( string3 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #20.3
    !
    call logmsg ( "Test #20.3 : trim left with a string allready trimmed" )
    call vstring_new ( string1 , "my string" )
    string2 = vstring_trimleft ( string1 )
    call assertVstring_vstring ( string1 , string2 , "Wrong trim left. (1)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #20.4
    !
    call logmsg ( "Test #20.4 : trim left with blank spaces" )
    call vstring_new ( string1 , "         " )
    string2 = vstring_trimleft ( string1 )
    call vstring_new ( string3 , "" )
    call assertVstring_vstring ( string2 , string3 , "Wrong trim left. (2)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    call vstring_free ( string3 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #22.1
    !
    call logmsg ( "Test #22.1 : trim right with no arguments" )
    call vstring_new ( string1 , "  my string   " )
    string2 = vstring_trimright ( string1 )
    call vstring_new ( string3 , "  my string" )
    call assertVstring_vstring ( string2 , string3 , "Wrong trim left. (1)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    call vstring_free ( string3 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #22.2
    !
    call logmsg ( "Test #22.2 : trim right with chars argument" )
    call vstring_new ( string1 , "*/*/*/my string***" )
    call vstring_new ( string2 , "*/" )
    string3 = vstring_trimright ( string1 , chars = string2 )
    call vstring_new ( string4 , "*/*/*/my string" )
    call assertVstring_vstring ( string3 , string4 , "Wrong trim left. (1)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    call vstring_free ( string3 )
    call vstring_free ( string4 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #22.2.b
    !
    call logmsg ( "Test #22.2.b : trim right with chars argument" )
    call vstring_new ( string1 , "*/*/*/my string***" )
    string3 = vstring_trimright ( string1 , chars = "*/" )
    call assertVstring_charstring ( string3 , "*/*/*/my string" , "Wrong trim left. (1)" )
    call vstring_free ( string1 )
    call vstring_free ( string3 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #22.3
    !
    call logmsg ( "Test #22.3 : trim right with a string allready trimmed" )
    call vstring_new ( string1 , "my string" )
    string2 = vstring_trimright ( string1 )
    call assertVstring_vstring ( string1 , string2 , "Wrong trim left. (1)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #22.4
    !
    call logmsg ( "Test #22.4 : trim right with blank spaces" )
    call vstring_new ( string1 , "         " )
    string2 = vstring_trimright ( string1 )
    call vstring_new ( string3 , "" )
    call assertVstring_vstring ( string2 , string3 , "Wrong trim right. (2)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    call vstring_free ( string3 )

    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #21.1
    !
    call logmsg ( "Test #21.1 : trim with no arguments" )
    call vstring_new ( string1 , "  my string   " )
    string2 = vstring_trim ( string1 )
    call vstring_new ( string3 , "my string" )
    call assertVstring_vstring ( string2 , string3 , "Wrong trim left. (1)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    call vstring_free ( string3 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #21.2
    !
    call logmsg ( "Test #21.2 : trim with chars argument" )
    call vstring_new ( string1 , "*/*/*/my string***" )
    call vstring_new ( string2 , "*/" )
    string3 = vstring_trim ( string1 , chars = string2 )
    call vstring_new ( string4 , "my string" )
    call assertVstring_vstring ( string3 , string4 , "Wrong trim left. (1)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    call vstring_free ( string3 )
    call vstring_free ( string4 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #21.2.b
    !
    call logmsg ( "Test #21.2.b : trim with chars argument" )
    call vstring_new ( string1 , "*/*/*/my string***" )
    string3 = vstring_trim ( string1 , chars = "*/" )
    call assertVstring_charstring ( string3 , "my string" , "Wrong trim left. (1)" )
    call vstring_free ( string1 )
    call vstring_free ( string3 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #21.3
    !
    call logmsg ( "Test #21.3 : trim with a string allready trimmed" )
    call vstring_new ( string1 , "my string" )
    string2 = vstring_trim ( string1 )
    call assertVstring_vstring ( string1 , string2 , "Wrong trim left. (1)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
  end subroutine test_m_vstring_trim
  !
  ! Test vstring_first , vstring_last
  !
  subroutine test_m_vstring_firstlast
    type ( t_vstring ) :: string1
    type ( t_vstring ) :: string2
    integer :: charindex
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #16
    !
    call logmsg ( "Test #16 : first" )
    call vstring_new ( string1 , "my string" )
    call vstring_new ( string2 , "m" )
    charindex = vstring_first ( string1 , string2 )
    call assert ( charindex==1 , "Wrong string_first. (1)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #16.b
    !
    call logmsg ( "Test #16.b : first with character string" )
    call vstring_new ( string1 , "my string" )
    charindex = vstring_first ( string1 , "m" )
    call assert ( charindex==1 , "Wrong string_first. (1)" )
    call vstring_free ( string1 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #16.2
    !
    call logmsg ( "Test #16.2 : first" )
    call vstring_new ( string1 , "my string" )
    call vstring_new ( string2 , "y " )
    charindex = vstring_first ( string1 , string2 )
    call assert ( charindex==2 , "Wrong string_first. (1)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #16.3
    !
    call logmsg ( "Test #16.3 : first" )
    call vstring_new ( string1 , "my string" )
    call vstring_new ( string2 , "aaaaaaaaaaaaaaaaaaaaaa" )
    charindex = vstring_first ( string1 , string2 )
    call assert ( charindex==VSTRING_INDEX_UNKNOWN , "Wrong string_first. (1)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #16.4
    !
    call logmsg ( "Test #16.4 : first when no one matches" )
    call vstring_new ( string1 , "my string" )
    call vstring_new ( string2 , "aa" )
    charindex = vstring_first ( string1 , string2 )
    call assert ( charindex==VSTRING_INDEX_UNKNOWN , "Wrong string_first. (1)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #16.5
    !
    call logmsg ( "Test #16.5 : first with firstIndex" )
    call vstring_new ( string1 , "gmy string" )
    call vstring_new ( string2 , "g")
    charindex = vstring_first ( string1 , string2 , first = 2 )
    call assert ( charindex==10 , "Wrong string_first. (1)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #16.6
    !
    call logmsg ( "Test #16.6 : first with empty string" )
    call vstring_new ( string1 , "gmy string" )
    call vstring_new ( string2 , "")
    charindex = vstring_first ( string1 , string2 )
    call assert ( charindex==0 , "Wrong string_first. (1)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #19.1
    !
    call logmsg ( "Test #19.1 : string last" )
    call vstring_new ( string1 , "my strimg" )
    call vstring_new ( string2 , "m" )
    charindex = vstring_last ( string1 , string2 )
    call assert ( charindex==8 , "Wrong string_last. (1)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #19.1.b
    !
    call logmsg ( "Test #19.1.b : string last" )
    call vstring_new ( string1 , "my strimg" )
    charindex = vstring_last ( string1 , "m" )
    call assert ( charindex==8 , "Wrong string_last. (1)" )
    call vstring_free ( string1 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #19.2
    !
    call logmsg ( "Test #19.2 : string last with lastIndex" )
    call vstring_new ( string1 , "my strimg" )
    call vstring_new ( string2 , "m" )
    charindex = vstring_last ( string1 , string2 , last = 5 )
    call assert ( charindex==1 , "Wrong string_last. (1)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #19.3
    !
    call logmsg ( "Test #19.3 : string last when no one matches" )
    call vstring_new ( string1 , "my strimg" )
    call vstring_new ( string2 , "w" )
    charindex = vstring_last ( string1 , string2 )
    call assert ( charindex==0 , "Wrong string_last. (1)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #19.4
    !
    call logmsg ( "Test #19.4 : last with empty string" )
    call vstring_new ( string1 , "gmy string" )
    call vstring_new ( string2 , "")
    charindex = vstring_last ( string1 , string2 )
    call assert ( charindex==0 , "Wrong string_last. (1)" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
  end subroutine test_m_vstring_firstlast
  !
  ! Test all interfaces to standard fortran
  ! Fail with IVF8
  !
  subroutine test_m_vstring_stdfortran ()
    type ( t_vstring ) :: string1
    type ( t_vstring ) :: string2
    type ( t_vstring ) :: string3
    integer :: charindex
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! Test #40.1
    !
    call logmsg ( "Test #40.1 vstring_adjustl" )
    call vstring_new ( string1 , "       toto  " )
    string2 = vstring_adjustl ( string1 )
    call vstring_new ( string3 , "toto         " )
    call assertVstring_vstring ( string2 , string3 , "Wrong vstring_adjustl" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    call vstring_free ( string3 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! Test #41.1
    !
    call logmsg ( "Test #41.1 vstring_adjustr" )
    call vstring_new ( string1 , "       toto  " )
    string2 = vstring_adjustr ( string1 )
    call vstring_new ( string3 , "         toto" )
    call assertVstring_vstring ( string2 , string3 , "Wrong vstring_adjustr" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    call vstring_free ( string3 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! Test #38.1
    !
    call logmsg ( "Test #38.1 vstring_scan" )
    call vstring_new ( string1 , "1abcaababcabababc" )
    call vstring_new ( string2 , "c" )
    charindex = vstring_scan ( string1 , string2 )
    call assert ( charindex == 4 , "Wrong vstring_scan" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! Test #38.2
    !
    call logmsg ( "Test #38.2 vstring_scan with back" )
    call vstring_new ( string1 , "1abcaababcabababc" )
    call vstring_new ( string2 , "c" )
    charindex = vstring_scan ( string1 , string2 , back = .true. )
    call assert ( charindex == 17 , "Wrong vstring_scan" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #32.
    !
    call logmsg ( "Test #32. : vstring_iachar" )
    call vstring_new ( string1 , "@" )
    charindex = vstring_iachar ( string1 )
    call assert ( charindex==64 , "Wrong vstring_iachar" )
    call vstring_free ( string1 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #33.
    !
    call logmsg ( "Test #33. : vstring_ichar" )
    call vstring_new ( string1 , "@" )
    charindex = vstring_ichar ( string1 )
    call assert ( charindex==64 , "Wrong vstring_ichar" )
    call vstring_free ( string1 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! Test #39.1
    !
    call logmsg ( "Test #39.1 vstring_verify" )
    call vstring_new ( string1 , "1abcaababcabababc" )
    call vstring_new ( string2 , "abc1" )
    charindex = vstring_verify ( string1 , string2 )
    call assert ( charindex == 0 , "Wrong vstring_verify" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! Test #39.2
    !
    call logmsg ( "Test #39.2 vstring_verify with back" )
    call vstring_new ( string1 , "1abcaababcabababc" )
    call vstring_new ( string2 , "c1b" )
    charindex = vstring_verify ( string1 , string2 , back = .true. )
    call assert ( charindex == 15 , "Wrong vstring_verify" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
  end subroutine test_m_vstring_stdfortran
  !
  ! Test vstring_is
  !
  subroutine test_m_vstring_is ()
    type ( t_vstring ) :: string1
    logical :: stringis
    integer :: charindex
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! String is digit
    !
    call logmsg ( "Test digit true" )
    call vstring_new ( string1 , "2008" )
    stringis = vstring_is ( string1 , "digit" )
    call assert ( stringis , "Wrong vstring_is digit" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test digit false" )
    call vstring_new ( string1 , "2008 " )
    stringis = vstring_is ( string1 , "digit" )
    call assert ( .NOT. stringis , "Wrong vstring_is digit" )
    call vstring_free ( string1 )
    !
    ! String is integer
    !
    call logmsg ( "Test integer true" )
    call vstring_new ( string1 , "2008 " )
    stringis = vstring_is ( string1 , "integer" )
    call assert ( stringis , "Wrong vstring_is integer" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test integer false" )
    call vstring_new ( string1 , "2.5" )
    stringis = vstring_is ( string1 , "integer" )
    call assert ( .NOT.stringis , "Wrong vstring_is integer" )
    call vstring_free ( string1 )
    !
    ! String is alpha
    !
    call logmsg ( "Test alpha true" )
    call vstring_new ( string1 , "aB" )
    stringis = vstring_is ( string1 , "alpha" )
    call assert ( stringis , "Wrong vstring_is alpha" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test alpha false" )
    call vstring_new ( string1 , "aB." )
    stringis = vstring_is ( string1 , "alpha" )
    call assert ( .NOT.stringis , "Wrong vstring_is alpha" )
    call vstring_free ( string1 )
    !
    ! String is alnum
    !
    call logmsg ( "Test alnum true" )
    call vstring_new ( string1 , "aB1" )
    stringis = vstring_is ( string1 , "alnum" )
    call assert ( stringis , "Wrong vstring_is alnum" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test alnum false" )
    call vstring_new ( string1 , "aB1." )
    stringis = vstring_is ( string1 , "alnum" )
    call assert ( .NOT.stringis , "Wrong vstring_is alnum" )
    call vstring_free ( string1 )
    !
    ! String is logical
    !
    call logmsg ( "Test logical true" )
    call vstring_new ( string1 , ".true." )
    stringis = vstring_is ( string1 , "logical" )
    call assert ( stringis , "Wrong vstring_is alnum" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test logical true" )
    call vstring_new ( string1 , ".false." )
    stringis = vstring_is ( string1 , "logical" )
    call assert ( stringis , "Wrong vstring_is alnum" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test logical true" )
    call vstring_new ( string1 , "T" )
    stringis = vstring_is ( string1 , "logical" )
    call assert ( stringis , "Wrong vstring_is alnum" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test logical true" )
    call vstring_new ( string1 , "F" )
    stringis = vstring_is ( string1 , "logical" )
    call assert ( stringis , "Wrong vstring_is alnum" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test logical false" )
    call vstring_new ( string1 , "aB1." )
    stringis = vstring_is ( string1 , "logical" )
    call assert ( .NOT.stringis , "Wrong vstring_is alnum" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test logical false" )
    call vstring_new ( string1 , "not a logical at all" )
    stringis = vstring_is ( string1 , "logical" )
    call assert ( .NOT.stringis , "Wrong vstring_is alnum" )
    call vstring_free ( string1 )
    !
    ! String is real
    ! Fails with IFV8
    !
#ifndef _IVF8
    call logmsg ( "Test real true" )
    call vstring_new ( string1 , "1.5" )
    stringis = vstring_is ( string1 , "real" )
    call assert ( stringis , "Wrong vstring_is real" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test real true" )
    call vstring_new ( string1 , "1.5" )
    stringis = vstring_is ( string1 , "real" )
    call assert ( stringis , "Wrong vstring_is real" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test real false" )
    call vstring_new ( string1 , "A" )
    stringis = vstring_is ( string1 , "real" )
    call assert ( .NOT.stringis , "Wrong vstring_is real" )
    call vstring_free ( string1 )
#endif
    !
    ! String is true
    !
    call logmsg ( "Test true true" )
    call vstring_new ( string1 , "T" )
    stringis = vstring_is ( string1 , "true" )
    call assert ( stringis , "Wrong vstring_is true" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test true false" )
    call vstring_new ( string1 , "F" )
    stringis = vstring_is ( string1 , "true" )
    call assert ( .NOT.stringis , "Wrong vstring_is true" )
    call vstring_free ( string1 )
    !
    ! String is false
    !
    call logmsg ( "Test false true" )
    call vstring_new ( string1 , "F" )
    stringis = vstring_is ( string1 , "false" )
    call assert ( stringis , "Wrong vstring_is false" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test false false" )
    call vstring_new ( string1 , "T" )
    stringis = vstring_is ( string1 , "false" )
    call assert ( .NOT.stringis , "Wrong vstring_is false" )
    call vstring_free ( string1 )
    !
    ! String is upper
    !
    call logmsg ( "Test upper true" )
    call vstring_new ( string1 , "F" )
    stringis = vstring_is ( string1 , "upper" )
    call assert ( stringis , "Wrong vstring_is upper" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test upper false" )
    call vstring_new ( string1 , "f" )
    stringis = vstring_is ( string1 , "upper" )
    call assert ( .NOT.stringis , "Wrong vstring_is upper" )
    call vstring_free ( string1 )
    !
    ! String is lower
    !
    call logmsg ( "Test lower true" )
    call vstring_new ( string1 , "f" )
    stringis = vstring_is ( string1 , "lower" )
    call assert ( stringis , "Wrong vstring_is lower" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test lower false" )
    call vstring_new ( string1 , "F" )
    stringis = vstring_is ( string1 , "lower" )
    call assert ( .NOT.stringis , "Wrong vstring_is lower" )
    call vstring_free ( string1 )
    !
    ! String is space
    !
    call logmsg ( "Test space true" )
    call vstring_new ( string1 , " " )
    stringis = vstring_is ( string1 , "space" )
    call assert ( stringis , "Wrong vstring_is space" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test space false" )
    call vstring_new ( string1 , "F" )
    stringis = vstring_is ( string1 , "space" )
    call assert ( .NOT.stringis , "Wrong vstring_is space" )
    call vstring_free ( string1 )
    !
    ! String is punct
    !
    call logmsg ( "Test punct true" )
    call vstring_new ( string1 , "," )
    stringis = vstring_is ( string1 , "punct" )
    call assert ( stringis , "Wrong vstring_is punct" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test punct false" )
    call vstring_new ( string1 , "F" )
    stringis = vstring_is ( string1 , "punct" )
    call assert ( .NOT.stringis , "Wrong vstring_is punct" )
    call vstring_free ( string1 )
    !
    ! String is xdigit
    !
    call logmsg ( "Test xdigit true" )
    call vstring_new ( string1 , "0123456789abcdefABCDEF" )
    stringis = vstring_is ( string1 , "xdigit" )
    call assert ( stringis , "Wrong vstring_is xdigit" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test xdigit false" )
    call vstring_new ( string1 , "Z" )
    stringis = vstring_is ( string1 , "xdigit" )
    call assert ( .NOT.stringis , "Wrong vstring_is xdigit" )
    call vstring_free ( string1 )
    !
    ! String is ascii
    !
    call logmsg ( "Test ascii true" )
    call vstring_new ( string1 , "0123456789abcdefABCDEF" )
    stringis = vstring_is ( string1 , "ascii" )
    call assert ( stringis , "Wrong vstring_is ascii" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test ascii false" )
    string1 = vstring_char ( 129 )
    stringis = vstring_is ( string1 , "ascii" )
    call assert ( .NOT.stringis , "Wrong vstring_is ascii" )
    call vstring_free ( string1 )
    !
    ! String is control
    !
    call logmsg ( "Test control true" )
    string1 = vstring_char ( 12 )
    stringis = vstring_is ( string1 , "control" )
    call assert ( stringis , "Wrong vstring_is control" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test control false" )
    string1 = vstring_char ( 52 )
    stringis = vstring_is ( string1 , "control" )
    call assert ( .NOT.stringis , "Wrong vstring_is control" )
    call vstring_free ( string1 )
    !
    ! String is print
    !
    call logmsg ( "Test print true" )
    call vstring_new ( string1 , "A" )
    stringis = vstring_is ( string1 , "print" )
    call assert ( stringis , "Wrong vstring_is print" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test print true" )
    call vstring_new ( string1 , " " )
    stringis = vstring_is ( string1 , "print" )
    call assert ( stringis , "Wrong vstring_is print" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test print false" )
    string1 = vstring_char ( 10 )
    stringis = vstring_is ( string1 , "print" )
    call assert ( .NOT.stringis , "Wrong vstring_is print" )
    call vstring_free ( string1 )
    !
    ! String is graph
    !
    call logmsg ( "Test graph true" )
    call vstring_new ( string1 , "A" )
    stringis = vstring_is ( string1 , "graph" )
    call assert ( stringis , "Wrong vstring_is graph" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test graph false" )
    call vstring_new ( string1 , " " )
    stringis = vstring_is ( string1 , "graph" )
    call assert ( .NOT.stringis , "Wrong vstring_is graph" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test graph false" )
    string1 = vstring_char ( 10 )
    stringis = vstring_is ( string1 , "graph" )
    call assert ( .NOT.stringis , "Wrong vstring_is graph" )
    call vstring_free ( string1 )
    !
    ! String is wordchar
    !
    call logmsg ( "Test wordchar true" )
    call vstring_new ( string1 , "A" )
    stringis = vstring_is ( string1 , "wordchar" )
    call assert ( stringis , "Wrong vstring_is wordchar" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test wordchar false" )
    string1 = vstring_char ( 10 )
    stringis = vstring_is ( string1 , "wordchar" )
    call assert ( .NOT.stringis , "Wrong vstring_is wordchar" )
    call vstring_free ( string1 )
    !
    ! String is wordchar strict
    !
    call logmsg ( "Test wordchar true" )
    call vstring_new ( string1 , "" )
    stringis = vstring_is ( string1 , "wordchar" )
    call assert ( stringis , "Wrong vstring_is wordchar" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test wordchar false" )
    call vstring_new ( string1 , "" )
    stringis = vstring_is ( string1 , "wordchar" , strict = .true. )
    call assert ( .NOT.stringis , "Wrong vstring_is wordchar" )
    call vstring_free ( string1 )
    !
    ! Get failing index for all classes
    !
    call logmsg ( "Test wordchar failing index" )
    string1 = vstring_char ( 10 )
    stringis = vstring_is ( string1 , "wordchar" , .false. , charindex )
    call assert ( .NOT.stringis , "Wrong vstring_is wordchar failing index" )
    call assert ( charindex == 1 , "Wrong vstring_is wordchar failing index" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test digit failing index" )
    call vstring_new ( string1 , "2008 " )
    stringis = vstring_is ( string1 , "digit" , .false. , charindex )
    call assert ( .NOT. stringis , "Wrong vstring_is digit" )
    call assert ( charindex == 5 , "Wrong vstring_is digit failing index" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test integer failing index" )
    call vstring_new ( string1 , "2.5" )
    stringis = vstring_is ( string1 , "integer" , .false. , charindex )
    call assert ( .NOT.stringis , "Wrong vstring_is integer" )
    call assert ( charindex == 2 , "Wrong vstring_is integer failing index" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test alpha failing index" )
    call vstring_new ( string1 , "aB." )
    stringis = vstring_is ( string1 , "alpha" , .false. , charindex )
    call assert ( .NOT.stringis , "Wrong vstring_is alpha" )
    call assert ( charindex == 3 , "Wrong vstring_is alpha failing index" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test logical failing index" )
    call vstring_new ( string1 , "aB1." )
    stringis = vstring_is ( string1 , "logical" , .false. , charindex )
    call assert ( .NOT.stringis , "Wrong vstring_is alnum" )
    call assert ( charindex == 1 , "Wrong vstring_is alpha failing index" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test true failing index" )
    call vstring_new ( string1 , "F" )
    stringis = vstring_is ( string1 , "true" , .false. , charindex )
    call assert ( .NOT.stringis , "Wrong vstring_is true" )
    call assert ( charindex == 1 , "Wrong vstring_is alpha failing index" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test false failing index" )
    call vstring_new ( string1 , "T" )
    stringis = vstring_is ( string1 , "false" , .false. , charindex )
    call assert ( .NOT.stringis , "Wrong vstring_is false" )
    call assert ( charindex == 1 , "Wrong vstring_is false failing index" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test upper failing index" )
    call vstring_new ( string1 , "f" )
    stringis = vstring_is ( string1 , "upper" , .false. , charindex )
    call assert ( .NOT.stringis , "Wrong vstring_is upper" )
    call assert ( charindex == 1 , "Wrong vstring_is upper failing index" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test lower failing index" )
    call vstring_new ( string1 , "F" )
    stringis = vstring_is ( string1 , "lower" , .false. , charindex )
    call assert ( .NOT.stringis , "Wrong vstring_is lower" )
    call assert ( charindex == 1 , "Wrong vstring_is lower failing index" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test space failing index" )
    call vstring_new ( string1 , "F" )
    stringis = vstring_is ( string1 , "space" , .false. , charindex )
    call assert ( .NOT.stringis , "Wrong vstring_is space" )
    call assert ( charindex == 1 , "Wrong vstring_is space failing index" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test punct failing index" )
    call vstring_new ( string1 , "F" )
    stringis = vstring_is ( string1 , "punct" , .false. , charindex )
    call assert ( .NOT.stringis , "Wrong vstring_is punct" )
    call assert ( charindex == 1 , "Wrong vstring_is punct failing index" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test xdigit failing index" )
    call vstring_new ( string1 , "Z" )
    stringis = vstring_is ( string1 , "xdigit" , .false. , charindex )
    call assert ( .NOT.stringis , "Wrong vstring_is xdigit" )
    call assert ( charindex == 1 , "Wrong vstring_is xdigit failing index" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test ascii failing index" )
    string1 = vstring_char ( 129 )
    stringis = vstring_is ( string1 , "ascii" , .false. , charindex )
    call assert ( .NOT.stringis , "Wrong vstring_is ascii" )
    call assert ( charindex == 1 , "Wrong vstring_is ascii failing index" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test control failing index" )
    string1 = vstring_char ( 52 )
    stringis = vstring_is ( string1 , "control" , .false. , charindex )
    call assert ( .NOT.stringis , "Wrong vstring_is control" )
    call assert ( charindex == 1 , "Wrong vstring_is control failing index" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test print failing index" )
    string1 = vstring_char ( 10 )
    stringis = vstring_is ( string1 , "print" , .false. , charindex )
    call assert ( .NOT.stringis , "Wrong vstring_is print" )
    call assert ( charindex == 1 , "Wrong vstring_is print failing index" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test graph failing index" )
    string1 = vstring_char ( 10 )
    stringis = vstring_is ( string1 , "graph" , .false. , charindex )
    call assert ( .NOT.stringis , "Wrong vstring_is graph" )
    call assert ( charindex == 1 , "Wrong vstring_is graph failing index" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test wordchar failing index" )
    string1 = vstring_char ( 10 )
    stringis = vstring_is ( string1 , "wordchar" , .false. , charindex )
    call assert ( .NOT.stringis , "Wrong vstring_is wordchar" )
    call assert ( charindex == 1 , "Wrong vstring_is wordchar failing index" )
    call vstring_free ( string1 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
  end subroutine test_m_vstring_is
  !
  ! test_m_vstring_cast --
  ! Test converting a string into a basic type.
  !
  subroutine test_m_vstring_cast ()
    type ( t_vstring ) :: string1
    type ( t_vstring ) :: string2
    integer :: valueinteger
    real :: valuereal
    double precision :: valuedp
    character ( len = 10 ) :: char_string1
    character ( len = 4 ) :: char_string2
    character ( len = 10 ) :: char_string3
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    call logmsg ( "Test vstring_cast to integer" )
    call vstring_new ( string1 , "2008" )
    call vstring_cast ( string1 , valueinteger )
    call assert ( valueinteger == 2008 , "Wrong vstring_cast to integer" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test vstring_cast to real" )
    call vstring_new ( string1 , "2.5" )
    call vstring_cast ( string1 , valuereal )
    call assert ( abs( valuereal - 2.5) < 1.d-5 , "Wrong vstring_cast to real" )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test vstring_cast to double precision" )
    call vstring_new ( string1 , "2.5d0" )
    call vstring_cast ( string1 , valuedp )
    call assert ( abs( valuedp - 2.5d0) < 1.d-8 , "Wrong vstring_cast to double precision" )
    call vstring_free ( string1 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()
    !
    ! test #23.1
    !
    call logmsg ( "Test #23.1 : vstring_cast with fixed length" )
    call vstring_new ( string1 , "toto" )
    call vstring_cast ( string1 , len ( char_string1 ) , char_string1 )
    call vstring_new ( string2 , char_string1 )
    call assertVstring_charstring ( string2 , "toto      " , "Wrong vstring_cast" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! test #23.2
    !
    call logmsg ( "Test #23.2 : vstring_cast with auto length" )
    call vstring_new ( string1 , "toto" )
    call vstring_cast ( string1 , char_string2 )
    call vstring_new ( string2 , char_string2 )
    call assertVstring_charstring ( string2 , "toto" , "Wrong vstring_cast" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! test #23.3
    !
    call logmsg ( "Test #23.3 : vstring_cast with auto length and needed blanks" )
    call vstring_new ( string1 , "toto" )
    char_string3 = "@@@@@@@@@@"
    call vstring_cast ( string1 , char_string3 )
    call vstring_new ( string2 , char_string3 )
    call assertVstring_charstring ( string2 , "toto      " , "Wrong vstring_cast" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! test #23.4
    !
    call logmsg ( "Test #23.4 : vstring_cast with auto length and truncation" )
    call vstring_new ( string1 , "toto      @" )
    call vstring_cast ( string1 , char_string3 )
    call vstring_new ( string2 , char_string3 )
    call assertVstring_charstring ( string2 , "toto      " , "Wrong vstring_cast" )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    ! Check the number of strings references
    !
    call string_reference_check ()

  end subroutine test_m_vstring_cast

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
end program test_m_vstring


