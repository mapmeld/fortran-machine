!
! This program is a testing component for the module m_vstrlist.
!
#ifdef _IVF8
#ifdef _VSTRINGLIST_ALLOCATABLE
#define _DO_NOT_TEST
#endif
#endif
program test_m_vstringlist
  use m_vstring, only : &
       t_vstring ,&
       vstring_new , &
       vstring_free , &
       vstring_equals , &
       vstring_cast , &
       vstring_length , &
       vstring_compare
  use m_vstringlist , only : &
       t_vstringlist ,&
       vstrlist_new, &
       vstrlist_free, &
       vstrlist_length , &
       vstrlist_reference_get , &
       vstrlist_index , &
       vstrlist_concat , &
       vstrlist_append , &
       vstrlist_insert , &
       vstrlist_range , &
       vstringlist_set_stoponerror , &
       vstrlist_set, &
       vstrlist_split , &
       vstrlist_join , &
       vstrlist_search ,&
       vstrlist_lsearch , &
       vstrlist_sort
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
  !   Main subroutine for unit tests.
  !
  subroutine test_main ()
    implicit none
    call log_startup ( "test_m_vstringlist.log" )
    call assert_startup ( )
    call vstringlist_set_stoponerror ( .false. )
    !
    ! Test #0
    ! Check the number of lists references
    !
    call strlist_reference_check ()
    !
    ! Test vstrlist_new
    !
    call test_m_vstringlist_new ()
    !
    ! Test everything else...
    !
    call test_m_vstringlist_all ()
    !
    ! Test string splitting : joining
    !
    call test_m_vstringlist_split ()
    !
    ! Check the number of lists references
    !
    call strlist_reference_check ()
    !
    ! Test string search
    !
    call test_mstringlist_search ()
    !
    ! Check the number of lists references
    !
    call strlist_reference_check ()
    !
    ! Test string sort
    !
    call test_mstringlist_sort ()
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
  subroutine strlist_reference_check ()
    implicit none
    integer :: vstrlist_reference
    character (len=200) :: msg
    vstrlist_reference = vstrlist_reference_get ()
    write ( msg , * ) "Number of strings :", vstrlist_reference
    call logmsg (msg)
    if (vstrlist_reference /= 0) then
       continue
    endif
    call assert ( vstrlist_reference == 0, "Wrong number of lists references.")
  end subroutine strlist_reference_check
  !
  ! Test the vstrlist_new command
  !
  subroutine test_m_vstringlist_new ()
    implicit none
    type ( t_vstringlist ) :: list1
    type ( t_vstringlist ) :: list2
    type ( t_vstring ) , dimension ( 1 : 3 ) :: array1
    type ( t_vstring ) :: string1
    integer :: length
    !
    ! Check the number of lists references
    !
    call strlist_reference_check ()
    !
    call logmsg ( "Test : new from emty" )
    call vstrlist_new ( list1 )
    length = vstrlist_length ( list1 )
    call assert ( length == 0, "Wrong number of elements.")
    call vstrlist_free ( list1 )
    !
    ! Check the number of lists references
    !
    call strlist_reference_check ()
    !
    call logmsg ( "Test : new from string" )
    call vstring_new ( string1 , "foo" )
    call vstrlist_new ( list1 , string1 )
    length = vstrlist_length ( list1 )
    call assert ( length == 1, "Wrong number of elements.")
    call vstrlist_free ( list1 )
    call vstring_free ( string1 )
    !
    ! Check the number of lists references
    !
    call strlist_reference_check ()
    !
    call logmsg ( "Test : new from array" )
    call vstring_new ( array1 ( 1 ) , "foo" )
    call vstring_new ( array1 ( 2 ) , "foo" )
    call vstring_new ( array1 ( 3 ) , "foo" )
    call vstrlist_new ( list1 , array1 )
    length = vstrlist_length ( list1 )
    call assert ( length == 3, "Wrong number of elements.")
    call vstrlist_free ( list1 )
    call vstring_free ( array1 ( 1 ) )
    call vstring_free ( array1 ( 2 ) )
    call vstring_free ( array1 ( 3 ) )
    !
    call logmsg ( "Test : new from list" )
    call vstring_new ( array1 ( 1 ) , "foo" )
    call vstring_new ( array1 ( 2 ) , "foo" )
    call vstring_new ( array1 ( 3 ) , "foo" )
    call vstrlist_new ( list1 , array1 )
    call vstrlist_new ( list2 , list1 )
    length = vstrlist_length ( list2 )
    call assert ( length == 3, "Wrong number of elements.")
    call vstrlist_free ( list1 )
    call vstrlist_free ( list2 )
    call vstring_free ( array1 ( 1 ) )
    call vstring_free ( array1 ( 2 ) )
    call vstring_free ( array1 ( 3 ) )
    !
    call logmsg ( "Test : new from integer" )
    call vstrlist_new ( list1 , 3 )
    length = vstrlist_length ( list1 )
    call assert ( length == 3, "Wrong number of elements.")
    call vstrlist_free ( list1 )
    !
    call logmsg ( "Test : new from integer" )
    call vstrlist_new ( list1 , 1 )
    length = vstrlist_length ( list1 )
    call assert ( length == 1, "Wrong number of elements.")
    call vstrlist_free ( list1 )
    !
    ! Check the number of lists references
    !
    call strlist_reference_check ()
  end subroutine test_m_vstringlist_new
  !
  ! Test everything else
  !
  subroutine test_m_vstringlist_all ()
    type ( t_vstringlist ) :: list1
    type ( t_vstring ) :: string1
    type ( t_vstring ) :: string2
    type ( t_vstring ) , dimension ( 1 : 3 ) :: array1
    type ( t_vstring ) , dimension ( 1 : 2 ) :: array2
    type ( t_vstringlist ) :: list2
    type ( t_vstringlist ) :: list3
    integer :: length
    integer :: i
    !
    ! Check the number of lists references
    !
    call strlist_reference_check ()
    !
    ! Create a new list an use it intensively.
    !
    call vstring_new ( array1 ( 1 ) , "fortran" )
    call vstring_new ( array1 ( 2 ) , "tcl" )
    call vstring_new ( array1 ( 3 ) , "algol" )
    call vstrlist_new ( list1 , array1 )
    call vstring_free ( array1 ( 1 ) )
    call vstring_free ( array1 ( 2 ) )
    call vstring_free ( array1 ( 3 ) )
    !
    call logmsg ( "Test : vstrlist_index" )
    string1 = vstrlist_index ( list1 , 1 )
    call vstring_new ( string2 , "fortran" )
    call assertVstring_vstring ( string1 , string2 , "Wrong string index 1." )
    call vstring_free ( string1 )
    call vstring_free ( string2 )
    !
    call logmsg ( "Test : vstrlist_concat a string" )
    call vstring_new ( string1 , "java" )
    list2 = vstrlist_concat ( list1 , string1 )
    call vstring_free ( string1 )
    length = vstrlist_length ( list2 )
    call assert ( length == 4, "Wrong number of elements.")
    ! Check #1
    string1 = vstrlist_index ( list2 , 1 )
    call assertVstring_charstring ( string1 , "fortran" , "Wrong string index 1." )
    call vstring_free ( string1 )
    ! Check #2
    string1 = vstrlist_index ( list2 , 2 )
    call assertVstring_charstring ( string1 , "tcl" , "Wrong string index 2." )
    call vstring_free ( string1 )
    ! Check #3
    string1 = vstrlist_index ( list2 , 3 )
    call assertVstring_charstring ( string1 , "algol" , "Wrong string index 3." )
    call vstring_free ( string1 )
    ! Check #4
    string1 = vstrlist_index ( list2 , 4 )
    call assertVstring_charstring ( string1 , "java" , "Wrong string index 4." )
    call vstring_free ( string1 )
    ! Clean-up
    call vstrlist_free ( list2 )
    !
    call logmsg ( "Test : vstrlist_concat a list" )
    call vstring_new ( array2 ( 1 ) , "java" )
    call vstring_new ( array2 ( 2 ) , "C++" )
    call vstrlist_new ( list2 , array2 )
    list3 = vstrlist_concat ( list1 , list2 )
    call vstrlist_free ( list2 )
    call vstring_free ( array2 ( 1 ) )
    call vstring_free ( array2 ( 2 ) )
    length = vstrlist_length ( list3 )
    call assert ( length == 5, "Wrong number of elements.")
    ! Check #1
    string1 = vstrlist_index ( list3 , 1 )
    call assertVstring_charstring ( string1 , "fortran" , "Wrong string index 1." )
    call vstring_free ( string1 )
    ! Check #2
    string1 = vstrlist_index ( list3 , 2 )
    call assertVstring_charstring ( string1 , "tcl" , "Wrong string index 2." )
    call vstring_free ( string1 )
    ! Check #3
    string1 = vstrlist_index ( list3 , 3 )
    call assertVstring_charstring ( string1 , "algol" , "Wrong string index 3." )
    call vstring_free ( string1 )
    ! Check #4
    string1 = vstrlist_index ( list3 , 4 )
    call assertVstring_charstring ( string1 , "java" , "Wrong string index 4." )
    call vstring_free ( string1 )
    ! Check #5
    string1 = vstrlist_index ( list3 , 5 )
    call assertVstring_charstring ( string1 , "C++" , "Wrong string index 5." )
    call vstring_free ( string1 )
    ! Clean-up
    call vstrlist_free ( list3 )
    !
    call logmsg ( "Test : vstrlist_append a string" )
    call vstring_new ( string1 , "java" )
    call vstrlist_append ( list1 , string1 )
    call vstring_free ( string1 )
    length = vstrlist_length ( list1 )
    call assert ( length == 4, "Wrong number of elements.")
    ! Check #1
    string1 = vstrlist_index ( list1 , 1 )
    call assertVstring_charstring ( string1 , "fortran" , "Wrong string index 1." )
    call vstring_free ( string1 )
    ! Check #2
    string1 = vstrlist_index ( list1 , 2 )
    call assertVstring_charstring ( string1 , "tcl" , "Wrong string index 2." )
    call vstring_free ( string1 )
    ! Check #3
    string1 = vstrlist_index ( list1 , 3 )
    call assertVstring_charstring ( string1 , "algol" , "Wrong string index 3." )
    call vstring_free ( string1 )
    ! Check #4
    string1 = vstrlist_index ( list1 , 4 )
    call assertVstring_charstring ( string1 , "java" , "Wrong string index 4." )
    call vstring_free ( string1 )
    !
    call logmsg ( "Test : vstrlist_append a list" )
    call vstring_new ( array2 ( 1 ) , "C++" )
    call vstring_new ( array2 ( 2 ) , "C#" )
    call vstrlist_new ( list2 , array2 )
    call vstring_free ( array2 ( 1 ) )
    call vstring_free ( array2 ( 2 ) )
    call vstrlist_append ( list1 , list2 )
    call vstrlist_free ( list2 )
    length = vstrlist_length ( list1 )
    call assert ( length == 6, "Wrong number of elements.")
    ! Check #1
    string1 = vstrlist_index ( list1 , 1 )
    call assertVstring_charstring ( string1 , "fortran" , "Wrong string index 1." )
    call vstring_free ( string1 )
    ! Check #2
    string1 = vstrlist_index ( list1 , 2 )
    call assertVstring_charstring ( string1 , "tcl" , "Wrong string index 2." )
    call vstring_free ( string1 )
    ! Check #3
    string1 = vstrlist_index ( list1 , 3 )
    call assertVstring_charstring ( string1 , "algol" , "Wrong string index 3." )
    call vstring_free ( string1 )
    ! Check #4
    string1 = vstrlist_index ( list1 , 4 )
    call assertVstring_charstring ( string1 , "java" , "Wrong string index 4." )
    call vstring_free ( string1 )
    ! Check #5
    string1 = vstrlist_index ( list1 , 5 )
    call assertVstring_charstring ( string1 , "C++" , "Wrong string index 5." )
    call vstring_free ( string1 )
    ! Check #6
    string1 = vstrlist_index ( list1 , 6 )
    call assertVstring_charstring ( string1 , "C#" , "Wrong string index 6." )
    call vstring_free ( string1 )
    !
    ! Free the list
    !
    call vstrlist_free ( list1 )
    !
    call logmsg ( "Test : vstrlist_append a list several times" )
    call vstrlist_new ( list1 )
    call vstrlist_append ( list1 , "fortran" )
    call vstrlist_append ( list1 , "C++" )
    do i= 1, 3
       call vstrlist_new ( list2 )
       call vstrlist_append ( list2 , "fortran" )
       call vstrlist_append ( list2 , "C++" )
       call vstrlist_append ( list1 , list2 )
       call vstrlist_free ( list2 )
    end do
    length = vstrlist_length ( list1 )
    call assert ( length == 8, "Wrong number of elements.")
    ! Check that the content is fine
    call assertVstring_charstringindex ( list1 , 1 , "fortran" , "Wrong vstrlist_append with list" )
    call assertVstring_charstringindex ( list1 , 2 , "C++" , "Wrong vstrlist_append with list" )
    call assertVstring_charstringindex ( list1 , 3 , "fortran" , "Wrong vstrlist_append with list" )
    call assertVstring_charstringindex ( list1 , 4 , "C++" , "Wrong vstrlist_append with list" )
    call assertVstring_charstringindex ( list1 , 5 , "fortran" , "Wrong vstrlist_append with list" )
    call assertVstring_charstringindex ( list1 , 6 , "C++" , "Wrong vstrlist_append with list" )
    call assertVstring_charstringindex ( list1 , 7 , "fortran" , "Wrong vstrlist_append with list" )
    call assertVstring_charstringindex ( list1 , 8 , "C++" , "Wrong vstrlist_append with list" )
    call vstrlist_free ( list1 )
    !
    ! Check the number of lists references
    !
    call strlist_reference_check ()
    !
#ifndef _DO_NOT_TEST
    call logmsg ( "Test : vstrlist_concat a string" )
    call vstrlist_new ( list1 )
    list2 = vstrlist_concat ( list1 , "java" )
    call vstrlist_free ( list1 )
    length = vstrlist_length ( list2 )
    call assert ( length == 1, "Wrong number of elements.")
    ! Check #1
    string1 = vstrlist_index ( list2 , 1 )
    call assertVstring_charstring ( string1 , "java" , "Wrong string index 1." )
    call vstring_free ( string1 )
    ! Clean-up
    call vstrlist_free ( list2 )
#endif
    !
    ! Check the number of lists references
    !
    call strlist_reference_check ()
    !
#ifndef _DO_NOT_TEST
    call logmsg ( "Test : vstrlist_append a charstring" )
    call vstrlist_new ( list2 )
    call vstrlist_append ( list2 , "java" )
    length = vstrlist_length ( list2 )
    call assert ( length == 1, "Wrong number of elements.")
    ! Check #1
    string1 = vstrlist_index ( list2 , 1 )
    call assertVstring_charstring ( string1 , "java" , "Wrong string index 1." )
    call vstring_free ( string1 )
    call vstrlist_free ( list2 )
#endif
    !
    ! Check the number of lists references
    !
    call strlist_reference_check ()
    !
    call logmsg ( "Test : vstrlist_insert a charstring" )
    call vstrlist_new ( list1 , "fortran" )
    list2 = vstrlist_insert ( list1 , 1 , "java" )
    call vstrlist_free ( list1 )
    length = vstrlist_length ( list2 )
    call assert ( length == 2 , "Wrong number of elements.")
    ! Check #1
    string1 = vstrlist_index ( list2 , 1 )
    call assertVstring_charstring ( string1 , "java" , "Wrong string index 1." )
    call vstring_free ( string1 )
    ! Check #2
    string1 = vstrlist_index ( list2 , 2 )
    call assertVstring_charstring ( string1 , "fortran" , "Wrong string index 2." )
    call vstring_free ( string1 )
    ! Clean-up
    call vstrlist_free ( list2 )
    !
    !
    ! Check the number of lists references
    !
    call strlist_reference_check ()
#ifndef _DO_NOT_TEST
    call logmsg ( "Test : vstrlist_insert into an empty list" )
    call vstrlist_new ( list1 )
    list2 = vstrlist_insert ( list1 , 1 , "java" )
    call vstrlist_free ( list1 )
    length = vstrlist_length ( list2 )
    call assert ( length == 1, "Wrong number of elements.")
    ! Check #1
    string1 = vstrlist_index ( list2 , 1 )
    call assertVstring_charstring ( string1 , "java" , "Wrong string index 1." )
    call vstring_free ( string1 )
    ! Clean-up
    call vstrlist_free ( list2 )
#endif
    !
    call logmsg ( "Test : vstrlist_range" )
    call vstrlist_new ( list1 )
    call vstrlist_append ( list1 , "fortran" )
    call vstrlist_append ( list1 , "java" )
    call vstrlist_append ( list1 , "tcl" )
    call vstrlist_append ( list1 , "C++" )
    call vstrlist_append ( list1 , "C#" )
    call vstrlist_append ( list1 , "Algol" )
    list2 = vstrlist_range ( list1 , 2 , 5 )
    call vstrlist_free ( list1 )
    length = vstrlist_length ( list2 )
    call assert ( length == 4, "Wrong number of elements.")
    ! Check #1
    string1 = vstrlist_index ( list2 , 1 )
    call assertVstring_charstring ( string1 , "java" , "Wrong string index 1." )
    call vstring_free ( string1 )
    ! Check #2
    string1 = vstrlist_index ( list2 , 2 )
    call assertVstring_charstring ( string1 , "tcl" , "Wrong string index 2." )
    ! Check #3
    string1 = vstrlist_index ( list2 , 3 )
    call assertVstring_charstring ( string1 , "C++" , "Wrong string index 3." )
    ! Check #4
    string1 = vstrlist_index ( list2 , 4 )
    call assertVstring_charstring ( string1 , "C#" , "Wrong string index 4." )
    call vstring_free ( string1 )
    ! Cleanup
    call vstrlist_free ( list2 )
    !
    ! Check the number of lists references
    !
    call strlist_reference_check ()
    !
    !!$    ! Generates lots of errors, as expected.
    !!$    call logmsg ( "Test : vstrlist_range error case" )
    !!$    call vstrlist_new ( list1 )
    !!$    call vstrlist_append ( list1 , "fortran" )
    !!$    call vstrlist_append ( list1 , "java" )
    !!$    call vstrlist_append ( list1 , "tcl" )
    !!$    call vstrlist_append ( list1 , "C++" )
    !!$    call vstrlist_append ( list1 , "C#" )
    !!$    call vstrlist_append ( list1 , "Algol" )
    !!$    list2 = vstrlist_range ( list1 , 0 , 5 )
    !!$    call vstrlist_free ( list1 )
    !!$    call vstrlist_free ( list2 )
    !
!
    !
    call logmsg ( "Test : vstrlist_set" )
    call vstrlist_new ( list1 )
    call vstrlist_append ( list1 , "fortran" )
    call vstrlist_append ( list1 , "java" )
    call vstrlist_append ( list1 , "tcl" )
    call vstrlist_append ( list1 , "C++" )
    call vstrlist_append ( list1 , "C#" )
    call vstrlist_append ( list1 , "Algol" )
    call vstrlist_set ( list1 , 2 , "Python" )
    length = vstrlist_length ( list1 )
    call assert ( length == 6, "Wrong number of elements.")
    ! Check #2
    string1 = vstrlist_index ( list1 , 2 )
    call assertVstring_charstring ( string1 , "Python" , "Wrong string index 2." )
    call vstring_free ( string1 )
    ! Cleanup
    call vstrlist_free ( list1 )
    !
    ! Check the number of lists references
    !
    call strlist_reference_check ()
  end subroutine test_m_vstringlist_all
  !
  ! Test vstrlist_split
  !
  subroutine test_m_vstringlist_split ()
    type ( t_vstring ) :: string1
    type ( t_vstringlist ) :: listOfComponents
    integer :: numberOfComponents
    !
    ! Check the number of lists references
    !
    call strlist_reference_check ()
    !
    call logmsg ( "Test : vstrlist_split with default split char " )
    call vstring_new ( string1 , "my string" ) 
    listOfComponents = vstrlist_split ( string1 )
    numberOfComponents = vstrlist_length ( listOfComponents )
    call assert ( numberOfComponents == 2 , "Wrong vstrlist_split (1)." )
    call assertVstring_charstringindex ( listOfComponents , 1 , "my" , "Wrong vstrlist_split. (2)" )
    call assertVstring_charstringindex ( listOfComponents , 2 , "string" , "Wrong vstrlist_split. (2)" )
    call vstring_free ( string1 )
    call vstrlist_free ( listOfComponents )
    !
    ! Check the number of lists references
    !
    call strlist_reference_check ()
    !
    call logmsg ( "Test : vstrlist_split with empty split char " )
    call vstring_new ( string1 , "my s" )
    listOfComponents = vstrlist_split ( string1 , "" )
    numberOfComponents = vstrlist_length ( listOfComponents )
    call assert ( numberOfComponents==4 , "Wrong vstrlist_split (1)." )
    call assertVstring_charstringindex ( listOfComponents , 1 , "m" , "Wrong vstrlist_split. (2)" )
    call assertVstring_charstringindex ( listOfComponents , 2 , "y" , "Wrong vstrlist_split. (2)" )
    call assertVstring_charstringindex ( listOfComponents , 3 , " " , "Wrong vstrlist_split. (2)" )
    call assertVstring_charstringindex ( listOfComponents , 4 , "s" , "Wrong vstrlist_split. (2)" )
    call vstring_free ( string1 )
    call vstrlist_free ( listOfComponents )
    !
    ! Check the number of lists references
    !
    call strlist_reference_check ()
    !
    call logmsg ( "Test : vstrlist_split an empty string" )
    call vstring_new ( string1 , "" )
    listOfComponents = vstrlist_split ( string1 )
    numberOfComponents = vstrlist_length ( listOfComponents )
    call assert ( numberOfComponents==0 , "Wrong vstrlist_split (1)." )
    call vstring_free ( string1 )
    call vstrlist_free ( listOfComponents )
    !
    ! Check the number of lists references
    !
    call strlist_reference_check ()
    !
    call logmsg ( "Test : vstrlist_split a blank space" )
    call vstring_new ( string1 , " " )
    listOfComponents = vstrlist_split ( string1 )
    numberOfComponents = vstrlist_length ( listOfComponents )
    call assert ( numberOfComponents==1 , "Wrong vstrlist_split (1)." )
    call assertVstring_charstringindex ( listOfComponents , 1 , "" , "Wrong vstrlist_split. (2)" )
    call vstring_free ( string1 )
    call vstrlist_free ( listOfComponents )
    !
    ! Check the number of lists references
    !
    call strlist_reference_check ()
    !
    call logmsg ( "Test : vstrlist_split a word starting and ending with blank space" )
    call vstring_new ( string1 , " my string " )
    listOfComponents = vstrlist_split ( string1 )
    numberOfComponents = vstrlist_length ( listOfComponents )
    call assert ( numberOfComponents==3 , "Wrong vstrlist_split (1)." )
    call assertVstring_charstringindex ( listOfComponents , 1 , "" , "Wrong vstrlist_split. (2)" )
    call assertVstring_charstringindex ( listOfComponents , 2 , "my" , "Wrong vstrlist_split. (2)" )
    call assertVstring_charstringindex ( listOfComponents , 3 , "string" , "Wrong vstrlist_split. (2)" )
    call vstring_free ( string1 )
    call vstrlist_free ( listOfComponents )
    !
    ! Check the number of lists references
    !
    call strlist_reference_check ()
    !
    call logmsg ( "Test : vstrlist_split a regular string with no space" )
    call vstring_new ( string1 , "comp.lang.fortran" )
    listOfComponents = vstrlist_split ( string1 )
    numberOfComponents = vstrlist_length ( listOfComponents )
    call assert ( numberOfComponents==1 , "Wrong vstrlist_split (1)." )
    call assertVstring_charstringindex ( listOfComponents , 1 , "comp.lang.fortran" , "Wrong vstrlist_split. (2)" )
    call vstring_free ( string1 )
    call vstrlist_free ( listOfComponents )
    !
    !
    ! Check the number of lists references
    !
    call strlist_reference_check ()
    !
    call logmsg ( "Test : vstrlist_split a regular string with dot as separator" )
    call vstring_new ( string1 , "comp.lang.fortran" )
    listOfComponents = vstrlist_split ( string1 , "." )
    numberOfComponents = vstrlist_length ( listOfComponents )
    call assert ( numberOfComponents==3 , "Wrong vstrlist_split (1)." )
    call assertVstring_charstringindex ( listOfComponents , 1 , "comp" , "Wrong vstrlist_split. (2)" )
    call assertVstring_charstringindex ( listOfComponents , 2 , "lang" , "Wrong vstrlist_split. (2)" )
    call assertVstring_charstringindex ( listOfComponents , 3 , "fortran" , "Wrong vstrlist_split. (2)" )
    call vstring_free ( string1 )
    call vstrlist_free ( listOfComponents )
    !
    !
    ! Check the number of lists references
    !
    call strlist_reference_check ()
    !
    call logmsg ( "Test : vstrlist_join with default split char " )
    call vstrlist_new ( listOfComponents )
    call vstrlist_append ( listOfComponents , "comp" )
    call vstrlist_append ( listOfComponents , "lang" )
    call vstrlist_append ( listOfComponents , "fortran" )
    string1 = vstrlist_join ( listOfComponents )
    call assertVstring_charstring ( string1 , "comp lang fortran" , "Wrong vstrlist_join." )
    call vstrlist_free ( listOfComponents )
    call vstring_free ( string1 )
    !
    ! Check the number of lists references
    !
    call strlist_reference_check ()
    !
    call logmsg ( "Test : vstrlist_join with default split char " )
    call vstrlist_new ( listOfComponents )
    call vstrlist_append ( listOfComponents , "comp" )
    call vstrlist_append ( listOfComponents , "lang" )
    call vstrlist_append ( listOfComponents , "fortran" )
    string1 = vstrlist_join ( listOfComponents , "." )
    call assertVstring_charstring ( string1 , "comp.lang.fortran" , "Wrong vstrlist_join." )
    call vstrlist_free ( listOfComponents )
    call vstring_free ( string1 )
    !
    ! Check the number of lists references
    !
    call strlist_reference_check ()
  end subroutine test_m_vstringlist_split
  !
  ! test_mstringlist_search --
  !   Test searching in a list of strings.
  !
  subroutine test_mstringlist_search ()
    type ( t_vstringlist ) :: list1
    type ( t_vstringlist ) :: list2
    integer :: strindex
    integer :: length
    !
    !
    ! Check the number of lists references
    !
    call strlist_reference_check ()
    !
    call logmsg ( "Test : vstrlist_search" )
    call vstrlist_new ( list1 )
    call vstrlist_append ( list1 , "comp" )
    call vstrlist_append ( list1 , "lang" )
    call vstrlist_append ( list1 , "fortran" )
    strindex = vstrlist_search ( list1 , "lang" )
    call assert ( strindex == 2 , "Wrong vstrlist_search" )
    call vstrlist_free ( list1 )
    !
    call logmsg ( "Test : vstrlist_search and not found" )
    call vstrlist_new ( list1 )
    call vstrlist_append ( list1 , "comp" )
    call vstrlist_append ( list1 , "lang" )
    call vstrlist_append ( list1 , "fortran" )
    strindex = vstrlist_search ( list1 , "java" )
    call assert ( strindex == 0 , "Wrong vstrlist_search" )
    call vstrlist_free ( list1 )
    !
    call logmsg ( "Test : vstrlist_search starting at 2" )
    call vstrlist_new ( list1 )
    call vstrlist_append ( list1 , "comp" )
    call vstrlist_append ( list1 , "lang" )
    call vstrlist_append ( list1 , "fortran" )
    strindex = vstrlist_search ( list1 , "comp" , first = 2 )
    call assert ( strindex == 0 , "Wrong vstrlist_search" )
    call vstrlist_free ( list1 )
    !
    call logmsg ( "Test : vstrlist_search starting at 2" )
    call vstrlist_new ( list1 )
    call vstrlist_append ( list1 , "comp" )
    call vstrlist_append ( list1 , "lang" )
    call vstrlist_append ( list1 , "fortran" )
    strindex = vstrlist_search ( list1 , "fortran" , first = 2 )
    call assert ( strindex == 3 , "Wrong vstrlist_search" )
    call vstrlist_free ( list1 )
    !
    call logmsg ( "Test : vstrlist_search with notmatch option" )
    call vstrlist_new ( list1 )
    call vstrlist_append ( list1 , "comp" )
    call vstrlist_append ( list1 , "lang" )
    call vstrlist_append ( list1 , "fortran" )
    strindex = vstrlist_search ( list1 , "fortran" , notmatch = .true. )
    call assert ( strindex == 1 , "Wrong vstrlist_search" )
    call vstrlist_free ( list1 )
    !
    call logmsg ( "Test : vstrlist_search with notmatch option = true" )
    call vstrlist_new ( list1 )
    call vstrlist_append ( list1 , "comp" )
    call vstrlist_append ( list1 , "lang" )
    call vstrlist_append ( list1 , "fortran" )
    strindex = vstrlist_search ( list1 , "comp" , notmatch = .true. )
    call assert ( strindex == 2 , "Wrong vstrlist_search" )
    call vstrlist_free ( list1 )
    !
    call logmsg ( "Test : vstrlist_search with notmatch option = false " )
    call vstrlist_new ( list1 )
    call vstrlist_append ( list1 , "comp" )
    call vstrlist_append ( list1 , "lang" )
    call vstrlist_append ( list1 , "fortran" )
    strindex = vstrlist_search ( list1 , "comp" , notmatch = .false. )
    call assert ( strindex == 1 , "Wrong vstrlist_search" )
    call vstrlist_free ( list1 )
    !
    call logmsg ( "Test : vstrlist_search with exact option" )
    call vstrlist_new ( list1 )
    call vstrlist_append ( list1 , "comp" )
    call vstrlist_append ( list1 , "lang" )
    call vstrlist_append ( list1 , "fortran" )
    strindex = vstrlist_search ( list1 , "la*" , exact = .true. )
    call assert ( strindex == 0 , "Wrong vstrlist_search" )
    call vstrlist_free ( list1 )
    !
    call logmsg ( "Test : vstrlist_search with exact option" )
    call vstrlist_new ( list1 )
    call vstrlist_append ( list1 , "comp" )
    call vstrlist_append ( list1 , "lang" )
    call vstrlist_append ( list1 , "fortran" )
    strindex = vstrlist_search ( list1 , "la*" , exact = .false. )
    call assert ( strindex == 2 , "Wrong vstrlist_search" )
    call vstrlist_free ( list1 )
    !
    ! When result is a list.
    !
    call logmsg ( "Test : vstrlist_search with a list as result" )
    call vstrlist_new ( list1 )
    call vstrlist_append ( list1 , "comp" )
    call vstrlist_append ( list1 , "lang" )
    call vstrlist_append ( list1 , "fortran" )
    list2 = vstrlist_lsearch ( list1 , "la*" , allitems = .false. )
    length = vstrlist_length ( list2 )
    call assert ( length == 1 , "Wrong vstrlist_search" )
    call assertVstring_charstringindex ( list2 , 1 , "lang" , "Wrong vstrlist_search (2)" )
    call vstrlist_free ( list1 )
    call vstrlist_free ( list2 )
    !
    call logmsg ( "Test : vstrlist_search with a list as result" )
    call vstrlist_new ( list1 )
    call vstrlist_append ( list1 , "comp" )
    call vstrlist_append ( list1 , "lang" )
    call vstrlist_append ( list1 , "fortran" )
    list2 = vstrlist_lsearch ( list1 , "foo*" , allitems = .false. )
    length = vstrlist_length ( list2 )
    call assert ( length == 0 , "Wrong vstrlist_search" )
    call vstrlist_free ( list1 )
    call vstrlist_free ( list2 )
    !
    call logmsg ( "Test : vstrlist_search with a list as result" )
    call vstrlist_new ( list1 )
    call vstrlist_append ( list1 , "C++" )
    call vstrlist_append ( list1 , "Fortran 77" )
    call vstrlist_append ( list1 , "Fortran 90" )
    call vstrlist_append ( list1 , "Fortran 95" )
    call vstrlist_append ( list1 , "Fortran 2003" )
    call vstrlist_append ( list1 , "C#" )
    list2 = vstrlist_lsearch ( list1 , "C*" , allitems = .true. )
    length = vstrlist_length ( list2 )
    call assert ( length == 2 , "Wrong vstrlist_search" )
    call assertVstring_charstringindex ( list2 , 1 , "C++" , "Wrong vstrlist_search (2)" )
    call assertVstring_charstringindex ( list2 , 2 , "C#" , "Wrong vstrlist_search (2)" )
    call vstrlist_free ( list1 )
    call vstrlist_free ( list2 )
    !
    call logmsg ( "Test : vstrlist_search with a list as result" )
    call vstrlist_new ( list1 )
    call vstrlist_append ( list1 , "C++" )
    call vstrlist_append ( list1 , "Fortran 77" )
    call vstrlist_append ( list1 , "Fortran 90" )
    call vstrlist_append ( list1 , "Fortran 95" )
    call vstrlist_append ( list1 , "Fortran 2003" )
    call vstrlist_append ( list1 , "C#" )
    list2 = vstrlist_lsearch ( list1 , "C*" )
    length = vstrlist_length ( list2 )
    call assert ( length == 1 , "Wrong vstrlist_search" )
    call assertVstring_charstringindex ( list2 , 1 , "C++" , "Wrong vstrlist_search (2)" )
    call vstrlist_free ( list1 )
    call vstrlist_free ( list2 )
    !
    ! Check the number of lists references
    !
    call strlist_reference_check ()
  end subroutine test_mstringlist_search
  !
  ! test_mstringlist_sort --
  !   Test string list sorting.
  !
  subroutine test_mstringlist_sort ()
    type ( t_vstringlist ) :: list1
    type ( t_vstringlist ) :: list2
    integer :: length
    interface
       integer function test_compare ( string_a , string_b )
         use m_vstring, only : t_vstring
         type(t_vstring), intent(in) :: string_a
         type(t_vstring), intent(in) :: string_b
       end function test_compare
    end interface
    !
    !
    ! Check the number of lists references
    !
    call strlist_reference_check ()
    !
    call logmsg ( "Test : vstrlist_sort with no option" )
    call vstrlist_new ( list1 )
    call vstrlist_append ( list1 , "comp" )
    call vstrlist_append ( list1 , "lang" )
    call vstrlist_append ( list1 , "fortran" )
    list2 = vstrlist_sort ( list1 )
    length = vstrlist_length ( list2 )
    call assert ( length == 3 , "Wrong vstrlist_sort length" )
    call assertVstring_charstringindex ( list2 , 1 , "comp" , "Wrong vstrlist_sort (1)" )
    call assertVstring_charstringindex ( list2 , 2 , "fortran" , "Wrong vstrlist_sort (1)" )
    call assertVstring_charstringindex ( list2 , 3 , "lang" , "Wrong vstrlist_sort (1)" )
    call vstrlist_free ( list1 )
    call vstrlist_free ( list2 )
    !
    call logmsg ( "Test : vstrlist_sort with increasing option as true" )
    call vstrlist_new ( list1 )
    call vstrlist_append ( list1 , "comp" )
    call vstrlist_append ( list1 , "lang" )
    call vstrlist_append ( list1 , "fortran" )
    list2 = vstrlist_sort ( list1 , increasing = .true. )
    length = vstrlist_length ( list2 )
    call assert ( length == 3 , "Wrong vstrlist_sort length" )
    call assertVstring_charstringindex ( list2 , 1 , "comp" , "Wrong vstrlist_sort (2)" )
    call assertVstring_charstringindex ( list2 , 2 , "fortran" , "Wrong vstrlist_sort (2)" )
    call assertVstring_charstringindex ( list2 , 3 , "lang" , "Wrong vstrlist_sort (2)" )
    call vstrlist_free ( list1 )
    call vstrlist_free ( list2 )
    !
    call logmsg ( "Test : vstrlist_sort with increasing option as false" )
    call vstrlist_new ( list1 )
    call vstrlist_append ( list1 , "comp" )
    call vstrlist_append ( list1 , "lang" )
    call vstrlist_append ( list1 , "fortran" )
    list2 = vstrlist_sort ( list1 , increasing = .false. )
    length = vstrlist_length ( list2 )
    call assert ( length == 3 , "Wrong vstrlist_sort length" )
    call assertVstring_charstringindex ( list2 , 1 , "lang" , "Wrong vstrlist_sort (3)" )
    call assertVstring_charstringindex ( list2 , 2 , "fortran" , "Wrong vstrlist_sort (3)" )
    call assertVstring_charstringindex ( list2 , 3 , "comp" , "Wrong vstrlist_sort (3)" )
    call vstrlist_free ( list1 )
    call vstrlist_free ( list2 )
    !
    call logmsg ( "Test : vstrlist_sort with classtype as ascii" )
    call vstrlist_new ( list1 )
    call vstrlist_append ( list1 , "comp" )
    call vstrlist_append ( list1 , "lang" )
    call vstrlist_append ( list1 , "fortran" )
    list2 = vstrlist_sort ( list1 , classtype = "ascii" )
    length = vstrlist_length ( list2 )
    call assert ( length == 3 , "Wrong vstrlist_sort length" )
    call assertVstring_charstringindex ( list2 , 1 , "comp" , "Wrong vstrlist_sort (4)" )
    call assertVstring_charstringindex ( list2 , 2 , "fortran" , "Wrong vstrlist_sort (4)" )
    call assertVstring_charstringindex ( list2 , 3 , "lang" , "Wrong vstrlist_sort (4)" )
    call vstrlist_free ( list1 )
    call vstrlist_free ( list2 )
    !
    call logmsg ( "Test : vstrlist_sort with classtype as dictionnary" )
    call vstrlist_new ( list1 )
    call vstrlist_append ( list1 , "bigbang" )
    call vstrlist_append ( list1 , "bigBoy" )
    call vstrlist_append ( list1 , "bigboy" )
    list2 = vstrlist_sort ( list1 , classtype = "dictionnary" )
    length = vstrlist_length ( list2 )
    call assert ( length == 3 , "Wrong vstrlist_sort length" )
    call assertVstring_charstringindex ( list2 , 1 , "bigbang" , "Wrong vstrlist_sort (5)" )
    ! One cannot state about the 2nd element and the 3d, since the order 
    ! is not stated.
    call vstrlist_free ( list1 )
    call vstrlist_free ( list2 )
    !
    call logmsg ( "Test : vstrlist_sort with classtype as dictionnary and decreasing" )
    call vstrlist_new ( list1 )
    call vstrlist_append ( list1 , "bigbang" )
    call vstrlist_append ( list1 , "bigBoy" )
    call vstrlist_append ( list1 , "bigboy" )
    list2 = vstrlist_sort ( list1 , classtype = "dictionnary" , increasing = .false.)
    length = vstrlist_length ( list2 )
    call assert ( length == 3 , "Wrong vstrlist_sort length" )
    ! One cannot state about the 1st and the 2d since the order 
    ! is not stated.
    call assertVstring_charstringindex ( list2 , 3 , "bigbang" , "Wrong vstrlist_sort (6)" )
    call vstrlist_free ( list1 )
    call vstrlist_free ( list2 )
    !
    call logmsg ( "Test : vstrlist_sort with integers" )
    call vstrlist_new ( list1 )
    call vstrlist_append ( list1 , "4" )
    call vstrlist_append ( list1 , "2" )
    call vstrlist_append ( list1 , "10" )
    list2 = vstrlist_sort ( list1 , classtype = "integer" )
    length = vstrlist_length ( list2 )
    call assert ( length == 3 , "Wrong vstrlist_sort length" )
    call assertVstring_charstringindex ( list2 , 1 , "2" , "Wrong vstrlist_sort (7)" )
    call assertVstring_charstringindex ( list2 , 2 , "4" , "Wrong vstrlist_sort (7)" )
    call assertVstring_charstringindex ( list2 , 3 , "10" , "Wrong vstrlist_sort (7)" )
    call vstrlist_free ( list1 )
    call vstrlist_free ( list2 )
    !
    call logmsg ( "Test : vstrlist_sort with decreasing integers" )
    call vstrlist_new ( list1 )
    call vstrlist_append ( list1 , "4" )
    call vstrlist_append ( list1 , "2" )
    call vstrlist_append ( list1 , "10" )
    list2 = vstrlist_sort ( list1 , increasing = .false. , classtype = "integer" )
    length = vstrlist_length ( list2 )
    call assert ( length == 3 , "Wrong vstrlist_sort length" )
    call assertVstring_charstringindex ( list2 , 1 , "10" , "Wrong vstrlist_sort (8)" )
    call assertVstring_charstringindex ( list2 , 2 , "4" , "Wrong vstrlist_sort (8)" )
    call assertVstring_charstringindex ( list2 , 3 , "2" , "Wrong vstrlist_sort (8)" )
    call vstrlist_free ( list1 )
    call vstrlist_free ( list2 )
    !
    call logmsg ( "Test : vstrlist_sort with real" )
    call vstrlist_new ( list1 )
    call vstrlist_append ( list1 , "4." )
    call vstrlist_append ( list1 , "-2." )
    call vstrlist_append ( list1 , "10." )
    list2 = vstrlist_sort ( list1 , classtype = "real" )
    length = vstrlist_length ( list2 )
    call assert ( length == 3 , "Wrong vstrlist_sort length" )
    call assertVstring_charstringindex ( list2 , 1 , "-2." , "Wrong vstrlist_sort (9)" )
    call assertVstring_charstringindex ( list2 , 2 , "4." , "Wrong vstrlist_sort (9)" )
    call assertVstring_charstringindex ( list2 , 3 , "10." , "Wrong vstrlist_sort (9)" )
    call vstrlist_free ( list1 )
    call vstrlist_free ( list2 )
    !
    call logmsg ( "Test : vstrlist_sort with decreasing real" )
    call vstrlist_new ( list1 )
    call vstrlist_append ( list1 , "4." )
    call vstrlist_append ( list1 , "-2." )
    call vstrlist_append ( list1 , "10." )
    list2 = vstrlist_sort ( list1 , increasing = .false. , classtype = "real" )
    length = vstrlist_length ( list2 )
    call assert ( length == 3 , "Wrong vstrlist_sort length" )
    call assertVstring_charstringindex ( list2 , 1 , "10." , "Wrong vstrlist_sort (10)" )
    call assertVstring_charstringindex ( list2 , 2 , "4." , "Wrong vstrlist_sort (10)" )
    call assertVstring_charstringindex ( list2 , 3 , "-2." , "Wrong vstrlist_sort (10)" )
    call vstrlist_free ( list1 )
    call vstrlist_free ( list2 )
    !
    call logmsg ( "Test : vstrlist_sort with customized command" )
    call vstrlist_new ( list1 )
    call vstrlist_append ( list1 , "Newton" )
    call vstrlist_append ( list1 , "Michael" )
    call vstrlist_append ( list1 , "Einstein" )
    list2 = vstrlist_sort ( list1 , test_compare )
    length = vstrlist_length ( list2 )
    call assert ( length == 3 , "Wrong vstrlist_sort length" )
    call assertVstring_charstringindex ( list2 , 1 , "Michael" , "Wrong vstrlist_sort (10)" )
    call assertVstring_charstringindex ( list2 , 2 , "Einstein" , "Wrong vstrlist_sort (10)" )
    call assertVstring_charstringindex ( list2 , 3 , "Newton" , "Wrong vstrlist_sort (10)" )
    call vstrlist_free ( list1 )
    call vstrlist_free ( list2 )
    !
    call logmsg ( "Test : vstrlist_sort with customized command and unique" )
    call vstrlist_new ( list1 )
    call vstrlist_append ( list1 , "Newton" )
    call vstrlist_append ( list1 , "Michael" )
    call vstrlist_append ( list1 , "Einstein" )
    call vstrlist_append ( list1 , "Michael" )
    call vstrlist_append ( list1 , "Einstein" )
    call vstrlist_append ( list1 , "Einstein" )
    list2 = vstrlist_sort ( list1 , test_compare , unique = .true. )
    length = vstrlist_length ( list2 )
    call assert ( length == 3 , "Wrong vstrlist_sort length" )
    call assertVstring_charstringindex ( list2 , 1 , "Michael" , "Wrong vstrlist_sort (11)" )
    call assertVstring_charstringindex ( list2 , 2 , "Einstein" , "Wrong vstrlist_sort (11)" )
    call assertVstring_charstringindex ( list2 , 3 , "Newton" , "Wrong vstrlist_sort (11)" )
    call vstrlist_free ( list1 )
    call vstrlist_free ( list2 )
    !
    call logmsg ( "Test : vstrlist_sort with integer and unique" )
    call vstrlist_new ( list1 )
    call vstrlist_append ( list1 , "3" )
    call vstrlist_append ( list1 , "2" )
    call vstrlist_append ( list1 , "2" )
    call vstrlist_append ( list1 , "1" )
    call vstrlist_append ( list1 , "3" )
    call vstrlist_append ( list1 , "2" )
    list2 = vstrlist_sort ( list1 , classtype = "integer" , unique = .true. )
    length = vstrlist_length ( list2 )
    call assert ( length == 3 , "Wrong vstrlist_sort length" )
    call assertVstring_charstringindex ( list2 , 1 , "1" , "Wrong vstrlist_sort (12)" )
    call assertVstring_charstringindex ( list2 , 2 , "2" , "Wrong vstrlist_sort (12)" )
    call assertVstring_charstringindex ( list2 , 3 , "3" , "Wrong vstrlist_sort (12)" )
    call vstrlist_free ( list1 )
    call vstrlist_free ( list2 )
    !
    call logmsg ( "Test : vstrlist_sort with ascii and unique" )
    call vstrlist_new ( list1 )
    call vstrlist_append ( list1 , "Newton" )
    call vstrlist_append ( list1 , "Michael" )
    call vstrlist_append ( list1 , "Einstein" )
    call vstrlist_append ( list1 , "Michael" )
    call vstrlist_append ( list1 , "Einstein" )
    call vstrlist_append ( list1 , "Einstein" )
    list2 = vstrlist_sort ( list1 , classtype = "ascii" , unique = .true. )
    length = vstrlist_length ( list2 )
    call assert ( length == 3 , "Wrong vstrlist_sort length" )
    call assertVstring_charstringindex ( list2 , 1 , "Einstein" , "Wrong vstrlist_sort (13)" )
    call assertVstring_charstringindex ( list2 , 2 , "Michael" , "Wrong vstrlist_sort (13)" )
    call assertVstring_charstringindex ( list2 , 3 , "Newton" , "Wrong vstrlist_sort (13)" )
    call vstrlist_free ( list1 )
    call vstrlist_free ( list2 )
    !
    call logmsg ( "Test : vstrlist_sort with classtype as dictionnary and unique" )
    call vstrlist_new ( list1 )
    call vstrlist_append ( list1 , "bigbang" )
    call vstrlist_append ( list1 , "bigBoy" )
    call vstrlist_append ( list1 , "bigboy" )
    list2 = vstrlist_sort ( list1 , classtype = "dictionnary" , unique = .true. )
    length = vstrlist_length ( list2 )
    call assert ( length == 2 , "Wrong vstrlist_sort length" )
    call assertVstring_charstringindex ( list2 , 1 , "bigbang" , "Wrong vstrlist_sort (14)" )
    ! One cannot state about the 2d since the order 
    ! is not stated : it may be bigBoy or bigboy.
    call vstrlist_free ( list1 )
    call vstrlist_free ( list2 )
    !
  end subroutine test_mstringlist_sort
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
  !   Check that the computed vstring is equal to the expected character string
  !   and updates the assertion system.
  !
  subroutine assertVstring_charstring ( computedString , expectedString , message )
    implicit none
    type ( t_vstring ), intent(in) :: computedString
    character ( len=* ), intent(in) :: expectedString
    character(len=*), intent(in) :: message
    type ( t_vstring ) :: expectedVString
    call vstring_new ( expectedVString , expectedString )
    call assertVstring_vstring ( computedString , expectedVString , message )
    call vstring_free ( expectedVString )
  end subroutine assertVstring_charstring
  !
  ! assertVstring_charstringindex --
  !   Check that the vstring at index #icomponent of the given vstring list 
  !   is equal to the expected character string and updates the assertion system.
  !
  subroutine assertVstring_charstringindex ( list , icomponent , expectedString , message )
    implicit none
    type ( t_vstringlist ), intent(in) :: list
    integer , intent(in) :: icomponent
    character ( len=* ), intent(in) :: expectedString
    character(len=*), intent(in) :: message
    type ( t_vstring ) :: computedString
    computedString = vstrlist_index ( list , icomponent )
    call assertVstring_charstring ( computedString , expectedString , message )
    call vstring_free ( computedString )
  end subroutine assertVstring_charstringindex

end program test_m_vstringlist
  !
  ! Compare the two strings with special comparison.
  !
  integer function test_compare ( string_a , string_b )
    use m_vstring, only : t_vstring , vstring_equals , vstring_compare
    type(t_vstring), intent(in) :: string_a
    type(t_vstring), intent(in) :: string_b
    logical :: equalsreference_a
    logical :: equalsreference_b
    equalsreference_a = vstring_equals ( string_a , "Michael" )
    equalsreference_b = vstring_equals ( string_b , "Michael" )
    if ( equalsreference_a ) then
       if ( equalsreference_b ) then
          test_compare = 0
       else
          test_compare = -1
       endif
    elseif ( equalsreference_b ) then
          test_compare = 1       
    else
       test_compare = vstring_compare ( string_a , string_b )
    endif
  end function test_compare

