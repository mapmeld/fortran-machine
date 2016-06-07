!
! This program is a testing component for the module m_products.
!
program test_vfile
  use m_vfile, only : &
       vfile_rename, &
       vfile_copy, &
       vfile_pwd, &
       vfile_exists, &
       vfile_delete, &
       vfile_atime, &
       vfile_extension, &
       vfile_tail, &
       vfile_join, &
       vfile_add_extension , &
       vfile_normalize, &
       vfile_findbypattern, &
       vfile_tempdir, &
       vfile_startup, &
       vfile_shutdown, &
       vfile_split, &
       vfile_tempfile , &
       vfile_touch , &
       vfile_mtime, &
       vfile_isdirectory , &
       vfile_mkdir, &
       VFILE_PATHTYPE_ABSOLUTE, &
       VFILE_PATHTYPE_RELATIVE, &
       VFILE_PATHTYPE_VOLUMERELATIVE , &
       vfile_pathtype, &
       vfile_nativename , &
       VFILE_ERROR_SOURCE_FILE_DOES_NOT_EXIST , &
       VFILE_ERROR_UNABLE_TO_OPEN_TARGET , &
       VFILE_ERROR_OK , &
       vfile_set_stoponerror, &
       vfile_dirname , &
       vfile_volumes , &
       vfile_listfiles , &
       vfile_type , &
       vfile_isfile , &
       vfile_find
  use m_fileunit, only : &
       fileunit_getallopen , &
       fileunit_getfreeunit , &
       fileunit_displayopen
  use m_platform, only : &
       platform_get_platform, &
       PLATFORM_PLATFORM_WINDOWS, &
       PLATFORM_PLATFORM_UNIX, &
       PLATFORM_PLATFORM_MAC, &
       platform_cd
  use m_vstring, only : &
       t_vstring, &
       vstring_new , &
       vstring_free , &
       vstring_cast , &
       vstring_length , &
       vstring_equals , &
       vstring_reference_get ,&
       vstring_set_stoponerror , &
       vstring_first , &
       vstring_match
  use m_vstringlist , only : &
       t_vstringlist , &
       vstrlist_index ,&
       vstrlist_length , &
       vstrlist_free , &
       vstrlist_search , &
       vstringlist_set_stoponerror , &
       vstrlist_new , &
       vstrlist_append ,&
       vstrlist_join
  use m_vstrplatform, only : &
       vstrplatform_cd
  implicit none
  integer :: assertTotalTestSuccess
  integer :: assertTotalTestFail
  integer :: assertTestIndex
  integer , parameter :: log_unit = 12
  !
  ! Run the tests
  !
  call test_m_vfile_all ()
contains
  !
  ! Include support for unit tests.
  !
  include "test_support.f90"
  !
  ! Test all
  !
  subroutine test_m_vfile_all ()
    integer, parameter :: filelength = 200
    logical :: fexist
    !
    ! Initialize the test system
    !
    call log_startup ( "test_m_vfile.log" )
    call assert_startup ()
    !
    call vfile_startup ()
    call vfile_set_stoponerror ( .false. )
    call vstring_set_stoponerror ( .false. )
    call vstringlist_set_stoponerror ( .false. )
    !
    ! Check that the files used in the test are OK !
    !
    fexist = vfile_exists ( "declaration.txt" )
    call assert ( fexist , "The input file declaration.txt does not exist." )
    fexist = vfile_exists ( "declaration2.txt" )
    call assert ( fexist , "The input file declaration2.txt does not exist." )
    !
    ! Test the directory processing : pwd, cd, isdirectory, mkdir
    !
    call test_pwd_cd_mkdir ()
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! Check that there are no memory leaks.
    !
    call string_reference_check ()
    !
    ! Test the copy , rename, touch, atime
    !
    call test_file_copy ()
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! Check that there are no memory leaks.
    !
    call string_reference_check ()
    !
    ! Test vfile_dirname, tail, extension, join  etc...
    !
    call test_file_dirname ()
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! Check that there are no memory leaks.
    !
    call string_reference_check ()
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! Check that there are no memory leaks.
    !
    call string_reference_check ()
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! Test file normalize, pathtype, nativename
    !
    call test_vfile_normalize ()
    !
    ! Check that there are no memory leaks.
    !
    call string_reference_check ()
    !
    ! Test temp file, dir
    !
    call test_file_tempfiledir ()
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! Check that there are no memory leaks.
    !
    call string_reference_check ()
    !
    ! Test find by pattern, file split, listfiles
    !
    call test_vfile_findbypattern ()
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! Check that there are no memory leaks.
    !
    call string_reference_check ()
    !
    ! Shutdown vfile
    !
    call vfile_shutdown ()
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! Shutdown the tests
    !
    call logmsg ( "Test suite completed" )
    call assert_shutdown ()
    call log_shutdown ()
  end subroutine test_m_vfile_all
  !
  ! Check invariants of the program
  !
  subroutine test_checkinvariants ()
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! Check that there are no memory leaks.
    !
    call string_reference_check ()
  end subroutine test_checkinvariants
  !
  ! Test the directory processing : pwd, cd, isdirectory, isfile, 
  ! filetype, mkdir
  !
  subroutine test_pwd_cd_mkdir ()
    type ( t_vstring ) :: dirname1
    logical :: force
    logical :: isdir
    type ( t_vstring) :: tempfile
    type ( t_vstring) :: tmpdir
    integer :: status
    type ( t_vstring) :: cwd 
    type ( t_vstring) :: cwd1
    type ( t_vstring) :: cwd2
    type ( t_vstring) :: dirtail
    type ( t_vstring) :: ftail
    logical :: isfile
    type ( t_vstring) :: filetype
    logical :: fileexists
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
    !
    ! test #1 : current working directory
    !
    call logmsg ( "Test #1 : get_cwd" )
    ! Just test that it works
    call vfile_pwd (cwd)
    call logmsg ( "Current working directory:" )
    call logmsg_string ( cwd )
    ftail = vfile_tail ( cwd )
    call assertVstring_charstring ( ftail , "filedir" , "Error in cwd.")
    call vstring_free ( cwd )
    call vstring_free ( ftail )
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
    !
    ! Test #23 : change directory
    !
    call logmsg ( "Test #23 : change directories" )
    call vstrplatform_cd ( ".." )
    call vfile_pwd (cwd)
    dirtail = vfile_tail ( cwd )
    call vstring_free ( cwd )
    call assertVstring_charstring ( dirtail , "tests" , "Error in platform_cd (1)")
    call vstring_free ( dirtail )
    call platform_cd ( "filedir" )
    call vfile_pwd (cwd)
    dirtail = vfile_tail ( cwd )
    call vstring_free ( cwd )
    call assertVstring_charstring ( dirtail , "filedir" , "Error in platform_cd (2)")
    call vstring_free ( dirtail )
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
    !
    ! Test vfile_isdirectory
    !
    call logmsg ( "vfile_isdirectory" )
    call vfile_pwd (cwd1)
    isdir = vfile_isdirectory ( cwd1 )
    call assert ( isdir , "Error in vfile_isdirectory (1)")
    call vfile_pwd (cwd2)
    ! Check for possible side-effects
    call assertVstring_vstring ( cwd1 , cwd2 , "Error in platform_cd (3)")
    call vstring_free ( cwd2 )
    call vstring_free ( cwd1 )
    !
    isdir = vfile_isdirectory ( "testls" )
    call assert ( isdir , "Error in vfile_isdirectory (1)")
    !
    isdir = vfile_isdirectory ( "../filedir/testls" )
    call assert ( isdir , "Error in vfile_isdirectory (1)")
    !
    isdir = vfile_isdirectory ( "fooYepYepYep!" )
    call assert ( .NOT.isdir , "Error in vfile_isdirectory (2)")
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
    !
    ! Test vfile_isfile
    !
    call logmsg ( "vfile_isfile" )
    isfile = vfile_isfile ( "declaration.txt" )
    call assert ( isfile , "Error in vfile_isfile")
    !
    isfile = vfile_isfile ( "testls/declaration.txt" )
    call assert ( isfile , "Error in vfile_isfile")
    !
    isfile = vfile_isfile ( "testls" )
    call assert ( .NOT.isfile , "Error in vfile_isfile")
    !
    isfile = vfile_isfile ( "yayayaHEPHEP" )
    call assert ( .NOT.isfile , "Error in vfile_isfile")
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
    !
    ! Test vfile_exists with a directory argument.
    !
    call logmsg ( "Test : file exists with directory" )
    call vfile_pwd (cwd1)
    fileexists = vfile_exists ( cwd1 )
    call assert ( fileexists , "Error in vfile_exists (1)")
    call vstring_free ( cwd1 )
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
    !
    ! Test vfile_type
    !
    call logmsg ( "filetype" )
    filetype = vfile_type ( "declaration.txt" )
    call assertVstring_charstring ( filetype , "file" , "Error in vfile_type")
    call vstring_free ( filetype )
    !
    filetype = vfile_type ( "testls" , status )
    call assertVstring_charstring ( filetype , "directory" , "Error in vfile_type")
    call assert ( status == 0 , "Error in vfile_type")
    call vstring_free ( filetype )
    !
    filetype = vfile_type ( "yayayaHEPHEP" , status )
    call assert ( status /= 0 , "Error in vfile_type")
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
    !
    ! Test #27 : create / delete a directory
    !
    call logmsg ( "Test #27 : file mkdir" )
    tmpdir = vfile_tempdir (  )
    dirname1 = vfile_join ( tmpdir , "myTestDir" )
    call vfile_mkdir ( dirname1 )
    isdir = vfile_isdirectory ( dirname1 )
    call assert ( isdir , "Error in vfile_mkdir (1)")
    call vfile_delete ( dirname1 )
    isdir = vfile_isdirectory ( dirname1 )
    call assert ( .NOT.isdir , "Error in vfile_mkdir (2)")
    call vstring_free ( tmpdir )
    call vstring_free ( dirname1 )
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
    !
    ! Test #28 : create / delete a non empty directory without / with the force option
    !
    call logmsg ( "Test #28 : file mkdir" )
    tmpdir = vfile_tempdir (  )
    dirname1 = vfile_join ( tmpdir , "myTestDir" )
    call vfile_mkdir ( dirname1 )
    tempfile = vfile_join ( dirname1 , "testfile.txt" )
    call vfile_touch ( tempfile )
    isdir = vfile_isdirectory ( dirname1 )
    call assert ( isdir , "Error in vfile_mkdir (3)")
    call vfile_delete ( dirname1 , status=status )
    call assert ( status/= 0, "Error in vfile_delete (5)")
    force = .true.
    call vfile_delete ( dirname1 , force , status )
    isdir = vfile_isdirectory ( dirname1 )
    call assert ( .NOT.isdir , "Error in vfile_mkdir (4)")
    call vstring_free ( tmpdir )
    call vstring_free ( dirname1 )
    call vstring_free ( tempfile )
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
    !
    ! Test #29 : create a directory which allready exists
    !
    call logmsg ( "Test #29 : file mkdir" )
    tmpdir = vfile_tempdir (  )
    dirname1 = vfile_join ( tmpdir , "myTestDir" )
    call vfile_mkdir ( dirname1 )
    call vfile_mkdir ( dirname1 , status )
    call assert ( status/= 0, "Error in vfile_mkdir (5)")
    call vfile_delete ( dirname1 )
    call vstring_free ( tmpdir )
    call vstring_free ( dirname1 )
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
  end subroutine test_pwd_cd_mkdir
  !
  ! Test  vfile_dirname, tail, extension, join etc...
  !
  subroutine test_file_dirname ()
    type ( t_vstring ) :: dirname
    type ( t_vstring ) :: cwd
    type ( t_vstring ) :: fullname
    type ( t_vstring ) :: fext
    type ( t_vstring ) :: ftail
    type ( t_vstring ) :: fullfilename
    type ( t_vstring ) :: source_file1
    integer :: current_platform
    !
    ! Check that there are no memory leaks.
    !
    call string_reference_check ()
    !
    dirname = vfile_dirname ( "declaration.txt" )
    call assertVstring_charstring ( dirname , "" , "Error in vfile_dirname")
    call vstring_free ( dirname )
    !
    current_platform = platform_get_platform ()
    if (current_platform==PLATFORM_PLATFORM_WINDOWS) then
       ! Simple
       call logmsg ( "Test : file join (1)" )
       call vstring_new ( dirname , "/titi/toto" )
       fullname = vfile_join ( dirname , "declaration.txt" )
       call assertVstring_charstring ( fullname , "/titi/toto/declaration.txt" , "Error in vfile_dirname" )
       call vstring_free ( dirname )
       call vstring_free ( fullname )
       ! More complicated : last slash in the directory name
       call logmsg ( "Test : file join (1)" )
       call vstring_new ( dirname , "/titi/toto/" )
       fullname = vfile_join ( dirname , "declaration.txt" )
       call assertVstring_charstring ( fullname , "/titi/toto/declaration.txt" , "Error in vfile_dirname" )
       call vstring_free ( dirname )
       call vstring_free ( fullname )
       ! More complicated: first slash in the file name
       call logmsg ( "Test : file join (1)" )
       call vstring_new ( dirname , "/titi/toto" )
       fullname = vfile_join ( dirname , "/declaration.txt" )
       call assertVstring_charstring ( fullname , "/declaration.txt" , "Error in vfile_dirname" )
       call vstring_free ( dirname )
       call vstring_free ( fullname )
    endif
    !
    call vfile_pwd (cwd)
    fullname = vfile_join ( cwd , "declaration.txt" )
    dirname = vfile_dirname ( fullname )
    call assertVstring_vstring ( dirname , cwd , "Error in vfile_dirname")
    call vstring_free ( cwd )
    call vstring_free ( fullname )
    call vstring_free ( dirname )
    !
    ! Test #11 : file extension
    !
    call logmsg ( "Test #11 : file extension (1)" )
    fext = vfile_extension ( "declaration.txt" )
    call assertVstring_charstring ( fext , ".txt" , "Error in file extension.")
    call vstring_free ( fext )
    !
    ! Test #12 : file extension, without extension
    !
    call logmsg ( "Test #12 : file extension without extension (2)" )
    fext = vfile_extension ( "declaration" )
    call assertVstring_charstring ( fext , "" , "Error in file extension (2).")
    call vstring_free ( fext )
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
    !
    ! Test #13 : file tail, without dirname
    !
    call logmsg ( "Test #13 : file tail without dirname (1)" )
    ftail = vfile_tail ( "declaration.txt" )
    call assertVstring_charstring ( ftail , "declaration.txt" , "Error in file tail (1).")
    call vstring_free ( ftail )
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! Test #14 : file tail, with dirname (test also vfile_join)
    !
    call logmsg ( "Test #14 : file tail with dirname (2)" )
    call vfile_pwd (cwd)
    fullfilename = vfile_join ( cwd , "declaration.txt" )
    ftail = vfile_tail ( fullfilename )
    call assertVstring_charstring ( ftail ,  "declaration.txt" , "Error in file tail (1).")
    call vstring_free ( cwd )
    call vstring_free ( fullfilename )
    call vstring_free ( ftail )
    !
    ! Test : file tail with an absolute directory name
    !
    call logmsg ( "Test : file tail" )
    ftail = vfile_tail ( "/" )
    call assertVstring_charstring ( ftail , "" , "Error in vfile_normalize (3)")
    call vstring_free ( ftail )
    !
    ! Test : file tail with an absolute directory name ending with a separator
    !
    call logmsg ( "Test : file tail" )
    ftail = vfile_tail ( "/toto/tata/" )
    call assertVstring_charstring ( ftail , "tata" , "Error in vfile_normalize (3)")
    call vstring_free ( ftail )
    !
    ! Test : file tail with an absolute directory name on windows
    !
    current_platform = platform_get_platform ()
    if (current_platform==PLATFORM_PLATFORM_WINDOWS) then
       !
       call logmsg ( "Test : file normalize" )
       ftail = vfile_tail ( "C:/" )
       call assertVstring_charstring ( ftail , "" , "Error in vfile_normalize (3)")
       call vstring_free ( ftail )
       !
       call logmsg ( "Test : file normalize" )
       ftail = vfile_tail ( "C:/foo/faa" )
       call assertVstring_charstring ( ftail , "faa" , "Error in vfile_normalize (3)")
       call vstring_free ( ftail )
       !
       call logmsg ( "Test : file normalize" )
       ftail = vfile_tail ( "C:/foo/faa/" )
       call assertVstring_charstring ( ftail , "faa" , "Error in vfile_normalize (3)")
       call vstring_free ( ftail )
    endif
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
    !
    ! Test #15 : add extension
    !
    call logmsg ( "Test #15 : add extension (1)" )
    call vstring_new ( source_file1 , "declaration" )
    fullfilename = vfile_add_extension ( source_file1 , ".txt" )
    call assertVstring_charstring ( fullfilename , "declaration.txt" , "Error in add extension (1).")
    call vstring_free ( source_file1 )
    call vstring_free ( fullfilename )
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
    !
    ! Test #16 : add extension to a file with allready one dot at the end
    !
    call logmsg ( "Test #16 : add extension (2)" )
    call vstring_new ( source_file1 , "declaration" )
    fullfilename = vfile_add_extension ( source_file1 , ".txt" )
    call assertVstring_charstring ( fullfilename , "declaration.txt" , "Error in add extension (2).")
    call vstring_free ( source_file1 )
    call vstring_free ( fullfilename )
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
  end subroutine test_file_dirname
  !
  ! Test temporary files, directory
  !
  subroutine test_file_tempfiledir ()
  logical :: fexist
  type ( t_vstring ) :: tempfile
  type ( t_vstring ) :: tempfile2
  type ( t_vstring ) :: tmpdir
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
    !
    ! Test #18 : get temporary directory
    !
    call logmsg ( "Test #18 : temporary directory" )
    tmpdir = vfile_tempdir ( )
    call logmsg ( "Temporary directory :" )
    call logmsg_string ( tmpdir )
    call vstring_free ( tmpdir )
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
    !
    ! Test #21 : compute a temporary file name
    !
    call logmsg ( "Test #21 : computes a temporary file name" )
    ! Call #1
    tempfile = vfile_tempfile (  )
    fexist = vfile_exists ( tempfile )
    call assert ( fexist , "Error in vfile_tempfile (1)")
    ! Call #2
    tempfile2 = vfile_tempfile (  )
    fexist = vfile_exists ( tempfile2 )
    call assert ( fexist , "Error in vfile_tempfile (2)")
    ! Call #3
    call assertVstring_vstring_different ( tempfile , tempfile2 , "Error in vfile_tempfile (3)")
    call vfile_delete ( tempfile )
    call vfile_delete ( tempfile2 )
    call vstring_free ( tempfile )
    call vstring_free ( tempfile2 )
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
  end subroutine test_file_tempfiledir
  !
  ! Test the copy/delete
  !
  subroutine test_file_copy ()
    logical :: fexist
    type ( t_vstring ) :: source_file1
    integer :: status
    integer :: atime
    integer :: atime1
    integer :: atime2
    integer, parameter :: MSG_LEN = 500
    character (len= MSG_LEN ) :: msg
    integer :: mtime1
    integer :: mtime2
    type ( t_vstring ) :: target_file
    type ( t_vstring ) :: tempfile
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
    !
    ! Test #3
    !
    call logmsg ( "Test #3 : copy ascii" )
    call vstring_new ( source_file1 , "declaration.txt" )
    call vfile_copy ( source_file1 , "declaration_copy.txt" , status , mode = "ascii" )
    call assert ( status==VFILE_ERROR_OK , "Error while copying the file (3)")
    fexist = vfile_exists ("declaration_copy.txt")
    call assert ( fexist , "File copy does not exist (3)")
    call vfile_delete ( "declaration_copy.txt" )
    fexist = vfile_exists ("declaration_copy.txt")
    call assert ( .NOT.fexist , "File copy exists (6)")
    call vstring_free ( source_file1 )
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
    !
    ! Test #3.2 with errors
    !
    call logmsg ( "Test #3 : copy ascii with error" )
    call vstring_new ( source_file1 , "foofoo" )
    call vfile_copy ( source_file1 , "declaration_copy.txt" , status , mode = "ascii" )
    call assert ( status== VFILE_ERROR_SOURCE_FILE_DOES_NOT_EXIST , "Error while copying the file (3)")
    fexist = vfile_exists ("declaration_copy.txt")
    call assert ( .NOT.fexist , "File copy exists (6)")
    call vstring_free ( source_file1 )
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
    !
    ! Test #4
    !
    call logmsg ( "Test #4 : copy based on system" )
    call vstring_new ( source_file1 , "declaration.txt" )
    call vfile_copy ( source_file1 , "declaration_copy.txt" , status )
    fexist = vfile_exists ("declaration_copy.txt")
    call assert ( fexist , "File copy does not exist (4)")
    call assert ( status==VFILE_ERROR_OK , "Error while copying the file (4)")
    call vfile_delete ( "declaration_copy.txt" )
    fexist = vfile_exists ("declaration_copy.txt")
    call assert ( .NOT.fexist , "File copy exists (6)")
    call vstring_free ( source_file1 )
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
    !
    ! Test #5
    ! Prove that one can copy several times the same file with vfile_copy,
    ! therefore overwriting the same file the second time
    !
    call logmsg ( "Test #5 : copy with overwrite" )
    ! 1st copy
    call vstring_new ( source_file1 , "declaration.txt" )
    call vfile_copy ( source_file1 , "declaration_copy.txt" , status )
    fexist = vfile_exists ("declaration_copy.txt")
    call assert ( fexist , "File copy does not exist (5)")
    call assert ( status==VFILE_ERROR_OK , "Error while copying the file (5)")
    ! 2nd copy
    call vfile_copy ( source_file1 , "declaration_copy.txt" , status )
    fexist = vfile_exists ("declaration_copy.txt")
    call assert ( fexist , "File copy does not exist (5)")
    call assert ( status==VFILE_ERROR_OK , "Error while copying the file (5)")
    ! Delete the file
    call vfile_delete ( "declaration_copy.txt" )
    fexist = vfile_exists ("declaration_copy.txt")
    call assert ( .NOT.fexist , "File copy exists (6)")
    call vstring_free ( source_file1 )
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
    !
    ! Copy with an error because target allready exist.
    !
    call vstring_new ( source_file1 , "declaration.txt" )
    call vfile_copy ( source_file1 , "declaration2.txt" , status , mode = "ascii" )
    call assert ( status== VFILE_ERROR_UNABLE_TO_OPEN_TARGET , "Error while copying the file (5)")
    fexist = vfile_exists ( "declaration.txt" )
    call assert ( fexist , "File copy exists (6)")
    fexist = vfile_exists ( "declaration2.txt" )
    call assert ( fexist , "File copy exists (6)")
    call vstring_free ( source_file1 )
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
    !
    ! Test #10
    ! Try to force a copy.
    !
    call logmsg ( "Test #10 : copy -force" )
    call vstring_new ( source_file1 , "declaration.txt" )
    call vfile_copy ( source_file1 , "declaration_copy.txt" , mode = "ascii" )
    call vfile_copy ( source_file1 , "declaration_copy.txt" , status = status , force = .true. , mode = "ascii" )
    fexist = vfile_exists ("declaration_copy.txt")
    call assert ( fexist , "File copy does not exist (10)")
    call assert ( status==VFILE_ERROR_OK , "Error while copying the file (10)")
    call vfile_delete ( "declaration_copy.txt" )
    call vstring_free ( source_file1 )
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
    !
    ! Test #8
    !
    call logmsg ( "Test #8 : rename" )
    call vstring_new ( source_file1 , "declaration.txt" )
    call vstring_new ( target_file , "declaration_copy.txt" )
    call vfile_rename ( source_file1 , target_file , status )
    call assert ( status==VFILE_ERROR_OK , "Error while renaming file (8).")
    fexist = vfile_exists ( "declaration_copy.txt" )
    call assert ( fexist , "File renamed does not exist (8)")
    ! Rename back to the old name
    call vfile_rename ( target_file , source_file1 , status )
    call assert ( status==VFILE_ERROR_OK , "Error while renaming file (8).")
    call vstring_free ( source_file1 )
    call vstring_free ( target_file )
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
    !
    ! Test #22 : touch
    !
    call logmsg ( "Test #22 : touch files" )
    tempfile = vfile_tempfile (  )
    atime1 = vfile_atime ( tempfile )
    mtime1 = vfile_mtime ( tempfile )
    call vfile_touch ( tempfile )
    atime2 = vfile_atime ( tempfile )
    mtime2 = vfile_mtime ( tempfile )
    ! TODO : fix that the windows subroutine does not allow to update the access or modification time.
    !call assert ( atime2 > atime1 , "Error in vfile_touch (1)")
    !call assert ( mtime2 > mtime1 , "Error in vfile_touch (2)")
    call vfile_delete ( tempfile )
    call vstring_free ( tempfile )
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
    !
    ! Test #9
    !
    call logmsg ( "Test #9 : atime" )
    call logmsg ( "File :" )
    call logmsg ( "declaration.txt" )
    atime = vfile_atime ( "declaration.txt" )
    write ( msg ,*)  "Atime :", atime
    call logmsg ( msg )
    call assert ( atime>=0 , "Error while getting acces time (9).")
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
  end subroutine test_file_copy
  !
  ! Test vfile_normalize, native name, path type, volumes
  !
  subroutine test_vfile_normalize ()
    type ( t_vstring ) :: tempfile
    type ( t_vstring ) :: tempfile2
    type ( t_vstring ) :: cwd
    type ( t_vstring ) :: dirname1
    type ( t_vstring ) :: dirname2
    type ( t_vstring ) :: dirname3
    type ( t_vstring ) :: dirtail
    type ( t_vstring ) :: computed
    type ( t_vstring ) :: expected
    integer :: current_platform
    integer :: pathtype
    integer :: firstdotdot
    type ( t_vstringlist ) :: listofvolumes
    integer :: volumefound
    integer :: nbvolumes
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
    !
    ! Get list of volumes
    ! Note:
    ! On windows, this generates an interactive error while the command vfile_isdirectory
    ! tries to access to the "A:/" volume !
    !
    listofvolumes = vfile_volumes ()
    nbvolumes = vstrlist_length ( listofvolumes )
    call assert ( nbvolumes > 1 , "Error in vfile_volumes (1)")
    current_platform = platform_get_platform ()
    if (current_platform==PLATFORM_PLATFORM_WINDOWS) then
       volumefound = vstrlist_search ( listofvolumes , "C:/" )
       call assert ( volumefound /= 0 , "Error in vfile_volumes (5)")

    elseif (current_platform==PLATFORM_PLATFORM_UNIX) then
       call assert ( nbvolumes == 1 , "Error in vfile_volumes (1)")
       volumefound = vstrlist_search ( listofvolumes , "/" )
       call assert ( volumefound /= 0 , "Error in vfile_volumes (5)")
    else
       ! What to tests ?
    endif
    call vstrlist_free ( listofvolumes )
    !
    ! Test #31 : compute the path type of one file
    !
    call logmsg ( "Test #31 : vfile_pathtype" )
    pathtype = vfile_pathtype ( "./toto.txt" )
    call assert ( pathtype==VFILE_PATHTYPE_RELATIVE , "Error in vfile_pathtype (1)")
    pathtype = vfile_pathtype ( "toto.txt" )
    call assert ( pathtype==VFILE_PATHTYPE_RELATIVE , "Error in vfile_pathtype (2)")
    pathtype = vfile_pathtype ( "toto/toto.txt" )
    call assert ( pathtype==VFILE_PATHTYPE_RELATIVE , "Error in vfile_pathtype (2)")
    pathtype = vfile_pathtype ( "../toto.txt" )
    call assert ( pathtype==VFILE_PATHTYPE_RELATIVE , "Error in vfile_pathtype (3)")
    if (current_platform==PLATFORM_PLATFORM_WINDOWS) then
       pathtype = vfile_pathtype ( "C:/toto.txt" )
       call assert ( pathtype==VFILE_PATHTYPE_ABSOLUTE , "Error in vfile_pathtype (3)")
    endif
    tempfile = vfile_tempfile (  )
    pathtype = vfile_pathtype ( tempfile )
    call assert ( pathtype==VFILE_PATHTYPE_ABSOLUTE , "Error in vfile_pathtype (4)")
    call vfile_delete ( tempfile )
    pathtype = vfile_pathtype ( "/toto.txt" )
    current_platform = platform_get_platform ()
    if (current_platform==PLATFORM_PLATFORM_WINDOWS) then
       call assert ( pathtype==VFILE_PATHTYPE_VOLUMERELATIVE , "Error in vfile_pathtype (5)")
    else
       call assert ( pathtype==VFILE_PATHTYPE_ABSOLUTE , "Error in vfile_pathtype (6)")
    endif
    call vstring_free ( tempfile )
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
    !
    ! Test #17 : normalize
    !
    call logmsg ( "Test #17 : normalize" )
    computed = vfile_normalize ( "declaration.txt" )
    call vfile_pwd (cwd)
    expected = vfile_join ( cwd , "declaration.txt" )
    call assertVstring_vstring ( expected, computed , "Error in normalize.")
    call vstring_free ( computed )
    call vstring_free ( cwd )
    call vstring_free ( expected )
    !
    ! Test : file normalize with full path
    ! There is nothing to do in normalize, since the path is absolute.
    !
    call logmsg ( "Test : file normalize" )
    tempfile = vfile_tempfile (  )
    tempfile2 = vfile_normalize ( tempfile )
    call assertVstring_vstring ( tempfile , tempfile2 , "Error in vfile_normalize (2)")
    call vstring_free ( tempfile )
    call vstring_free ( tempfile2 )
    !
    ! Test : file normalize with relative path
    !
    call logmsg ( "Test : file normalize" )
    call vfile_pwd (cwd)
    call logmsg ( "Current working directory:" )
    call logmsg_string ( cwd )
    dirname1 = vfile_join ( cwd , ".." )
    dirname2 = vfile_normalize ( ".." )
    dirtail = vfile_tail ( dirname2 )
    call assertVstring_charstring ( dirtail , "tests" , "Error in vfile_normalize (3)")
    call vstring_free ( cwd )
    call vstring_free ( dirname1 )
    call vstring_free ( dirname2 )
    call vstring_free ( dirtail )
    !
    ! Test : file normalize with relative path and . and "/"
    !
    call logmsg ( "Test : file normalize" )
    call vfile_pwd (cwd)
    expected = vfile_join ( cwd , "declaration.txt" )
    computed = vfile_normalize ( "./declaration.txt" )
    call assertVstring_vstring ( computed , expected , "Error in vfile_normalize (3)")
    call vstring_free ( cwd )
    call vstring_free ( expected )
    call vstring_free ( computed )
    !
    ! Test : file normalize with relative path .. and ..
    !
    call logmsg ( "Test : file normalize" )
    call vfile_pwd (cwd)
    dirname1 = vfile_join ( cwd , ".." )
    dirname2 = vfile_join ( dirname1 , ".." )
    dirname3 = vfile_join ( dirname2 , "src" )
    computed = vfile_normalize ( dirname3 )
    dirtail = vfile_tail ( computed )
    call assertVstring_charstring ( dirtail , "src" , "Error in vfile_normalize (3)")
    firstdotdot = vstring_first ( computed , ".." )
    call assert ( firstdotdot == 0, "Error in vfile_normalize (3)")
    call vstring_free ( cwd )
    call vstring_free ( dirname1 )
    call vstring_free ( dirname2 )
    call vstring_free ( dirname3 )
    call vstring_free ( computed )
    call vstring_free ( dirtail )
    current_platform = platform_get_platform ()
    if (current_platform==PLATFORM_PLATFORM_WINDOWS) then
       !
       ! Test : file normalize with a volume relative file unit
       !
       ! Store the current path and go to C:/
       call vfile_pwd (cwd)
       call vstrplatform_cd ( "C:/" )
       call logmsg ( "Test : file normalize" )
       computed = vfile_normalize ( "/file/toto/titi" )
       call assertVstring_charstring ( computed , "C:/file/toto/titi" , "Error in vfile_normalize (3)")
       call vstring_free ( computed )
       ! Go back to the original directory
       call vstrplatform_cd ( cwd )
       call vstring_free ( cwd )
       !
       ! Test : file normalize with a volume relative file unit
       !
       ! Store the current path and go to C:/
       call vfile_pwd (cwd)
       call vstrplatform_cd ( "C:/" )
       call logmsg ( "Test : file normalize" )
       computed = vfile_normalize ( "/" )
       call assertVstring_charstring ( computed , "C:/" , "Error in vfile_normalize (3)")
       call vstring_free ( computed )
       ! Go back to the original directory
       call vstrplatform_cd ( cwd )
       call vstring_free ( cwd )
    endif
    !
    ! Test #30 : compute the native name
    ! Caution !
    !   The expected string contains a backslash.
    !   To prevent gcc-style compilers (e.g. gfortran)
    !   to process to backslash substitution, use "-fno-backslash" option.
    !
    call logmsg ( "Test #30 : vfile_nativename" )
    tempfile = vfile_nativename ( "./toto.txt" )
    current_platform = platform_get_platform ()
    if (current_platform==PLATFORM_PLATFORM_WINDOWS) then
       call assertVstring_charstring ( tempfile , ".\toto.txt" , "Error in vfile_nativename (1)")
    else
       call assertVstring_charstring ( tempfile , "./toto.txt" , "Error in vfile_nativename (1)")
    endif
    call vstring_free ( tempfile )
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
  end subroutine test_vfile_normalize
  !
  ! Test find by pattern
  !
  subroutine test_vfile_findbypattern ()
    type ( t_vstringlist ) :: listOfFiles
    type ( t_vstring ) :: normalized
    integer, parameter :: MSG_LEN = 500
    integer :: numberOfFiles
    type ( t_vstringlist ) :: filetypes
    type ( t_vstringlist ) :: expectedlist
    type ( t_vstring ) :: computedstring
    integer :: expectedlength
    integer :: computedlength
    external testfiltercmd_vstring
    logical :: testfiltercmd_vstring
    call logmsg ( "Entering test_vfile_findbypattern..." )
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
    !
    ! Test : split a file name
    !
#ifndef _IVF8
    ! This test does not pass with allocatable strings and intel fortran 8 (
    ! The test pass with with allocatable strings and Intel Fortran 10
    call logmsg ( "Split a file name" )
    normalized = vfile_normalize ( "declaration.txt" )
    listOfFiles = vfile_split ( normalized )
    numberOfFiles = vstrlist_length ( listOfFiles )
    call assertVstring_charstringindex ( listOfFiles , numberOfFiles , "declaration.txt" , "Error in vfile_findbypattern")
    call assertVstring_charstringindex ( listOfFiles , numberOfFiles - 1, "filedir" , "Error in vfile_findbypattern")
    call assertVstring_charstringindex ( listOfFiles , numberOfFiles - 2 , "tests" , "Error in vfile_findbypattern")
    call vstrlist_free ( listOfFiles )
    call vstring_free ( normalized )
#endif
    !
    ! Test : split a file name with a mix of \ and /
    !
#ifndef _IVF8
    ! This test does not pass with allocatable strings and intel fortran 8
    ! The test pass with with allocatable strings and Intel Fortran 10
    call logmsg ( "Test : split a file name with a mix of \ and /" )
    call vstring_new ( normalized , "titi/toto\declaration.txt" )
    listOfFiles = vfile_split ( normalized )
    numberOfFiles = vstrlist_length ( listOfFiles )
    call assert ( numberOfFiles == 3 , "Error in vfile_findbypattern.")
    call assertVstring_charstringindex ( listOfFiles , 1 , "titi" , "Error in vfile_split")
    call assertVstring_charstringindex ( listOfFiles , 2, "toto" , "Error in vfile_split")
    call assertVstring_charstringindex ( listOfFiles , 3 , "declaration.txt" , "Error in vfile_split")
    call vstrlist_free ( listOfFiles )
    call vstring_free ( normalized )
#endif
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
    !
    ! List files in current directory
    !
    call logmsg ( "List files" )
    call vstrplatform_cd ( "testls" )
    listOfFiles = vfile_listfiles ( )
    call vstrlist_new ( expectedlist )
    call vstrlist_append ( expectedlist , "declaration.txt" )
    call vstrlist_append ( expectedlist , "declaration2.txt" )
    call checklist ( expectedlist , listOfFiles )
    call vstrlist_free ( expectedlist )
    call vstrlist_free ( listOfFiles )
    call vstrplatform_cd ( ".." )
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
    !
    ! List files in given directory
    !
    call logmsg ( "List files" )
    listOfFiles = vfile_listfiles ( "testls" )
    call vstrlist_new ( expectedlist )
    call vstrlist_append ( expectedlist , "testls/declaration.txt" )
    call vstrlist_append ( expectedlist , "testls/declaration2.txt" )
    call checklist ( expectedlist , listOfFiles )
    call vstrlist_free ( expectedlist )
    call vstrlist_free ( listOfFiles )
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
    !
    ! List files in given directory, but only files
    !
    call logmsg ( "List files" )
    call vstrlist_new ( filetypes )
    call vstrlist_append ( filetypes , "f" )
    listOfFiles = vfile_listfiles ( "testls2" , filetypes = filetypes )
    call vstrlist_new ( expectedlist )
    call vstrlist_append ( expectedlist , "testls2/declaration.txt" )
    call vstrlist_append ( expectedlist , "testls2/declaration2.txt" )
    call checklist ( expectedlist , listOfFiles )
    call vstrlist_free ( expectedlist )
    call vstrlist_free ( listOfFiles )
    call vstrlist_free ( filetypes )
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
    !
    ! List files in given directory, but only directories
    !
    call logmsg ( "List files" )
    call vstrlist_new ( filetypes )
    call vstrlist_append ( filetypes , "d" )
    listOfFiles = vfile_listfiles ( "testls2" , filetypes = filetypes )
    call vstrlist_new ( expectedlist )
    call vstrlist_append ( expectedlist , "testls2/subdir1" )
    call vstrlist_append ( expectedlist , "testls2/subdir2" )
    call checklist ( expectedlist , listOfFiles )
    call vstrlist_free ( expectedlist )
    call vstrlist_free ( listOfFiles )
    call vstrlist_free ( filetypes )
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
    !
    ! List files in given directory, with complex string matching
    !
    call logmsg ( "List files" )
    call vstrlist_new ( filetypes )
    listOfFiles = vfile_listfiles ( "testls3" , pattern = "[a-m]*" )
    numberOfFiles = vstrlist_length ( listOfFiles )
    call assert ( numberOfFiles == 0 , "Error in vfile_listfiles.")
    call vstrlist_free ( filetypes )
    call vstrlist_free ( listOfFiles )
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
    !
    ! List files in given directory, with complex string matching
    !
    call logmsg ( "List files" )
    call vstrlist_new ( filetypes )
    listOfFiles = vfile_listfiles ( "testls3" , pattern = "testls3/[a-m]*" )
    call vstrlist_new ( expectedlist )
    call vstrlist_append ( expectedlist , "testls3/adir" )
    call vstrlist_append ( expectedlist , "testls3/afile.txt" )
    call vstrlist_append ( expectedlist , "testls3/mdir" )
    call vstrlist_append ( expectedlist , "testls3/mfile.txt" )
    call checklist ( expectedlist , listOfFiles )
    call vstrlist_free ( expectedlist )
    call vstrlist_free ( filetypes )
    call vstrlist_free ( listOfFiles )
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
    !
    ! List files in given directory, with tails option
    !
    call logmsg ( "List files" )
    listOfFiles = vfile_listfiles ( "testls" , tails = .true. )
    call vstrlist_new ( expectedlist )
    call vstrlist_append ( expectedlist , "declaration.txt" )
    call vstrlist_append ( expectedlist , "declaration2.txt" )
    call checklist ( expectedlist , listOfFiles )
    call vstrlist_free ( expectedlist )
    call vstrlist_free ( listOfFiles )
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
    !
    ! Test find files
    !
    call logmsg ( "Find files" )
    listOfFiles = vfile_find ( "testls" )
    call logmsg ( "Found:" )
    computedstring = vstrlist_join ( listOfFiles )
    call logmsg_string ( computedstring )
    call vstring_free ( computedstring )
    call vstrlist_new ( expectedlist )
    call vstrlist_append ( expectedlist , "testls/declaration.txt" )
    call vstrlist_append ( expectedlist , "testls/declaration2.txt" )
    call checklist ( expectedlist , listOfFiles )
    call vstrlist_free ( expectedlist )
    call vstrlist_free ( listOfFiles )
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
    !
    listOfFiles = vfile_find ( "testls4" )
    call logmsg ( "Found:" )
    computedstring = vstrlist_join ( listOfFiles )
    call logmsg_string ( computedstring )
    call vstring_free ( computedstring )
    call vstrlist_new ( expectedlist )
    call vstrlist_append ( expectedlist , "testls4/afile.txt" )
    call vstrlist_append ( expectedlist , "testls4/mfile.txt" )
    call vstrlist_append ( expectedlist , "testls4/zfile.txt" )
    call vstrlist_append ( expectedlist , "testls4/adir" )
    call vstrlist_append ( expectedlist , "testls4/adir/afile.txt" )
    call vstrlist_append ( expectedlist , "testls4/adir/mfile.txt" )
    call vstrlist_append ( expectedlist , "testls4/adir/zfile.txt" )
    call vstrlist_append ( expectedlist , "testls4/mdir" )
    call vstrlist_append ( expectedlist , "testls4/mdir/afile.txt" )
    call vstrlist_append ( expectedlist , "testls4/mdir/mfile.txt" )
    call vstrlist_append ( expectedlist , "testls4/mdir/zfile.txt" )
    call vstrlist_append ( expectedlist , "testls4/zdir" )
    call vstrlist_append ( expectedlist , "testls4/zdir/afile.txt" )
    call vstrlist_append ( expectedlist , "testls4/zdir/mfile.txt" )
    call vstrlist_append ( expectedlist , "testls4/zdir/zfile.txt" )
    call checklist ( expectedlist , listOfFiles )
    call vstrlist_free ( listOfFiles )
    call vstrlist_free ( expectedlist )
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
    !
    listOfFiles = vfile_find ( "testls4" , testfiltercmd_vstring )
    call logmsg ( "Found:" )
    computedstring = vstrlist_join ( listOfFiles )
    call logmsg_string ( computedstring )
    call vstring_free ( computedstring )
    call vstrlist_new ( expectedlist )
    call vstrlist_append ( expectedlist , "testls4/afile.txt" )
    call vstrlist_append ( expectedlist , "testls4/mfile.txt" )
    call vstrlist_append ( expectedlist , "testls4/adir" )
    call vstrlist_append ( expectedlist , "testls4/adir/afile.txt" )
    call vstrlist_append ( expectedlist , "testls4/adir/mfile.txt" )
    call vstrlist_append ( expectedlist , "testls4/mdir" )
    call vstrlist_append ( expectedlist , "testls4/mdir/afile.txt" )
    call vstrlist_append ( expectedlist , "testls4/mdir/mfile.txt" )
    call vstrlist_append ( expectedlist , "testls4/zdir/afile.txt" )
    call vstrlist_append ( expectedlist , "testls4/zdir/mfile.txt" )
    call checklist ( expectedlist , listOfFiles )
    call vstrlist_free ( listOfFiles )
    call vstrlist_free ( expectedlist )
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
#ifndef _IVF8
    ! This test does not pass with allocatable strings and intel fortran 8
    ! The test pass with with allocatable strings and Intel Fortran 10
    !
    call logmsg ( "Find without filter" )
    listOfFiles = vfile_find ( "testfind" )
    call logmsg ( "Found:" )
    computedstring = vstrlist_join ( listOfFiles )
    call logmsg_string ( computedstring )
    call vstring_free ( computedstring )
    expectedlength = 63
    computedlength = vstrlist_length ( listOfFiles )
    call assert ( expectedlength == computedlength , "Error in vfile_join : the lists do not have the same length.")
    call vstrlist_new ( expectedlist )
    call vstrlist_append ( expectedlist , "testfind/adir")
    call vstrlist_append ( expectedlist , "testfind/adir/afile.txt")
    call vstrlist_append ( expectedlist , "testfind/adir/mfile.txt")
    call vstrlist_append ( expectedlist , "testfind/adir/subdir")
    call vstrlist_append ( expectedlist , "testfind/adir/subdir/adir")
    call vstrlist_append ( expectedlist , "testfind/adir/subdir/adir/afile.txt")
    call vstrlist_append ( expectedlist , "testfind/adir/subdir/adir/mfile.txt")
    call vstrlist_append ( expectedlist , "testfind/adir/subdir/adir/zfile.txt")
    call vstrlist_append ( expectedlist , "testfind/adir/subdir/afile.txt")
    call vstrlist_append ( expectedlist , "testfind/adir/subdir/mdir")
    call vstrlist_append ( expectedlist , "testfind/adir/subdir/mdir/afile.txt")
    call vstrlist_append ( expectedlist , "testfind/adir/subdir/mdir/mfile.txt")
    call vstrlist_append ( expectedlist , "testfind/adir/subdir/mdir/zfile.txt")
    call vstrlist_append ( expectedlist , "testfind/adir/subdir/mfile.txt")
    call vstrlist_append ( expectedlist , "testfind/adir/subdir/zdir")
    call vstrlist_append ( expectedlist , "testfind/adir/subdir/zdir/afile.txt")
    call vstrlist_append ( expectedlist , "testfind/adir/subdir/zdir/mfile.txt")
    call vstrlist_append ( expectedlist , "testfind/adir/subdir/zdir/zfile.txt")
    call vstrlist_append ( expectedlist , "testfind/adir/subdir/zfile.txt")
    call vstrlist_append ( expectedlist , "testfind/adir/zfile.txt")
    call vstrlist_append ( expectedlist , "testfind/afile.txt")
    call vstrlist_append ( expectedlist , "testfind/mdir")
    call vstrlist_append ( expectedlist , "testfind/mdir/afile.txt")
    call vstrlist_append ( expectedlist , "testfind/mdir/mfile.txt")
    call vstrlist_append ( expectedlist , "testfind/mdir/subdir")
    call vstrlist_append ( expectedlist , "testfind/mdir/subdir/adir")
    call vstrlist_append ( expectedlist , "testfind/mdir/subdir/adir/afile.txt")
    call vstrlist_append ( expectedlist , "testfind/mdir/subdir/adir/mfile.txt")
    call vstrlist_append ( expectedlist , "testfind/mdir/subdir/adir/zfile.txt")
    call vstrlist_append ( expectedlist , "testfind/mdir/subdir/afile.txt")
    call vstrlist_append ( expectedlist , "testfind/mdir/subdir/mdir")
    call vstrlist_append ( expectedlist , "testfind/mdir/subdir/mdir/afile.txt")
    call vstrlist_append ( expectedlist , "testfind/mdir/subdir/mdir/mfile.txt")
    call vstrlist_append ( expectedlist , "testfind/mdir/subdir/mdir/zfile.txt")
    call vstrlist_append ( expectedlist , "testfind/mdir/subdir/mfile.txt")
    call vstrlist_append ( expectedlist , "testfind/mdir/subdir/zdir")
    call vstrlist_append ( expectedlist , "testfind/mdir/subdir/zdir/afile.txt")
    call vstrlist_append ( expectedlist , "testfind/mdir/subdir/zdir/mfile.txt")
    call vstrlist_append ( expectedlist , "testfind/mdir/subdir/zdir/zfile.txt")
    call vstrlist_append ( expectedlist , "testfind/mdir/subdir/zfile.txt")
    call vstrlist_append ( expectedlist , "testfind/mdir/zfile.txt")
    call vstrlist_append ( expectedlist , "testfind/mfile.txt")
    call vstrlist_append ( expectedlist , "testfind/zdir")
    call vstrlist_append ( expectedlist , "testfind/zdir/afile.txt")
    call vstrlist_append ( expectedlist , "testfind/zdir/mfile.txt")
    call vstrlist_append ( expectedlist , "testfind/zdir/subdir")
    call vstrlist_append ( expectedlist , "testfind/zdir/subdir/adir")
    call vstrlist_append ( expectedlist , "testfind/zdir/subdir/adir/afile.txt")
    call vstrlist_append ( expectedlist , "testfind/zdir/subdir/adir/mfile.txt")
    call vstrlist_append ( expectedlist , "testfind/zdir/subdir/adir/zfile.txt")
    call vstrlist_append ( expectedlist , "testfind/zdir/subdir/afile.txt")
    call vstrlist_append ( expectedlist , "testfind/zdir/subdir/mdir")
    call vstrlist_append ( expectedlist , "testfind/zdir/subdir/mdir/afile.txt")
    call vstrlist_append ( expectedlist , "testfind/zdir/subdir/mdir/mfile.txt")
    call vstrlist_append ( expectedlist , "testfind/zdir/subdir/mdir/zfile.txt")
    call vstrlist_append ( expectedlist , "testfind/zdir/subdir/mfile.txt")
    call vstrlist_append ( expectedlist , "testfind/zdir/subdir/zdir")
    call vstrlist_append ( expectedlist , "testfind/zdir/subdir/zdir/afile.txt")
    call vstrlist_append ( expectedlist , "testfind/zdir/subdir/zdir/mfile.txt")
    call vstrlist_append ( expectedlist , "testfind/zdir/subdir/zdir/zfile.txt")
    call vstrlist_append ( expectedlist , "testfind/zdir/subdir/zfile.txt")
    call vstrlist_append ( expectedlist , "testfind/zdir/zfile.txt")
    call vstrlist_append ( expectedlist , "testfind/zfile.txt")
    call checklist ( expectedlist , listOfFiles )
    call vstrlist_free ( listOfFiles )
    call vstrlist_free ( expectedlist )
#endif
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
    ! TODO : test with g95
#ifndef _IVF8
    ! This test does not pass with allocatable strings and intel fortran 8
    ! The test pass with with allocatable strings and Intel Fortran 10
    !
    call logmsg ( "Find files by pattern (1)" )
    listOfFiles = vfile_findbypattern ( "testfindbypattern" , pattern = "*dec*.txt" )
    call logmsg ( "Found:" )
    computedstring = vstrlist_join ( listOfFiles )
    call logmsg_string ( computedstring )
    call vstring_free ( computedstring )
    call vstrlist_new ( expectedlist )
    call vstrlist_append ( expectedlist , "testfindbypattern/subdir/declaration.txt" )
    call vstrlist_append ( expectedlist , "testfindbypattern/subdir/declaration2.txt" )
    call checklist ( expectedlist , listOfFiles )
    call vstrlist_free ( expectedlist )
    call vstrlist_free ( listOfFiles )
    !
    call logmsg ( "Find files by pattern when no directory is given" )
    call vstrplatform_cd ( "testfindbypattern" )
    listOfFiles = vfile_findbypattern ( pattern = "*dec*.txt" )
    call logmsg ( "Found:" )
    computedstring = vstrlist_join ( listOfFiles )
    call logmsg_string ( computedstring )
    call vstring_free ( computedstring )
    call vstrlist_new ( expectedlist )
    call vstrlist_append ( expectedlist , "./subdir/declaration.txt" )
    call vstrlist_append ( expectedlist , "./subdir/declaration2.txt" )
    call checklist ( expectedlist , listOfFiles )
    call vstrlist_free ( expectedlist )
    call vstrlist_free ( listOfFiles )
    call vstrplatform_cd ( ".." )
#endif
    !
    ! Check the invariants of the program
    !
    call test_checkinvariants ()
  end subroutine test_vfile_findbypattern
  !
  ! Check that the content of the expected list of the one of the computed list,
  ! no matter of the order.
  !
  subroutine checklist ( expectedlist , computedlist  )
    type ( t_vstringlist ) , intent(in) :: expectedlist
    type ( t_vstringlist ) , intent(in) :: computedlist
    type ( t_vstring ) :: expecteditem
    integer :: itemindex
    integer :: expectedlength
    integer :: computedlength
    integer :: indexfound
    expectedlength = vstrlist_length ( expectedlist )
    computedlength = vstrlist_length ( computedlist )
    call assert ( expectedlength == computedlength , "Error in checklist : the lists do not have the same length.")
    do itemindex = 1 , expectedlength
       expecteditem = vstrlist_index ( expectedlist , itemindex )
       call logmsg ( "Expected string :" )
       call logmsg_string ( expecteditem )
       indexfound = vstrlist_search ( computedlist , expecteditem )
       call assert ( indexfound /= 0 , "Error in checklist : the expected item is not in the computed list.")
       call vstring_free ( expecteditem )
    enddo
  end subroutine checklist
  !
  ! check_fileunitsnb --
  !   Check that the number of opened units is OK
  !
  subroutine check_fileunitsnb ()
    integer :: nbunits
    integer , dimension(:) , pointer :: units
    call fileunit_getallopen ( nbunits , units )
    if ( nbunits /= 1 ) then
       call fileunit_displayopen ( log_unit )
    endif
    call assert ( nbunits == 1 , "Wrong number of units")
    deallocate ( units )
  end subroutine check_fileunitsnb
  !
  ! logmsg_string --
  !   Logs a vstring.
  !
  subroutine logmsg_string ( message )
    type ( t_vstring ) , intent(in) :: message
    !    character(len=vstring_length(message)) :: message_charstring
    character(len=10000) :: message_charstring
    call vstring_cast ( message , message_charstring )
    call logmsg ( message_charstring )
  end subroutine logmsg_string
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
  !
  ! assertVstring_vstringindex --
  !   Check that the vstring at index #icomponent of the given vstring list
  !   is equal to the expected character string and updates the assertion system.
  !
  subroutine assertVstring_vstringindex ( list , icomponent , expectedString , message )
    implicit none
    type ( t_vstringlist ), intent(in) :: list
    integer , intent(in) :: icomponent
    type ( t_vstring ), intent(in) :: expectedString
    character(len=*), intent(in) :: message
    type ( t_vstring ) :: computedString
    computedString = vstrlist_index ( list , icomponent )
    call assertVstring_vstring ( computedString , expectedString , message )
    call vstring_free ( computedString )
  end subroutine assertVstring_vstringindex
  !
  ! assertVstring_vstring_different --
  !   Check that the computed vstring is different from the expected string
  !   and updates the assertion system.
  !
  subroutine assertVstring_vstring_different ( computedString , expectedString , message )
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
    if ( equals ) then
       write ( msg , * ) "String expected :-", trim ( char_string ), "-"
       call logmsg ( msg )
       call vstring_cast ( computedString , len ( char_string ) , char_string )
       write ( msg , * ) "String computed :-", trim ( char_string ) , "-"
       call logmsg ( msg )
    endif
    call assert ( .NOT.equals , message )
  end subroutine assertVstring_vstring_different
  !
  ! Check that there are no memory leaks.
  !
  subroutine string_reference_check ()
    implicit none
    integer :: vstring_reference
    character (len=200) :: msg
    ! These are the commands defined statically in m_vfile
    integer , parameter :: reference_strings = 8
    vstring_reference = vstring_reference_get()
    write ( msg , * ) "Number of strings :", vstring_reference
    call logmsg (msg)
    call assert ( vstring_reference == reference_strings, "Wrong number of strings references.")
  end subroutine string_reference_check
end program test_vfile
!
! testfiltercmd_vstring --
!   Command callback passed to vfile_find.
!
function testfiltercmd_vstring ( filename ) result ( keepfile )
  use m_vstring, only : &
       t_vstring, &
       vstring_match , &
       vstring_free
  use m_vfile, only : &
       vfile_tail
  type ( t_vstring ), intent(in) :: filename
  logical :: keepfile
  type ( t_vstring ) :: filetail
  filetail = vfile_tail ( filename )
  keepfile = vstring_match ( filetail , "[a-m]*" )
  call vstring_free ( filetail )
end function testfiltercmd_vstring

