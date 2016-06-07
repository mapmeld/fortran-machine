!
! This program is a testing component for the module m_products.
! Functions tested :
!  public :: filedir_add_extension : OK
!  public :: filedir_atime : OK
!  public :: filedir_tail : OK
!  public :: filedir_copy : OK
!  public :: filedir_copy_std : OK
!  public :: filedir_delete : OK
!  public :: filedir_exists : OK
!  public :: filedir_extension : OK
!  public :: filedir_pwd : OK
!  public :: filedir_get_unit : OK
!  public :: filedir_join : OK
!  public :: filedir_rename : OK
!  public :: filedir_dirname : TODO
!  public :: filedir_rootname : TODO
!  public :: filedir_normalize : EXTREMERLY PARTIALLY
!  public :: filedir_separator : IMPLICITELY
!
program test_filedir
  use m_filedir, only : &
       filedir_rename, &
       filedir_copy, &
       filedir_copy_std, &
       filedir_get_unit, &
       filedir_pwd, &
       filedir_exists, &
       filedir_delete, &
       filedir_atime, &
       filedir_extension, &
       filedir_tail, &
       filedir_join, &
       filedir_add_extension , &
       filedir_normalize, &
       filedir_findByPattern, &
       filedir_tempdir, &
       filedir_init, &
       filedir_split, &
       filedir_tempfile , &
       filedir_touch , &
       filedir_mtime, &
       filedir_isdirectory , &
       filedir_mkdir, &
       FS_PATHTYPE_ABSOLUTE, &
       FS_PATHTYPE_RELATIVE, &
       FS_PATHTYPE_VOLUMERELATIVE , &
       filedir_pathtype, &
       filedir_nativename , &
       FS_ERROR_SOURCE_FILE_DOES_NOT_EXIST , &
       FS_ERROR_UNABLE_TO_OPEN_TARGET , &
       FS_ERROR_OK , &
       filedir_set_stoponerror, &
       filedir_closeallopenunits, &
       filedir_reportunit, &
       filedir_displayopenunits , &
       filedir_getallopenunits

  use m_platform, only : &
       platform_get_platform, &
       PLATFORM_PLATFORM_WINDOWS, &
       PLATFORM_PLATFORM_UNIX, &
       PLATFORM_PLATFORM_MAC, &
       platform_cd
  implicit none
  integer :: assertTotalTestSuccess
  integer :: assertTotalTestFail
  integer :: assertTestIndex
  integer , parameter :: log_unit = 12
  !
  ! Run the tests
  !
  call test_m_filedir_all ()
  
contains
  subroutine test_m_filedir_all ()
    character (len= 100 ) :: cwd
    integer :: file_unit, other_file_unit
    character (len= 200 ), parameter :: source_file1 = "declaration.txt"
    character (len= 200 ), parameter :: target_file = "declaration_copy.txt"
    character (len= 200 ), parameter :: file_without_extension = "declaration"
    character (len= 200 ), parameter :: file_with_dot = "declaration."
    character (len= 200 ), parameter :: inputfilenotexit1 = "foofoo"
    character (len= 200 ), parameter :: source_file2 = "declaration2.txt"
    character (len= 200 ) :: normalized_source_file
    character (len= 200 ) :: result_file_name
    character (len= 200 ) :: computed
    character (len= 200 ) :: expected
    !character(len=50), dimension(:), pointer :: listOfFiles => NULL()
    !character(len=50), dimension(1:50) :: listOfFiles
    character, dimension(:,:), pointer :: listOfFiles => NULL()
    integer                                  :: ifile
    character, dimension(:,:), pointer :: listOfComponents => NULL()
    external :: file_error
    logical :: fexist
    integer :: status
    integer, parameter :: MSG_LEN = 500
    character (len= MSG_LEN ) :: msg
    integer :: atime
    logical :: force
    character (len=10) :: fext
    character (len=20) :: ftail
    character(len=200)             :: tmpdir
    character(len=20)            :: pattern
    integer, parameter :: filelength = 200
    integer :: numberOfFiles
    integer :: numberOfComponents
    integer :: iComponent
    character(len=20)            :: computedComponent
    integer :: numberOfChars
    character(len=200)             :: tempfile
    character(len=200)             :: tempfile2
    integer :: atime1 , atime2
    integer :: mtime1 , mtime2
    character(len=200)             :: dirname1
    character(len=200)             :: dirtail
    character(len=200)             :: dirname2
    logical :: isdir
    character (len= 100 ) :: cwd1
    character (len= 100 ) :: cwd2
    integer :: pathtype
    integer :: current_platform
    integer :: nbunits
    integer , dimension(:) , pointer :: units
    integer :: file_unit1
    integer :: file_unit2
    !
    ! Initialize the test system
    !
    call log_startup ( "test_m_filedir.log" )
    call assert_startup ()
    !
    call filedir_init ()
    call filedir_set_stoponerror ( .false. )
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! test #1 : current working directory
    !
    call logmsg ( "Test #1 : get_cwd" )
    ! Just test that it works
    call filedir_pwd ( cwd )
    write ( msg ,*) "Current working directory:"
    call logmsg ( msg )
    write ( msg ,*) trim(cwd)
    call logmsg ( msg )
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! test #2 : get an unused file unit
    !
    call logmsg ( "Test #2 : get_unit" )
    file_unit = filedir_get_unit (  )
    write ( msg ,*)  "Unused unit:", file_unit
    call logmsg ( msg )
    call assert (file_unit==1, "Wrong file unit. (1)")
    other_file_unit = filedir_get_unit (  )
    call assert (other_file_unit==1, "Wrong file unit. (2)")
    open ( file_unit , file = source_file1 )
    other_file_unit = filedir_get_unit (  )
    write ( msg ,*)  "Unused unit:", other_file_unit
    call logmsg ( msg )
    call assert (other_file_unit==2, "Wrong file unit. (3)")
    close ( file_unit )
    other_file_unit = filedir_get_unit (  )
    call assert (other_file_unit==1, "Wrong file unit. (4)")
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! Test #3
    !
    call logmsg ( "Test #3 : copy_std" )
    call filedir_copy_std ( source_file1 , target_file , status )
    call assert ( status==FS_ERROR_OK , "Error while copying the file (3)")
    fexist = filedir_exists (target_file)
    call assert ( fexist , "File copy does not exist (3)")
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! Test #3.2 with errors
    !
    call filedir_copy_std ( inputfilenotexit1 , target_file , status )
    call assert ( status== FS_ERROR_SOURCE_FILE_DOES_NOT_EXIST , "Error while copying the file (3)")
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! Test #4
    !
    call logmsg ( "Test #4 : copy" )
    call filedir_copy ( source_file1 , target_file , status )
    fexist = filedir_exists (target_file)
    call assert ( fexist , "File copy does not exist (4)")
    call assert ( status==FS_ERROR_OK , "Error while copying the file (4)")
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! Test #5
    ! Prove that one can copy several times the same file with filedir_copy,
    ! therefore overwriting the same file the second time
    !
    call logmsg ( "Test #5 : copy with overwrite" )
    call filedir_copy ( source_file1 , target_file , status )
    fexist = filedir_exists (target_file)
    call assert ( fexist , "File copy does not exist (5)")
    call assert ( status==FS_ERROR_OK , "Error while copying the file (5)")
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! Test #6
    !
    call logmsg ( "Test #6 : delete" )
    call filedir_delete ( target_file )
    fexist = filedir_exists (target_file)
    call assert ( .NOT.fexist , "File copy exists (6)")
    !
    ! Copy with an error because target allready exist.
    !
    call filedir_copy_std ( source_file1 , source_file2 , status )
    call assert ( status== FS_ERROR_UNABLE_TO_OPEN_TARGET , "Error while copying the file (5)")
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! Test #7
    !
    call logmsg ( "Test #7 : copy" )
    call filedir_copy ( source_file1 , target_file , status )
    call filedir_delete ( target_file , status )
    call assert ( status==FS_ERROR_OK , "Error while deleting file (7).")
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! Test #8
    !
    call logmsg ( "Test #8 : rename" )
    call filedir_rename ( source_file1 , target_file )
    call assert ( status==FS_ERROR_OK , "Error while renaming file (8).")
    fexist = filedir_exists ( target_file )
    call assert ( fexist , "File renamed does not exist (8)")
    ! Rename back to the old name
    call filedir_rename ( target_file , source_file1 , status )
    call assert ( status==FS_ERROR_OK , "Error while renaming file (8).")
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! Test #9
    !
    call logmsg ( "Test #9 : atime" )
    write ( msg ,*)  "File :", source_file1
    call logmsg ( msg )
    atime = filedir_atime ( source_file1 )
    write ( msg ,*)  "Atime :", atime
    call logmsg ( msg )
    call assert ( atime>=0 , "Error while getting acces time (9).")
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! Test #10
    ! Try to force a copy.
    !
    call logmsg ( "Test #10 : copy -force" )
    call filedir_copy_std ( source_file1 , target_file )
    force = .true.
    call filedir_copy_std ( source_file1 , target_file , status , force )
    fexist = filedir_exists (target_file)
    call assert ( fexist , "File copy does not exist (10)")
    call assert ( status==FS_ERROR_OK , "Error while copying the file (10)")
    call filedir_delete ( target_file )
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! Test #11 : file extension
    !
    call logmsg ( "Test #11 : file extension (1)" )
    write ( msg ,*)  "File :", source_file1
    call logmsg ( msg )
    fext = filedir_extension ( source_file1 )
    write ( msg ,*)  "Extension :", fext
    call logmsg ( msg )
    call assert ( fext(1:4)==".txt" , "Error in file extension.")
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! Test #12 : file extension, without extension
    !
    call logmsg ( "Test #12 : file extension without extension (2)" )
    write ( msg ,*)  "File :", file_without_extension
    call logmsg ( msg )
    fext = filedir_extension ( file_without_extension )
    write ( msg ,*)  "Extension :", fext
    call logmsg ( msg )
    call assert ( trim(fext)=="" , "Error in file extension (2).")
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! Test #13 : file tail, without dirname
    !
    call logmsg ( "Test #13 : file tail without dirname (1)" )
    write ( msg ,*)  "File :", source_file1
    call logmsg ( msg )
    ftail = filedir_tail ( source_file1 )
    write ( msg ,*)  "Tail :", ftail
    call logmsg ( msg )
    call assert ( trim(ftail)=="declaration.txt" , "Error in file tail (1).")
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! Test #14 : file tail, with dirname (test also filedir_join)
    !
    call logmsg ( "Test #14 : file tail with dirname (2)" )
    normalized_source_file = filedir_join ( cwd , source_file1 )
    write ( msg ,*)  "File :", normalized_source_file
    call logmsg ( msg )
    ftail = filedir_tail ( normalized_source_file )
    write ( msg ,*)  "Tail :", ftail
    call logmsg ( msg )
    call assert ( trim(ftail)=="declaration.txt" , "Error in file tail (1).")
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! Test #15 : add extension
    !
    call logmsg ( "Test #15 : add extension (1)" )
    write ( msg ,*)  "File :", file_without_extension
    call logmsg ( msg )
    result_file_name = filedir_add_extension ( file_without_extension , ".txt" )
    write ( msg ,*)  "With extension :", result_file_name
    call logmsg ( msg )
    call assert ( trim(source_file1)==trim(result_file_name) , "Error in add extension (1).")
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! Test #16 : add extension to a file with allready one dot at the end
    !
    call logmsg ( "Test #16 : add extension (2)" )
    write ( msg ,*)  "File :", file_with_dot
    call logmsg ( msg )
    result_file_name = filedir_add_extension ( file_with_dot , ".txt" )
    write ( msg ,*)  "With extension :", result_file_name
    call logmsg ( msg )
    call assert ( trim(source_file1)==trim(result_file_name) , "Error in add extension (2).")
    !
    ! Test #17 : normalize
    !
    call logmsg ( "Test #17 : normalize" )
    write ( msg ,*)  "File :", source_file1
    call logmsg ( msg )
    computed = filedir_normalize ( source_file1 )
    write ( msg ,*)  "Normalized :", computed
    call logmsg ( msg )
    call filedir_pwd ( cwd )
    expected = filedir_join ( cwd , source_file1 )
    call assert ( trim(expected)==trim(computed) , "Error in normalize.")
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! Test #18 : get temporary directory
    !
    call logmsg ( "Test #18 : temporary directory" )
    call filedir_tempdir ( tmpdir )
    write ( msg ,*)  "Temporary directory :", tmpdir
    call logmsg ( msg )
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! Test #19 : find files by pattern
    !
    call logmsg ( "Test #19 : find files by pattern" )
    pattern = "*.txt"
    call filedir_findByPattern ( pattern , filelength , numberOfFiles , listOfFiles )
    call logmsg ("Files: ")
    write ( msg ,*)  "Number :", numberOfFiles
    call logmsg ( msg )
    call assert ( numberOfFiles == 2 , "Error in filedir_findByPattern.")
    ! TODO : test that the file tails are the expected ones : declaration.txt and declaration2.txt
    do ifile = 1 , numberOfFiles
       write ( msg ,*)  "File #", ifile, ":", listOfFiles ( : , ifile )
       call logmsg (msg)
       write ( computed , "(200a)" ) listOfFiles ( : , ifile )
       computed = filedir_tail ( computed )
       select case ( ifile )
       case ( 1 )
          call assertString ( trim(computed) , "declaration.txt" , "Error in filedir_findByPattern")
       case ( 2 )
          call assertString ( trim(computed) , "declaration2.txt" , "Error in filedir_findByPattern")
       case default
          call logmsg ( "Unknown file number in filedir_findByPattern")
       end select
    enddo
    deallocate( listOfFiles )
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! Test #20 : split a file name
    !
    call logmsg ( "Test #20 : split a file name" )
    normalized_source_file = filedir_normalize ( source_file1 )
    call filedir_split ( normalized_source_file , numberOfComponents , numberOfChars , &
         listOfComponents )
    write ( msg ,*)  "Number of components:", numberOfComponents
    call logmsg ( msg )
    write ( msg ,*)  "Number of characters:", numberOfChars
    call logmsg ( msg )
    do iComponent =1 , numberOfComponents
       write ( msg ,*)  "Component #", iComponent , ":", listOfComponents ( 1:numberOfChars , iComponent )
       call logmsg (msg)
       write ( computedComponent , "(15a)" ) listOfComponents ( : , iComponent )
       if (iComponent==numberOfComponents) then
          call assertString ( trim(computedComponent) , "declaration.txt" , "Error in filedir_split (1)")
       elseif (iComponent==numberOfComponents-1) then
          call assertString ( trim(computedComponent) , "filedir" , "Error in filedir_split (2)")
       elseif (iComponent==numberOfComponents-2) then
          call assertString ( trim(computedComponent) , "tests" , "Error in filedir_split (3)")
       endif
    enddo
    deallocate ( listOfComponents )
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! Test #21 : compute a temporary file name
    !
    call logmsg ( "Test #21 : computes a temporary file name" )
    ! Call #1
    call filedir_tempfile ( tempfile )
    fexist = filedir_exists ( tempfile )
    call assert ( fexist , "Error in filedir_tempfile (1)")
    ! Call #2
    call filedir_tempfile ( tempfile2 )
    fexist = filedir_exists ( tempfile2 )
    call assert ( fexist , "Error in filedir_tempfile (2)")
    ! Call #3
    call assert ( tempfile /= tempfile2 , "Error in filedir_tempfile (3)")
    call filedir_delete ( tempfile )
    call filedir_delete ( tempfile2 )
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! Test #22 : touch
    !
    call logmsg ( "Test #22 : touch files" )
    call filedir_tempfile ( tempfile )
    atime1 = filedir_atime ( tempfile )
    mtime1 = filedir_mtime ( tempfile )
    call filedir_touch ( tempfile )
    atime2 = filedir_atime ( tempfile )
    mtime2 = filedir_mtime ( tempfile )
    ! TODO : fix that the windows subroutine does not allow to update the access or modification time.
    !call assert ( atime2 > atime1 , "Error in filedir_touch (1)")
    !call assert ( mtime2 > mtime1 , "Error in filedir_touch (2)")
    call filedir_delete ( tempfile )
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! Test #23 : change directory
    !
    call logmsg ( "Test #23 : change directories" )
    dirname1 = ".."
    call platform_cd ( dirname1 )
    call filedir_pwd ( cwd )
    dirtail = filedir_tail ( cwd )
    call assertString ( trim(dirtail) , "tests" , "Error in platform_cd (1)")
    dirname2 = "filedir"
    call platform_cd ( dirname2 )
    call filedir_pwd ( cwd )
    dirtail = filedir_tail ( cwd )
    call assertString ( trim(dirtail) , "filedir" , "Error in platform_cd (2)")
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()    
    !
    ! Test #24 : is directory
    !
    call logmsg ( "Test #24 : is directory" )
    call filedir_pwd ( cwd1 )
    isdir = filedir_isdirectory ( cwd1 )
    call assert ( isdir , "Error in filedir_isdirectory (1)")
    dirname1 = "fooYepYepYep!"
    isdir = filedir_isdirectory ( dirname1 )
    call assert ( .NOT.isdir , "Error in filedir_isdirectory (2)")
    call filedir_pwd ( cwd2 )
    call assertString ( trim(cwd1) , trim(cwd2) , "Error in platform_cd (3)")
    !!$  !
    !!$  ! Test #25 : file normalize with full path
    !!$  !
    !!$  call logmsg ( "Test #25 : file normalize" )
    !!$  call filedir_tempfile ( tempfile )
    !!$  tempfile2 = filedir_normalize ( tempfile )
    !!$  call assertString ( trim(tempfile) , trim(tempfile2) , "Error in filedir_normalize (2)")
    !!$  !
    !!$  ! Test #26 : file normalize with relative path
    !!$  !
    !!$  call logmsg ( "Test #26 : file normalize" )
    !!$  dirname1 = ".."
    !!$  call filedir_pwd ( cwd )
    !!$  dirname1 = filedir_join ( cwd , dirname1 )
    !!$  dirname2 = filedir_normalize ( dirname1 )
    !!$  dirtail = filedir_tail ( dirname2 )
    !!$  call assertString ( trim(dirtail) , "tests" , "Error in filedir_normalize (3)")
    !!$  !
    
    !
    ! Test #27 : create / delete a directory
    !
    call logmsg ( "Test #27 : file mkdir" )
    call filedir_tempdir ( tmpdir )
    dirname1 = filedir_join ( tmpdir , "myTestDir" )
    call filedir_mkdir ( dirname1 )
    isdir = filedir_isdirectory ( dirname1 )
    call assert ( isdir , "Error in filedir_mkdir (1)")
    call filedir_delete ( dirname1 )
    isdir = filedir_isdirectory ( dirname1 )
    call assert ( .NOT.isdir , "Error in filedir_mkdir (2)")
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! Test #28 : create / delete a non empty directory without / with the force option
    !
    call logmsg ( "Test #28 : file mkdir" )
    call filedir_tempdir ( tmpdir )
    dirname1 = filedir_join ( tmpdir , "myTestDir" )
    call filedir_mkdir ( dirname1 )
    tempfile = filedir_join ( dirname1 , "testfile.txt" )
    call filedir_touch ( tempfile )
    isdir = filedir_isdirectory ( dirname1 )
    call assert ( isdir , "Error in filedir_mkdir (3)")
    call filedir_delete ( dirname1 , status )
    call assert ( status/= 0, "Error in filedir_delete (5)")
    force = .true.
    call filedir_delete ( dirname1 , force , status )
    isdir = filedir_isdirectory ( dirname1 )
    call assert ( .NOT.isdir , "Error in filedir_mkdir (4)")
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! Test #29 : create a directory which allready exists
    !
    call logmsg ( "Test #29 : file mkdir" )
    call filedir_tempdir ( tmpdir )
    dirname1 = filedir_join ( tmpdir , "myTestDir" )
    call filedir_mkdir ( dirname1 )
    call filedir_mkdir ( dirname1 , status )
    call assert ( status/= 0, "Error in filedir_mkdir (5)")
    call filedir_delete ( dirname1 )
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! Test #30 : compute the native name
    ! Caution !
    !   The expected string contains a backslash.
    !   To prevent gcc-style compilers (e.g. gfortran)
    !   to process to backslash substitution, use "-fno-backslash" option.
    !
    call logmsg ( "Test #30 : filedir_nativename" )
    tempfile = filedir_nativename ( "./toto.txt" )
    expected = ".\toto.txt"
    call assertString ( trim(tempfile) , trim(expected) , "Error in filedir_nativename (1)")
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! Test #31 : compute the path type of one file
    !
    call logmsg ( "Test #31 : filedir_pathtype" )
    pathtype = filedir_pathtype ( "./toto.txt" )
    call assert ( pathtype==FS_PATHTYPE_RELATIVE , "Error in filedir_pathtype (1)")
    pathtype = filedir_pathtype ( "toto.txt" )
    call assert ( pathtype==FS_PATHTYPE_RELATIVE , "Error in filedir_pathtype (2)")
    pathtype = filedir_pathtype ( "../toto.txt" )
    call assert ( pathtype==FS_PATHTYPE_RELATIVE , "Error in filedir_pathtype (3)")
    call filedir_tempfile ( tempfile )
    pathtype = filedir_pathtype ( tempfile )
    call assert ( pathtype==FS_PATHTYPE_ABSOLUTE , "Error in filedir_pathtype (4)")
    call filedir_delete ( tempfile )
    pathtype = filedir_pathtype ( "/toto.txt" )
    current_platform = platform_get_platform ()
    if (current_platform==PLATFORM_PLATFORM_WINDOWS) then
       call assert ( pathtype==FS_PATHTYPE_VOLUMERELATIVE , "Error in filedir_pathtype (5)")
    else
       call assert ( pathtype==FS_PATHTYPE_ABSOLUTE , "Error in filedir_pathtype (6)")
    endif
    !
    ! Check that the number of opened units is OK
    !
    call check_fileunitsnb ()
    !
    ! Open several units and close all
    !
    file_unit1 = filedir_get_unit (  )
    open ( unit = file_unit1 , file = source_file1 )
    file_unit2 = filedir_get_unit (  )
    open ( unit = file_unit2 , file = source_file2 )
    call filedir_getallopenunits ( nbunits , units )
    call assert ( nbunits == 3 , "Wrong number of units")
    ! Unit #1 is the logger for the tests itself
    call assert ( units ( 1 ) == file_unit1 , "Wrong unit #1")
    call assert ( units ( 2 ) == file_unit2 , "Wrong unit #2")
    deallocate ( units )
    call filedir_reportunit ( log_unit , file_unit1 )
    call filedir_reportunit ( log_unit , file_unit2 )
    call filedir_displayopenunits ( log_unit )
    ! We cannot do that here because of the logger for the tests !!!
    ! But, yes, it works.
    ! call filedir_closeallopenunits ()
    close ( file_unit1 )
    close ( file_unit2 )
    call filedir_getallopenunits ( nbunits , units )
    call assert ( nbunits == 1 , "Wrong number of units")
    deallocate ( units )
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
    
  end subroutine test_m_filedir_all
  !
  ! assert --
  !   Check that the given test is true and updates the assertion system.
  !
  subroutine assert (test, message)
    implicit none
    logical         , intent(in) :: test
    character(len=*), intent(in) :: message
    character(len=50) :: origin
    integer, parameter :: MSG_LEN = 200
    character (len= MSG_LEN ) :: msg
    origin = "test_m_filedir.f90"
    assertTestIndex = assertTestIndex + 1
    if (.NOT.test) then
       write(msg,*) "-> Test #", assertTestIndex , " FAIL"
       call logmsg ( msg )
       write(msg,*) "Origin:", origin
       call logmsg ( msg )
       write(msg,*) "Error: ", trim(message)
       call logmsg ( msg )
       assertTotalTestFail = assertTotalTestFail + 1
    else
       write(msg,*) "-> Test #", assertTestIndex , " PASS"
       call logmsg ( msg )
       assertTotalTestSuccess = assertTotalTestSuccess + 1
    endif
  end subroutine assert
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
  ! logmsg --
  !   Write a message into the log file
  !
  subroutine logmsg ( message )
    implicit none
    character(len=*), intent(in) :: message
    write(6,*) trim(message)
    write(log_unit,*) trim(message)
  end subroutine logmsg
  !
  ! log_startup --
  !   Startup logger
  !
  subroutine log_startup ( filename )
    implicit none
    character(len=*), intent(in) :: filename
    open ( log_unit , file=filename, action = "write")
  end subroutine log_startup
  !
  ! log_shutdown --
  !   Shutdown logger
  !
  subroutine log_shutdown ( )
    implicit none
    close ( log_unit )
  end subroutine log_shutdown
  !
  ! assert_startup --
  !   Startup assertion system.
  !
  subroutine assert_startup ( )
    implicit none
    assertTotalTestFail = 0
    assertTotalTestSuccess = 0
    assertTestIndex = 0
  end subroutine assert_startup
  !
  ! assert_shutdown --
  !   Shutdown assertion system.
  !
  subroutine assert_shutdown ( )
    implicit none
    character (len= 200 ) :: msg
    call logmsg ( "**********************" )
    call logmsg ( "End of tests." )
    write ( msg , * ) "Total number of tests : ", assertTotalTestSuccess + assertTotalTestFail
    call logmsg ( msg )
    write ( msg , * ) "Total number of success tests : ", assertTotalTestSuccess
    call logmsg ( msg )
    write ( msg , * ) "Total number of failing tests : ", assertTotalTestFail
    call logmsg ( msg )
  end subroutine assert_shutdown
  !
  ! check_fileunitsnb --
  !   Check that the number of opened units is OK
  !
  subroutine check_fileunitsnb ()
    integer :: nbunits
    integer , dimension(:) , pointer :: units
    call filedir_getallopenunits ( nbunits , units )
    if ( nbunits /= 1 ) then
       call filedir_displayopenunits ( log_unit )
    endif
    call assert ( nbunits == 1 , "Wrong number of units")
    deallocate ( units )
  end subroutine check_fileunitsnb
end program test_filedir

!
! Display an error and STOP the execution.
!
subroutine file_error ( command )
  use m_filedir, only : MAX_COMMAND_LENGTH
  implicit none
  character (len= MAX_COMMAND_LENGTH ), intent(in) :: command
  character (len= 40 ) :: cwd
  write (6,*) "Error in operating on files while executing command :"
  write (6,*) trim(command)
  call GETCWD (cwd)
  write (6,*) "Current working directory : ", trim(cwd)
  STOP
end subroutine file_error

