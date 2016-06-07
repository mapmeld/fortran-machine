!
! m_filedir.f90 --
!   Manipulate file and directory names.
!
!   Obsolete : use m_vfile instead to get the advantage of dynamic strings,
!   and several bug fixes and features improvements.
!
!   The module contains the following functions:
!   filedir_rootname       Return the name without an extension
!   filedir_extension      Return the extension
!   filedir_tail           Return the name without the directory (if present)
!   filedir_dirname        Return the directory name
!   filedir_join           Concatenate a directory name and a file name
!   filedir_add_extension  Add an extension to a file name
!
!   The functions actually perform fairly simple string manipulations.
!   It is just that these manipulations occur frequently.
!
! Overview
!
!   This component allows to manage the file system, by providing 
!   services to create, move and destroy files and directories, and 
!   to get informations about files and directories.
!   The services provided are based either on standard fortran,
!   or on fortran extensions.
!
! Portability
!
!   One of the main interest of this component is to separate
!   the client-side application from platform-specific file 
!   management or from compiler-specific fortran extensions. 
!
!   This separation is possible because m_filedir
!   deals for the platform directly, by using the m_platform
!   module. This allows to design a client source code which 
!   portable on several operating systems (for example windows,
!   linux) without any change. For example, the several file 
!   separators used on the various operating systems are taken
!   into account internally : "/" on linux systems, "\" on 
!   windows systems and ":" on Mac OS.
!
!   The portability is also ensured with respect to the 
!   fortran compiler used to create the executable.
!   All fortran compilers provide commands to rename the files
!   or get the working directory. But not all fortran compilers
!   define these commands the same way : some provide subroutines,
!   some provide functions, etc... The current component can 
!   be configured at compile-time with pre-processing commands.
!   This allows to configure the component with compiler 
!   specific settings, to make so that the component know
!   what features your particular compiler knows about.
!
!     <your application> 
!            |
!        m_filedir -> (operating system , fortran compiler)
!            |
!        m_platform -> (operating system , fortran compiler)
!
! How to use it
!
!   Before using this component, it must be initialized with
!   filedir_init. This allows to initialize platform-specific 
!   internal settings.
!
!   The commands filedir_delete, filedir_copy, filedir_rename
!   allow to delete, copy and rename files or directories.
!   To inquire about a file or directory, one can use 
!   filedir_exists or filedir_isdirectory.
!  
!   In the following example, one creates a new file with filedir_touch,
!   rename that file and finally delete it.
!
!   call filedir_init ()
!   call filedir_touch ( "foo.txt" )
!   call filedir_rename ( "foo.txt" , "toto.txt" )
!   call filedir_delete ( "toto.txt" )
!
!   The filedir_separator returns the platform-specific character 
!   used on the current operating system.
!
!   The commands filedir_nativename , filedir_normalize , filedir_pathtype
!   provide ways to manage file names and paths.
!   The filedir_nativename function returns the platform-specific name of the file. 
!   The filedir_pathtype command returns one of FS_PATHTYPE_ABSOLUTE, 
!   FS_PATHTYPE_RELATIVE, FS_PATHTYPE_VOLUMERELATIVE which correspond to 
!   the current file. The FS_PATHTYPE_VOLUMERELATIVE only exist on 
!   windows. The filedir_normalize command returns a unique normalized 
!   path representation for the file-system object (file, directory, link, 
!   etc), whose string value can be used as a unique identifier for it.
!
!   The filedir_split and filedir_join services allows to separate
!   or concatenate the components of a file. This can be useful
!   when dealing with relative file or directories.
!   The filedir_split command splits a file into pieces each time 
!   the platform-specific separator is found.
!   The filedir_join command concatenate a list of strings with 
!   the platform-specific separator and returns the concatenated
!   file name.
!
!   One particularly useful command when dealing with files is 
!   filedir_findByPattern. The command takes a string as an input
!   file pattern. It then computes the list of all files which
!   match that pattern.
!
! Error management
!
!   The file management may raise errors, for example when the 
!   user want to rename a file which does not exist.
!   Many of the provided commands have an optional integer output 
!   argument "status" which is zero when no error occured 
!   and non-zero in case of error.
!   If the status argument is not provided and an error is generated,
!   then the program stops and a message is displayed on standard 
!   output.
!   These are the public error flags that the current component may generate :
!     FS_ERROR_OK = 0
!     FS_ERROR_UNABLE_TO_OPEN_SOURCE
!     FS_ERROR_UNABLE_TO_OPEN_TARGET
!     FS_ERROR_UNABLE_TO_WRITE_TARGET
!     FS_ERROR_SOURCE_FILE_DOES_NOT_EXIST
!
! Manage file units
!
!   The component provides services to manage fortran file units.
!   The function filedir_get_unit returns an integer representing
!   a fortran unit which is available for opening a file.
!   The typical use of this function is to manage the files dynamically,
!   without any database of file units in the library/software.
!   In the following example, one opens a file with a dynamical
!   file unit.
! 
!     integer :: fileunit
!     fileunit = filedir_get_unit ()
!     open ( unit = fileunit , file = "data.txt" )
!     [etc...]
!
!   If several files are to be opened, the "filedir_get_unit" 
!   method has to be inserted between the "open" statements.
!   This is because two consecutive calls to "filedir_get_unit"
!   will return the same integer, as expected : if a unit is available
!   the first time, it will also be available the second time.
!   In the following example, several files are opened and connected
!   to several files.
! 
!     integer :: fileunit1
!     integer :: fileunit2
!     fileunit1 = filedir_get_unit ()
!     open ( unit = fileunit1 , file = "data.txt" )
!     fileunit2 = filedir_get_unit ()
!     open ( unit = fileunit2 , file = "data2.txt" )
!     [etc...]
!
!   In a large fortran software, it may be difficult to see if some 
!   bug has been introduced in the file management, especially
!   when the software is the composition of several libraries.
!   The subroutines filedir_getallopenunits , filedir_closeallopenunits , 
!   filedir_reportunit , filedir_displayopenunits allow to manage for 
!   the units currently used in the software.
!   The filedir_getallopenunits returns an array of integers which 
!   contains all the currently opened units. The filedir_closeallopenunits
!   subroutine close all currently opened units. The filedir_reportunit
!   displays a full report about a given unit number by using the 
!   "inquire" fortran intrinsic statement.
!
!   Several methods of this component are based on Fortran extensions,
!   which requires compiler-specific settings.
!   For Intel Fortran compiler, the current implementation was based on
!   IFPORT.F90 file in the Intel release for details on the interfaces provided.
!
!   File rename fortran extension.
!   Depending on the compiler, the "RENAME" fortran extension is 
!   provided as a subroutine or a function.
!   For example, this is a short list of compilers and their particular 
!   RENAME provided :
!   - function : Intel Fortran, g95
!   - subroutine : gfortran
!   Choose your RENAME version between one of these :
!   _FS_RENAME_FUNCTION , _FS_RENAME_SUBROUTINE
!
! This is an abstract of all macros for several compilers.
!
! Compiler : Intel Fortran
! _FS_INTEL_FORTRAN_PORTABILITY_ROUTINES
! _FS_RENAME_FUNCTION
!
! Compiler : g95
! _FS_RENAME_FUNCTION
!
! Compiler : gfortran
! _FS_RENAME_SUBROUTINE
!
! Copyright (c) 2008 Arjen Markus arjenmarkus@sourceforge.net
! Copyright (c) 2008 Michael Baudin michael.baudin@gmail.com
!
!   $Id: m_filedir.f90,v 1.6 2008/07/16 13:45:35 relaxmike Exp $
!
module m_filedir
#ifdef _FS_INTEL_FORTRAN_PORTABILITY_ROUTINES
  use ifport
#endif
  use m_platform, only : &
       platform_get_platform, &
       platform_system, &
       PLATFORM_PLATFORM_WINDOWS, &
       PLATFORM_PLATFORM_UNIX, &
       PLATFORM_PLATFORM_MAC, &
       PLATFORM_PLATFORM_NB, &
       platform_get_environment_variable , &
       platform_cd, &
       platform_stat
  implicit none
  private
  public :: filedir_add_extension
  public :: filedir_atime
  public :: filedir_copy
  public :: filedir_copy_std
  public :: filedir_delete
  public :: filedir_dirname
  public :: filedir_exists
  public :: filedir_extension
  public :: filedir_findByPattern
  public :: filedir_first_separator_index
  public :: filedir_get_unit
  public :: filedir_init
  public :: filedir_isdirectory
  public :: filedir_join
  public :: filedir_last_separator_index
  public :: filedir_mkdir
  public :: filedir_mtime
  public :: filedir_nativename
  public :: filedir_normalize
  public :: filedir_pathtype
  public :: filedir_pwd
  public :: filedir_rename
  public :: filedir_rootname
  public :: filedir_separator
  public :: filedir_split
  public :: filedir_tail
  public :: filedir_tempdir
  public :: filedir_tempfile
  public :: filedir_touch
  public :: filedir_set_stoponerror
  public :: filedir_closeallopenunits
  public :: filedir_reportunit
  public :: filedir_displayopenunits
  public :: filedir_getallopenunits
  !
  ! Static attributes
  !
  ! Maximum number of columns in a text file
  integer, parameter, public :: MAX_COLUMNS_TEXT = 1000
  ! Maximum number of units when searching for an unused file unit
  integer, parameter, public :: MAX_UNIT_NUMBER = 1000
  ! Maximum string length for the error messages generated by the file subroutines
  integer, parameter, public :: MAX_COMMAND_LENGTH = 1000
  character (len=1), parameter :: PLATFORM_SEPARATOR_UNKNOWN = "?"
  character (len=1), save :: PLATFORM_SEPARATOR = PLATFORM_SEPARATOR_UNKNOWN
  !
  ! Static, Platform-specific commands
  !
  character(len=20), save :: command_ls
  character(len=20), save :: command_copy
  character(len=20), save :: command_mkdir
  character(len=20), save :: command_redirect
  character(len=20), save :: command_suppress_msg
  character(len=20), save :: command_touch
  character(len=20), save :: command_rmdir
  character(len=20), save :: command_rmdir_force
  ! Set to .true. if the static attributes have allready been initialized
  logical :: filedir_static_initialized = .false.
  !
  ! Tags for path types
  !
  integer, parameter, public :: FS_PATHTYPE_ABSOLUTE = 1
  integer, parameter, public :: FS_PATHTYPE_RELATIVE = 2
  integer, parameter, public :: FS_PATHTYPE_VOLUMERELATIVE = 3
  interface filedir_delete
     module procedure filedir_delete_with_force
     module procedure filedir_delete_without_force
  end interface filedir_delete
  !
  ! Tags to manage errors.
  !
  integer , parameter, public :: FS_ERROR_OK = 0
  integer , parameter, public :: FS_ERROR_UNABLE_TO_OPEN_SOURCE = 1
  integer , parameter, public :: FS_ERROR_UNABLE_TO_OPEN_TARGET = 2
  integer , parameter, public :: FS_ERROR_UNABLE_TO_WRITE_TARGET = 3
  integer , parameter, public :: FS_ERROR_SOURCE_FILE_DOES_NOT_EXIST = 4
  integer , parameter, public :: FS_ERROR_UNDEFINED_SERVICE = 5
  !
  ! Set to true to stop whenever an error comes in the vstring component.
  logical, save :: filedir_stoponerror = .true.
contains
  !
  ! filedir_init --
  !   Initialize module internal state.
  ! Arguments:
  !   no argument
  subroutine filedir_init ( )
    integer :: platform
    character (len=200) :: message
    if (.NOT.filedir_static_initialized) then
       !
       ! 0. Get the current platform
       !
       platform = platform_get_platform ()
       !
       ! 1. Initialize the platform-specific separator
       !
       ! Setup the separator depending on the platform
       select case ( platform )
       case ( PLATFORM_PLATFORM_WINDOWS )
          PLATFORM_SEPARATOR = "\"
       case ( PLATFORM_PLATFORM_UNIX )
          PLATFORM_SEPARATOR = "/"
       case ( PLATFORM_PLATFORM_MAC )
          PLATFORM_SEPARATOR = ":"
       case default
          write(message,*) "Unknown separator for platform :", platform
          call filedir_error ( "filedir_init" , message )
       end select
       !
       ! 2. Initialize the platform-specific commands
       !
       select case ( platform )
       case ( PLATFORM_PLATFORM_WINDOWS )
          ! See http://en.wikipedia.org/wiki/List_of_DOS_commands
          command_ls = "dir /b"
          command_copy = "copy"
          command_mkdir = "mkdir"
          command_redirect = ">"
          command_suppress_msg = "2>nul"
          command_touch = ""
          command_rmdir = "rd"
          command_rmdir_force = "rmdir /s /q"
       case ( PLATFORM_PLATFORM_UNIX )
          ! See http://en.wikipedia.org/wiki/List_of_Unix_programs
          command_ls = "ls"
          command_copy = "cp"
          command_mkdir = "md"
          command_redirect = ">"
          command_suppress_msg = "2>/dev/null"
          command_touch = "touch"
          command_rmdir = "rm"
          command_rmdir_force = "rm -r -f"
       case default
          write(message,*) "Unknown commands for platform :", platform
          call filedir_error ( "filedir_init" , message )
       end select
       !
       ! Update the static flag
       !
       filedir_static_initialized = .true.
    endif
  end subroutine filedir_init
  !
  ! filedir_rootname --
  !   Return the name without the extension (if any)
  ! Arguments:
  !   filename   Name of the file to be examined
  ! Result:
  !   The part of the name _before_ the last "." or the whole name
  !   if no "." is present
  !
  function filedir_rootname ( filename ) result (rootname)
    character(len=*), intent(in)             :: filename
    character(len=len(filename)) :: rootname
    integer                      :: kdot
    integer                      :: kseparator
    kdot   = scan( filename, '.', .true. )
    kseparator = filedir_last_separator_index ( filename )
    rootname = filename
    if ( kdot .ne. 0 .and. kdot .gt. kseparator+1 ) then
       rootname = filename(1:kdot-1)
    endif
  end function filedir_rootname
  !
  ! filedir_extension --
  !   Return the extension (if any)
  ! Arguments:
  !   filename   Name of the file to be examined
  ! Result:
  !   The part of the name _after_ and including the last "." or empty if none
  !   present
  ! Example : if filename is "declaration.txt", the file extension is ".txt".
  !
  function filedir_extension ( filename ) result (extension)
    character(len=*), intent(in)             :: filename
    character(len=len(filename)) :: extension
    integer                      :: kdot
    integer                      :: kseparator
    kdot   = scan( filename, '.', .true. )
    kseparator = filedir_last_separator_index ( filename )
    extension = ''
    if ( kdot .ne. 0 .and. kdot .gt. kseparator+1 ) then
       extension = filename(kdot:)
    endif
  end function filedir_extension
  !
  ! filedir_tail --
  !   Returns all of the characters in name after the last directory separator
  ! Arguments:
  !   filename   Name of the file to be examined
  ! Result:
  !   Returns all of the characters in name after the last directory separator
  !   If name contains no separators then returns name.
  !
  function filedir_tail ( filename ) result (basename)
    character(len=*), intent(in)             :: filename
    character(len=len(filename)) :: basename
    integer                      :: kseparator
    kseparator = filedir_last_separator_index ( filename )
    basename = filename
    if ( kseparator .gt. 1 ) then
       basename = filename(kseparator+1:)
    endif
  end function filedir_tail
  !
  ! filedir_dirname --
  !   Return the directory (if any)
  ! Arguments:
  !   filename   Name of the file to be examined
  ! Result:
  !   The part of the name _before_ the last directory separator
  !
  function filedir_dirname( filename ) result (dirname)
    character(len=*), intent(in)             :: filename
    character(len=len(filename)) :: dirname
    integer                      :: kseparator
    kseparator = filedir_last_separator_index ( filename )
    dirname = ''
    if ( kseparator .gt. 1 ) then
       dirname = filename(1:kseparator)
    endif
  end function filedir_dirname
  !
  ! filedir_last_separator_index --
  !   Returns the index of the last separator in the given filename
  !   or 0 if there is no separator in the given file name.
  ! Arguments:
  !   filename   Name of the file to be examined
  !
  integer function filedir_last_separator_index ( filename )
    character (len=1) :: separator
    character(len=*), intent(in)             :: filename
    logical , parameter :: backward = .true.
    separator = filedir_separator ()
    filedir_last_separator_index = scan( filename, separator , backward )
  end function filedir_last_separator_index
  !
  ! filedir_join --
  !   Return the directory plus the file
  ! Arguments:
  !   directory  Name of the directory to be used
  !   filename   Name of the file to be used
  ! Result:
  !   Concatenated directory and file names
  !
  function filedir_join ( directory, filename ) result (fullname)
    character(len=*),intent(in)             :: directory
    character(len=*),intent(in)             :: filename
    character(len=len(directory)+len(filename)+1) :: fullname
    fullname = trim(directory) // filedir_separator () // filename
  end function filedir_join
  !
  ! filedir_add_extension --
  !   Return a new file name with the given extension concatenated.
  !   If the given file name ends with a dot and the given extension begins
  !   with a dot, only one dot is kept.
  ! Arguments:
  !   filename   Name of the file to be used
  !   extension  Extension to be added (e.g. ".txt")
  ! Result:
  !   The file name with an added extension
  ! Note :
  !   The extension of one file begins with a dot : ".txt" is a file
  !   extension while "txt" is not.
  !
  function filedir_add_extension ( filename , extension ) result (newname)
    character(len=*), intent(in)             :: filename
    character(len=*), intent(in)             :: extension
    ! Caution !
    ! The length of the new file is greater that the length of the original file !
    ! This is why the logic must be kept simple :
    ! if one takes into account that the extension may or may not include
    ! 1 dot, the length of the new file is not predictible, that is
    ! may be len(filename)+len(extension) or len(filename)+len(extension) + 1,
    ! which would lead to memory errors if the file names have just the right size.
    character(len=len(filename)+len(extension)) :: newname
    integer :: kdot
    integer :: length_trimmed
    kdot    = index( filename , '.', .true. )
    length_trimmed = len_trim(filename)
    if ( kdot == length_trimmed ) then
       ! The file name ends with a dot, so we do not add one.
       newname = trim(filename(1:kdot-1))//trim(extension)
    else
       newname = trim(filename)//trim(extension)
    endif
  end function filedir_add_extension
  !
  ! filedir_rename --
  !   Renames the file ofdln to newfn by using the RENAME fortran extension.
  ! Arguments:
  !   oldfn : the old file name
  !   newfn : the new file name
  !   status, optional : if supplied, it contains 0 on success or nonzero error code
  !     upon return
  !
  subroutine filedir_rename ( oldfn , newfn , status )
    character(len=*), intent(in) :: oldfn, newfn
    integer, intent(out) , optional :: status
    integer :: local_status
    character ( len = len( oldfn )+len(newfn)) :: message
    logical :: fexist
    !
    ! Check source file.
    !
    fexist = filedir_exists ( oldfn )
    if ( .NOT. fexist ) then
       if ( present ( status ) ) then
          status = FS_ERROR_SOURCE_FILE_DOES_NOT_EXIST
       else
          write(message,*) "Unable to access to old file :", trim( oldfn )
          call filedir_error ( "filedir_rename" , message )
       endif
       return
    endif
    !
    ! Rename the file
    !
#ifdef _FS_RENAME_FUNCTION
    local_status = RENAME ( oldfn , newfn )
#endif
#ifdef _FS_RENAME_SUBROUTINE
    call RENAME ( oldfn , newfn , local_status )
#endif
    !
    ! Case when no macro is defined
    !
#ifndef _FS_RENAME_FUNCTION
#ifndef _FS_RENAME_SUBROUTINE
    if ( present ( status ) ) then
       status = FS_ERROR_UNDEFINED_SERVICE
    else
       write(message,*) "The rename service is not provided."
       call filedir_error ( "filedir_rename" , message )
    endif
#endif
#endif
    if ( present ( status ) ) then
       status = local_status
       ! TODO : check if the following line should be instead "elseif ( local_status /=0 ) then"
    else
       write ( message , * ) "Unable to rename file ", trim( oldfn ), " to file ", trim( newfn )
       call filedir_error ( "filedir_rename" , message )
    endif
  end subroutine filedir_rename
  !
  ! filedir_copy --
  !   Copy the file ofdln to newfn by using the SYSTEM fortran extension.
  ! Arguments:
  !   sourcefn : the source file name
  !   targetfn : the target file name
  !   status, optional : if supplied, it contains 0 on success or nonzero error code
  !      upon return
  ! Note :
  !    This subroutine is system-dependent.
  !
  subroutine filedir_copy ( sourcefn , targetfn , status )
    character(len=*), intent(in) :: sourcefn, targetfn
    integer, intent(out) , optional :: status
    integer :: local_status
    character (len=len(sourcefn)+len(targetfn)+len(command_copy) + 2) :: command
    character ( len = len(sourcefn)+len(targetfn)+22) :: message
    logical :: fexist
    !
    ! Check source file.
    !
    fexist = filedir_exists ( sourcefn )
    if ( .NOT. fexist ) then
       if ( present ( status ) ) then
          status = FS_ERROR_SOURCE_FILE_DOES_NOT_EXIST
       else
          write(message,*) "Unable to access to source file :", trim( sourcefn )
          call filedir_error ( "filedir_rename" , message )
       endif
       return
    endif
    !
    ! Make the copy
    !
    write ( command , *) trim(command_copy) , " ",  trim(sourcefn), " ", trim(targetfn)
    call platform_system ( command , local_status )
    if ( present ( status ) ) then
       status = local_status
    elseif ( local_status /=0 ) then
       write(message,*) "Unable to copy :", trim(sourcefn) , " into " , trim(targetfn)
       call filedir_error ( "filedir_copy" , message )
    endif
  end subroutine filedir_copy
  !
  ! filedir_copy_std --
  !   Copy the ascii file ofdln to targetfn by using standard fortran.
  !   If the source file does not exists, generates an error.
  !   If the target file allready exists and force option is undefined
  !   or defined to false, generates an error.
  ! Arguments:
  !   sourcefn : the source file name
  !   targetfn : the target file name
  !   status, optional : if supplied, it contains 0 on success or nonzero error code
  !     upon return
  !    status = 1 when one was unable to open the source file
  !    status = 2 when one was unable to open the target file
  !    status = 3 when there was a problem while writing the target file
  !    status = 4 when the source file does not exist
  !   force, optional : if supplied and true, if the target file allready exists, delete it before
  !                making the copy.
  ! Caution !
  !   1. The maximum number of columns in the source filename is 1000.
  !   2. After execution, the target file is not an exact copy of the source file.
  !   Because of the fortran format used, all the lines of the target file are of length 1000 :
  !   blank spaces are appended at the end of the string.
  !
  !
  subroutine filedir_copy_std ( sourcefn , targetfn , status , force , trimline )
    character(len = *), intent(in) :: sourcefn, targetfn
    integer, intent(out) , optional :: status
    logical, intent(in) , optional :: force
    logical, intent(in), optional :: trimline
    integer :: local_status
    integer :: source_unit, target_unit
    character ( len = MAX_COLUMNS_TEXT ) :: string
    integer :: end_of_file
    logical :: fexist
    character ( len = len(sourcefn)+len(targetfn)+22) :: message
    logical :: trimline_real
    !
    ! Process options
    !
    if ( present ( trimline ) ) then 
       trimline_real = trimline
    else
       trimline_real = .true.
    endif
    !
    ! By default, there is no problem.
    !
    local_status = FS_ERROR_OK
    !
    ! 0. Check that the source file exist.
    !
    fexist = filedir_exists ( sourcefn )
    if ( .NOT. fexist ) then
       if ( present ( status ) ) then
          status = FS_ERROR_SOURCE_FILE_DOES_NOT_EXIST
       else
          write(message,*) "Source file :", trim(sourcefn) , " does not exist."
          call filedir_error ( "filedir_copy_std" , message )
       endif
       return
    endif
    !
    ! 1. If the target file exists, delete it if force = .true.
    !
    if (present ( force )) then
       if (force) then
          fexist = filedir_exists ( targetfn )
          if (fexist) then
             call filedir_delete ( targetfn )
          endif
       endif
    endif
    !
    ! 2. Get two free file units
    !
    source_unit = filedir_get_unit (  )
    open ( UNIT = source_unit , FILE= sourcefn , ACTION='READ',STATUS='OLD', IOSTAT= local_status )
    if ( local_status /=0) then
       local_status = FS_ERROR_UNABLE_TO_OPEN_SOURCE
    else
       target_unit = filedir_get_unit (  )
       open ( UNIT = target_unit , FILE= targetfn , ACTION='WRITE',STATUS='NEW', IOSTAT= local_status )
       if ( local_status /=0) then
          local_status = FS_ERROR_UNABLE_TO_OPEN_TARGET
       else
          !
          ! 3. Copy the lines one after another.
          !
          do
             read ( source_unit , '(a)', iostat = end_of_file ) string
             if ( end_of_file /= 0 ) then
                ! The last line has been found.
                exit
             end if
             if ( trimline_real ) then
                write ( target_unit , '(a)', iostat = local_status ) trim(string)
             else
                write ( target_unit , '(a)', iostat = local_status ) string
             endif
             if ( local_status /= 0 ) then
                ! There was an error while writing
                local_status = FS_ERROR_UNABLE_TO_WRITE_TARGET
                exit
             end if
          enddo
          close ( target_unit )
       endif
       close ( source_unit )
    endif
    if ( present ( status ) ) then
       status = local_status
    elseif ( local_status /=0 ) then
       write(message,*) "Unable to copy :", trim(sourcefn) , " into " , trim(targetfn)
       call filedir_error ( "filedir_copy_std" , message )
    endif
  end subroutine filedir_copy_std
  !
  ! filedir_pwd --
  !   Returns the name of the current directory by using the fortran
  !   extension GETCWD
  ! Arguments:
  !   cwd : the current working directory
  !
  subroutine filedir_pwd ( cwd )
    character(len=*), intent ( out ) :: cwd
    call GETCWD ( cwd )
  end subroutine filedir_pwd
  !
  ! filedir_exists --
  !   Returns .true. if file name exists, .false. otherwise.
  ! Arguments:
  !   filename   Name of the file to be examined
  !
  logical function filedir_exists ( filename )
    character(len=*), intent(in) :: filename
    logical :: fexist
    ! Note :
    ! The other possibility is the ACCESS fortran extension
    ! But the "inquire" intrinsic in fortran standard.
    inquire ( FILE = filename , EXIST = fexist )
    if (fexist) then
       filedir_exists = .true.
    else
       filedir_exists = .false.
    endif
  end function filedir_exists
  !
  ! filedir_delete_with_force --
  !   Removes the file or directory specified by each pathname argument. 
  !   Non-empty directories will be removed only if the force option is specified.
  ! Arguments:
  !   filename   Name of the file to be examined
  !   force, optional : if supplied, forces to delete the directory, even if it is empty.
  !   status, optional : if supplied, it contains 0 on success or nonzero error code
  !     upon return
  !
  subroutine filedir_delete_with_force ( filename , force , status )
    character(len=*), intent(in) :: filename
    logical, intent (in) :: force
    integer, intent (out), optional :: status
    integer :: local_status
    integer :: file_unit
    character(len=len(filename)+23) :: message
    logical :: isdir
    ! TODO : this length is ugly, introduce real dynamic strings in fortran 90 OO
    character ( len = len(filename)+len(command_rmdir)+len(command_rmdir_force)+ 1) :: command
    isdir = filedir_isdirectory ( filename )
    if (.NOT.isdir) then
       ! Delete that regular file
       ! One could use the following fortran extension :
       !   call UNLINK ( filename , local_status )
       ! But the following code uses standard fortran statements and therefore
       ! will be full portable.
       file_unit = filedir_get_unit (  )
       open ( UNIT = file_unit , FILE = filename , STATUS ='OLD', IOSTAT= local_status )
       if ( local_status == 0 ) then
          close ( UNIT = file_unit , STATUS = 'DELETE', IOSTAT= local_status )
       endif
    else
       !
       ! Delete that directory
       !
       if (force) then
          write ( command , * ) trim(command_rmdir_force) , " ", trim(filename)
       else
          write ( command , * ) trim(command_rmdir) , " ", trim(filename)
       endif
       call platform_system ( command , local_status )
       isdir = filedir_isdirectory ( filename )
       if (isdir) then
          local_status = -1
       endif
    endif
    if (present ( status )) then
       status = local_status
    elseif ( local_status /=0 ) then
       write(message,*) "Unable to delete file :", trim(filename)
       call filedir_error ( "filedir_delete" , message )
    endif
  end subroutine filedir_delete_with_force
  !
  ! filedir_delete_without_force --
  !   Interface for filedir_delete_with_force to manage the force option.
  !
  subroutine filedir_delete_without_force ( filename , status )
    character(len=*), intent(in) :: filename
    integer, intent (out), optional :: status
    logical , parameter :: force = .false.
    call filedir_delete_with_force ( filename , force , status )
  end subroutine filedir_delete_without_force
  !
  ! filedir_atime --
  !   Returns an integer representing the time at which file name was last accessed.
  ! Arguments:
  !   filename : the file name
  !   status, optional : if supplied, it contains 0 on success or nonzero error code
  !   upon return
  ! NOTE :
  !   The CTIME fortran extension is defined in gfortran, in Intel Fortran,
  !   but not in the g95 compiler. So we used the STAT fortran extension :
  !   INTEGER FUNCTION stat(file, sarray)
  !     CHARACTER(LEN=*), INTENT(IN) :: file
  !     INTEGER, INTENT(OUT) :: sarray(13), status
  !   END FUNCTION stat
  !
  integer function filedir_atime ( filename , status )
    character(len=*), intent(in) :: filename
    integer, dimension (1:13) :: statarray
    integer, intent(out) , optional :: status
    integer  :: local_status
    call platform_stat ( filename , statarray , local_status )
    filedir_atime = statarray (9)
    if (present ( status )) then
       status = local_status
    endif
  end function filedir_atime
  !
  ! filedir_mtime --
  !   Returns an integer representing the time at which file name was last modified.
  ! Arguments:
  !   filename : the file name
  !   status, optional : if supplied, it contains 0 on success or nonzero error code
  !   upon return
  integer function filedir_mtime ( filename , status )
    character(len=*), intent(in) :: filename
    integer, dimension (1:13) :: statarray
    integer, intent(out) , optional :: status
    integer  :: local_status
    call platform_stat ( filename , statarray , local_status )
    filedir_mtime = statarray (10)
    if (present ( status )) then
       status = local_status
    endif
  end function filedir_mtime
  !
  ! filedir_separator --
  !   Return the separator for the current platform
  ! Arguments:
  !   no argument
  ! Result:
  !   PLATFORM_SEPARATOR.
  ! Note:
  !   The separator depends on the platform :
  !   - "/" on Unix, Linux systems,
  !   - "\" on Windows systems,
  !   - ":" on Macintosh.
  !
  function filedir_separator ( )
    character :: filedir_separator
    call filedir_init ( )
    filedir_separator = PLATFORM_SEPARATOR
  end function filedir_separator
  !
  ! filedir_normalize --
  !   Returns a unique normalized path representation for the
  !   file-system object (file, directory, link, etc), whose string
  !   value can be used as a unique identifier for it. A normalized path
  !   is an absolute path which has all '../', './' removed. Also it is one which
  !   is in the ``standard'' format for the native platform.
  ! Arguments:
  !   filename   Name of the file to be used
  !   normalized  The normalized file name
  !
  function filedir_normalize ( filename )
    character(len=*), intent(in)             :: filename
    character(len=len(filename))             :: filedir_normalize
    character(len=len(filename)) :: directory
    character(len=len(filename)) :: ftail
    ! TODO : remove the .. and . in the path, based on filedir_split
    ! TODO : replace base separator into native separator
    call filedir_pwd ( directory )
    ftail = filedir_tail ( filename )
    filedir_normalize = filedir_join ( directory , ftail )
  end function filedir_normalize
  !
  ! filedir_findByPattern --
  !   Returns a list of files which match the given pattern
  !
  ! Arguments:
  !   pattern         Pattern for the file names (like: *.f90)
  !   list            Allocated list of file names (returned)
  !
  ! Note:
  !   The list of files is an allocated array, the caller should
  !   deallocate it when done.
  !   The length of the character strings in list must be long
  !   enough for the file names. Use for instance a length of at
  !   least 200 characters
  !   The list can zero names long, it will still be allocated.
  ! TODO : extend that subroutine so that it can do an optionnal recursive search into the subdirectories.
  !
  subroutine filedir_findByPattern ( pattern , filelength , numberOfFiles , listOfFiles )
    character(len=*), intent(in)            :: pattern
    integer, intent(in) :: filelength
    integer, intent(out)                    :: numberOfFiles
    ! listOfFiles ( : , ifile) is the file #ifile
    ! listOfFiles ( ichar , ifile ) is the character #ichar for file #ifile
    character, dimension(:,:), pointer :: listOfFiles ! -> does not work
    !!$    !character(len=*), dimension(:), pointer :: listOfFiles ! -> does not work
    !!$    ! NOTE :
    !!$    ! When I test it against Intel Fortran 8 or gfortran 4.0.2, the
    !!$    ! program freezes. I have to change the declaration of the list of files to :
    !!$    !character(len=50), dimension(1:50) :: listOfFiles -> works fine.
    !!$    !character(len=*), dimension(1:50) :: listOfFiles -> does not work
    !!$
    character(len=200)                      :: tmpfile
    character(len=200)                      :: cmd
    character(len=1)                        :: line_unused
    integer                                 :: luntmp
    integer                                 :: i
    integer                                 :: ierr
    integer :: platform
    character (len=200) :: tempdir
    character(len=200)             :: prefix
    integer :: ifile
    character (len=filelength) :: current_filename
    character (len=filelength) :: full_filename
    integer :: icharacter
    ! NOTE :
    ! I would like to use this declaration instead, but this does not work ????
    !character(len=len(pattern))             :: prefix
    !
    ! 1. Compute the list of files and redirect it to a temporary file.
    !
    call filedir_tempdir ( tempdir )
    ! TODO  : create a subroutine which returns the path to a temporary file.
    tmpfile = filedir_join ( tempdir , "__filelist__" )
    !
    ! Compute the platform-specific ls command
    !
    ! Only file names!
    ! Get the current platform
    platform = platform_get_platform ()
    cmd = trim(command_ls) // ' ' // trim(pattern) // ' ' // trim(command_redirect) // trim(tmpfile) &
         // ' ' // command_suppress_msg
    ! TODO : understand when the command has to be fixed up with "file_correct_separator"
    !cmd = filedir_normalize ( cmd )
    !
    ! Under windows, we only get the file tails.
    ! So we have to normalize these files tails to get the full file names.
    !
    if (platform == PLATFORM_PLATFORM_WINDOWS) then
       prefix = pattern
       prefix = filedir_normalize ( prefix )
       i = filedir_last_separator_index ( prefix )
       prefix(i:) = ' '
    endif
    call platform_system ( cmd )
    !
    ! 2. Analyse the content of the temporary file.
    !
    luntmp = filedir_get_unit ( )
    open( luntmp , file = tmpfile )
    !
    ! 2.1 Count the number of files, the maximum length of all files
    !
    numberOfFiles = 0
    do
       read( luntmp, '(a)', iostat = ierr ) line_unused
       if ( ierr == 0 ) then
          numberOfFiles = numberOfFiles + 1
       else
          exit
       endif
    enddo
    rewind( luntmp )
    !
    ! 2.2 Fill the array
    !
    allocate( listOfFiles(1:filelength , 1:numberOfFiles ) )
    do ifile = 1 , numberOfFiles
       !read( luntmp, '(a)' ) listOfFiles(ifile,:)
       read( luntmp, '(a)' ) current_filename
       if (platform == PLATFORM_PLATFORM_WINDOWS) then
          full_filename = filedir_join ( prefix , current_filename )
       else
          full_filename = current_filename
       endif
       do icharacter = 1 , filelength
          listOfFiles ( icharacter , ifile ) = full_filename ( icharacter : icharacter )
       enddo
    enddo
    !
    ! 3. Delete the temporary file
    !
    close( luntmp, status = 'delete' )
  end subroutine filedir_findByPattern
  !
  ! filedir_tempdir --
  !   Returns the temporary directory for the current platform.
  !   The command returns the path of a directory where the caller can
  !   place temporary files, such as "/tmp" on Unix systems.
  !   The algorithm we use to find the correct directory is as follows:
  !   1. The directory named in the TMPDIR environment variable.
  !   2. The directory named in the TEMP environment variable.
  !   3. The directory named in the TMP environment variable.
  !   4. A platform specific location:
  !     Windows
  !         "C:\TEMP", "C:\TMP", "\TEMP", and "\TMP" are tried in that order.
  !     (classic) Macintosh
  !         The TRASH_FOLDER environment variable is used. This is most likely not correct.
  !     Unix
  !         The directories "/tmp", "/var/tmp", and "/usr/tmp" are tried in that order.
  ! Arguments:
  !   tmpdir   Name of the temporary directory
  !
  subroutine filedir_tempdir ( tmpdir )
    ! TODO : how does the user know about the number of characters in the
    ! temporary directory so that he can define its client variable tmpdir ?
    character(len=*), intent(out)             :: tmpdir
    logical :: fexist
    integer :: platform
    character(len=200)             :: envvar
    logical :: tmpdir_found
    character (len=200) :: message
    tmpdir_found = .false.
    !
    ! 1. Try TMPDIR environment variable
    !
    if (.NOT.tmpdir_found) then
       envvar = "TMPDIR"
       call find_tmpdir_in_environment ( envvar , tmpdir , tmpdir_found )
    endif
    !
    ! 2. Try TEMP environment variable
    !
    if (.NOT.tmpdir_found) then
       envvar = "TEMP"
       call find_tmpdir_in_environment ( envvar , tmpdir , tmpdir_found )
    endif
    !
    ! 3. Try TMP environment variable
    !
    if (.NOT.tmpdir_found) then
       envvar = "TMP"
       call find_tmpdir_in_environment ( envvar , tmpdir , tmpdir_found )
    endif
    !
    ! 4. Try platform-specific temporary directories
    !
    if (.NOT.tmpdir_found) then
       platform = platform_get_platform ()
       select case ( platform )
       case ( PLATFORM_PLATFORM_WINDOWS )
          if (.NOT.tmpdir_found) then
             tmpdir = "C:\TEMP"
             tmpdir_found = filedir_exists ( tmpdir )
          endif
          if (.NOT.tmpdir_found) then
             tmpdir = "C:\TMP"
             tmpdir_found = filedir_exists ( tmpdir )
          endif
          if (.NOT.tmpdir_found) then
             tmpdir = "\TEMP"
             tmpdir_found = filedir_exists ( tmpdir )
          endif
          if (.NOT.tmpdir_found) then
             tmpdir = "\TMP"
             tmpdir_found = filedir_exists ( tmpdir )
          endif
       case ( PLATFORM_PLATFORM_UNIX )
          if (.NOT.tmpdir_found) then
             tmpdir = "/tmp"
             tmpdir_found = filedir_exists ( tmpdir )
          endif
          if (.NOT.tmpdir_found) then
             tmpdir = "/var/tmp"
             tmpdir_found = filedir_exists ( tmpdir )
          endif
          if (.NOT.tmpdir_found) then
             tmpdir = "/usr/tmp"
             tmpdir_found = filedir_exists ( tmpdir )
          endif
       case ( PLATFORM_PLATFORM_MAC )
          if (.NOT.tmpdir_found) then
             envvar = "TRASH_FOLDER"
             call find_tmpdir_in_environment ( envvar , tmpdir , tmpdir_found )
          endif
       case default
          write(message,*) "Unknown temporary directory for platform :", platform
          call filedir_error ( "filedir_tempdir" , message )
       end select
    endif
    if (.NOT.tmpdir_found) then
       write(message,*) "Unable to find a temporary directory for platform :", platform
       call filedir_error ( "filedir_tempdir" , message )
    endif
  contains
    subroutine find_tmpdir_in_environment ( envvar , tmpdir , tmpdir_found )
      character(len=*), intent(in) :: envvar
      character(len=*) , intent(out) :: tmpdir
      logical , intent(out) :: tmpdir_found
      character(len=200) :: envvar_value
      tmpdir = ""
      tmpdir_found = .false.
      call platform_get_environment_variable ( envvar , envvar_value )
      if (len_trim(envvar_value)/=0) then
         fexist = filedir_exists ( envvar_value )
         if ( fexist ) then
            tmpdir_found = .true.
            tmpdir = envvar_value
         endif
      endif
    end subroutine find_tmpdir_in_environment
  end subroutine filedir_tempdir
  !
  ! filedir_split --
  !   Computes an array whose elements are the path components in name.
  ! Arguments:
  !   filename   Name of the file to be used
  !   numberOfComponents : the number of items in the list of components
  !   numberOfChars : the number of characters for each component
  !   splitted   The array of splitted names.
  !
  subroutine filedir_split ( filename , numberOfComponents , numberOfChars , listOfComponents )
    character(len=*), intent(in)             :: filename
    integer , intent ( out ) :: numberOfComponents
    integer, intent(out) :: numberOfChars
    character, dimension (:,:), pointer :: listOfComponents
    logical :: findSeparators
    integer :: start
    integer :: component_length
    character ( len = len(filename) ) :: filename_trimmed
    integer :: filename_trimmed_length
    integer :: kseparator
    character ( len = len(filename) ) :: filename_part
    integer :: iComponent
    character :: current_char
    integer :: icharacter
    filename_trimmed = trim ( filename )
    filename_trimmed_length = len_trim ( filename_trimmed )
    !
    ! 1. Count the number of separators in the filename and
    ! the maximum length of all components
    !
    findSeparators = .true.
    start = 1
    numberOfChars = 0
    numberOfComponents = 0
    do while (findSeparators)
       filename_part (1:) = filename_trimmed (start:filename_trimmed_length)
       kseparator = filedir_first_separator_index ( filename_part )
       if ( kseparator == 0 ) then
          ! There are no separators left in the filename
          ! Compute the length of the last component
          component_length = filename_trimmed_length - start + 1
          findSeparators = .false.
       else
          component_length = kseparator - 1
          start = start + kseparator
       endif
       numberOfComponents = numberOfComponents + 1
       numberOfChars = max ( numberOfChars , component_length )
    end do
    !
    ! 2. Fill the array
    !
    allocate ( listOfComponents ( 1 : numberOfChars , 1 : numberOfComponents ) )
    !
    ! 2.1 Initialize with blanks
    !
    do iComponent = 1 , numberOfComponents
       do icharacter = 1 , numberOfChars
          listOfComponents ( icharacter , iComponent ) = " "
       enddo
    enddo
    !
    ! 2.2 Fill components
    !
    findSeparators = .true.
    start = 1
    iComponent = 0
    do while (findSeparators)
       filename_part (1:) = filename_trimmed (start:filename_trimmed_length)
       kseparator = filedir_first_separator_index ( filename_part )
       if ( kseparator == 0 ) then
          ! There are no separators left in the filename
          ! Compute the length of the last component
          component_length = filename_trimmed_length - start + 1
          findSeparators = .false.
       else
          component_length = kseparator - 1
          start = start + kseparator
       endif
       iComponent = iComponent + 1
       !
       ! 2.1 Copy the current component into the list
       !
       do icharacter = 1 , component_length
          current_char (1:) =  filename_part ( icharacter : icharacter + 1 )
          listOfComponents ( icharacter , iComponent ) = current_char
       enddo
    end do
  end subroutine filedir_split
  !
  ! filedir_first_separator_index --
  !   Returns the index of the last separator in the given filename
  !   or 0 if there is no separator in the given file name.
  ! Arguments:
  !   filename   Name of the file to be examined
  !
  integer function filedir_first_separator_index ( filename )
    character (len=1) :: separator
    character(len=*), intent(in)             :: filename
    logical , parameter :: backward = .false.
    separator = filedir_separator ()
    filedir_first_separator_index = scan( filename, separator , backward )
  end function filedir_first_separator_index
  !
  ! filedir_tempfile --
  !   The command generates a temporary file name suitable for writing to, and the 
  !   associated file. The file name will be unique, and the file will be writable and 
  !   contained in the appropriate system specific temp directory. The name of the file 
  !   will be returned as the result of the command.
  ! Arguments:
  !   tempfile   Name of the temporary file
  !
  subroutine filedir_tempfile ( tempfile )
    character(len=*), intent(out)             :: tempfile
    ! TODO : what if the temp file has less characters than the temp dir ?
    character(len=len(tempfile))             :: tempdir
    ! Number of characters for the random file tail
    integer , parameter ::  nrand_chars = 10
    character(len=nrand_chars)             :: randomFileTail
    ! The characters from which the name of the file is made of
    integer , parameter :: setsize = 36
    character, dimension(1:setsize), parameter :: characterSet = (/"a", "b", "c", "d", "e", "f", "g", "h", "i", &
     "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", &
     "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"/)
    integer :: icharacter
    ! Maximum number of tries
    integer , parameter :: maxtries = 10
    integer :: itry
    integer :: random_integer
    real :: alea
    logical :: fexist
    logical :: tempfile_done
    character (len=200) :: message
    call filedir_tempdir ( tempdir )
    !
    ! 1. Loop over the tries
    !
    tempfile_done = .false.
    call random_seed()
    do itry = 1, maxtries
       !
       ! 1.1 Computes a random file tail
       !
       do icharacter = 1, nrand_chars
          call random_number ( alea )
          random_integer = nint( alea * setsize + 1 - alea )
          randomFileTail(icharacter:icharacter) = characterSet ( random_integer )
       end do
       !
       ! 1.2 Computes a full file
       !
       tempfile = filedir_join ( tempdir , randomFileTail )
       !
       ! 1.3 See if the file allready exists
       !
       fexist = filedir_exists ( tempfile )
       if ( .NOT.fexist ) then
          ! If the file does not exist, we have found our temporary file name
          tempfile_done = .true.
          exit
       endif
    end do
    if (.NOT.tempfile_done) then
       write(message,*) "Unable to create a temporary file in directory :", tempdir
       call filedir_error ( "filedir_tempfile_name" , message )
    else
       call filedir_touch ( tempfile )
    endif
  end subroutine filedir_tempfile
  !
  ! filedir_touch --
  !   Implementation of touch. Alter the atime and mtime of the specified files. 
  ! Arguments:
  !   filename   Name of the file to touch
  !   status, optional : if supplied, it contains 0 on success or nonzero error code
  !     upon return
  !
  subroutine filedir_touch ( filename , status )
    character(len=*), intent(in)             :: filename
    integer, intent(out) , optional :: status
    integer :: local_status
    integer :: platform
    character (len= len(filename) + len(command_touch) + 1) :: command
    character (len=200) :: message
    platform = platform_get_platform ()
    select case ( platform )
    case ( PLATFORM_PLATFORM_WINDOWS )
       call filedir_touch_windows ( filename , local_status )
    case ( PLATFORM_PLATFORM_UNIX )
       write ( command , *) trim(command_touch), " ", trim(filename)
       call platform_system ( command , local_status )
    case default
       write(message,*) "Unknown touch command for platform :", platform
       call filedir_error ( "filedir_touch" , message )
    end select
    if (present ( status )) then
       status = local_status
    elseif ( local_status /=0 ) then
       write(message,*) "Unable to touch file :", trim(filename)
       call filedir_error ( "filedir_touch" , message )
    endif
  contains
    !
    ! A rough hack, based on a copy, to make something available under windows.
    ! TODO : make it modify the modification time.
    !
    subroutine filedir_touch_windows ( filename , local_status )
      character(len=*), intent(in)             :: filename
      integer, intent(out) :: local_status
      ! TODO : this will bug if the path to the temporary directory is more than 200 characters !
      character(len=len(filename))             :: tempfile
      logical :: fexist
      character (len=200) :: message
      integer :: file_unit
      fexist = filedir_exists ( filename )
      local_status = 0
      if ( .NOT. fexist ) then
         ! If the file does not exist, create it as an empty file
         file_unit = filedir_get_unit (  )
         open ( UNIT = file_unit , FILE = filename , STATUS ='UNKNOWN', IOSTAT= local_status )
         if ( local_status == 0 ) then
            close ( UNIT = file_unit , IOSTAT= local_status )
         else
            write(message,*) "Unable to touch file :", filename
            call filedir_error ( "filedir_touch" , message )
         endif
      else
         call filedir_tempfile ( tempfile )
         call filedir_delete ( tempfile )
         ! One cannot use the "light" filedir_rename, because it does not modify atime or mtime
         ! So we have to copy
         call filedir_copy ( filename , tempfile )
         call filedir_delete ( filename )
         call filedir_copy ( tempfile , filename )
         call filedir_delete ( tempfile )
      endif
    end subroutine filedir_touch_windows
  end subroutine filedir_touch
  !
  ! filedir_isdirectory --
  !   Returns .true. if file name is a directory, .false. otherwise.
  ! Arguments:
  !   dirname   Name of the directory to be examined
  !
  logical function filedir_isdirectory ( dirname )
    character(len=*), intent(in) :: dirname
    ! TODO : What if the current working directory is longer that the given directory name ?
    character ( len=len(dirname)) :: cwd
    integer :: status
    call filedir_pwd ( cwd )
    call platform_cd ( dirname , status )
    if ( status == 0 ) then
       filedir_isdirectory = .true.
    else
       filedir_isdirectory = .false.
    endif
    call platform_cd ( cwd )
  end function filedir_isdirectory
  !
  ! filedir_pathtype --
  !   Returns one of FS_PATHTYPE_ABSOLUTE, FS_PATHTYPE_RELATIVE, FS_PATHTYPE_VOLUMERELATIVE. 
  !   If name refers to a specific file on a specific volume, the path 
  !   type will be absolute. If name refers to a file relative to the current 
  !   working directory, then the path type will be relative. If name refers to 
  !   a file relative to the current working directory on a specified volume, or to 
  !   a specific file on the current working volume, then the path type is volumerelative.
  ! Arguments:
  !   filename   Name of the file to be examined
  ! Examples :
  !   "." is relative on all platforms
  !   ".." is relative on all platforms
  !   "/" is absolute on Linux/Unix
  !   "C:/" is absolute on Windows (if the C:/ exists)
  !   "/" is volumerelative on windows and refers to the current volume (for example C:/)
  !   "toto.txt" is relative on all platforms
  !   "./toto.txt" is relative on all platforms
  !
  integer function filedir_pathtype ( filename )
    character(len=*), intent(in) :: filename
    integer :: platform
    integer :: firstsep
    character(len=len(filename)) :: firstcomp
    character(len=len(filename)) :: nativename
    nativename = filedir_nativename ( filename )
    firstsep = filedir_first_separator_index ( nativename )
    if (firstsep==0) then
       ! There is no separator
       filedir_pathtype = FS_PATHTYPE_RELATIVE
    elseif (firstsep==1) then
       ! There is one separator, which is the first character
       platform = platform_get_platform ()
       if (platform==PLATFORM_PLATFORM_WINDOWS) then
          filedir_pathtype = FS_PATHTYPE_VOLUMERELATIVE
       else
          filedir_pathtype = FS_PATHTYPE_ABSOLUTE
       endif
    else
       firstcomp = nativename ( 1 : firstsep-1 )
       if ( trim(firstcomp)=="." .OR. trim(firstcomp)=="..") then
          filedir_pathtype = FS_PATHTYPE_RELATIVE
       else
          filedir_pathtype = FS_PATHTYPE_ABSOLUTE
       endif
    endif
  end function filedir_pathtype
  !
  ! filedir_mkdir --
  !   
  ! Arguments:
  !   dirname   Name of the directory to create
  !   status, optional : if supplied, it contains 0 on success or nonzero error code
  !     upon return
  !
  subroutine filedir_mkdir ( dirname , status )
    character(len=*), intent(in) :: dirname
    integer, intent(out) , optional :: status
    integer :: local_status
    character (len=len(dirname)+len(command_mkdir) + 2) :: command
    character(len=len(dirname)+26) :: message
    logical :: isdir
    isdir = filedir_isdirectory ( dirname )
    if (isdir) then
       local_status = -1
    else
       write ( command , *) trim(command_mkdir) , " ",  trim(dirname)
       call platform_system ( command , local_status )
    endif
    if ( present ( status ) ) then
       status = local_status
    elseif ( local_status /=0 ) then
       write(message,*) "Unable to make directory :", trim(dirname)
       call filedir_error ( "filedir_mkdir" , message )
    endif
  end subroutine filedir_mkdir
  !
  ! filedir_nativename --
  !   Returns the platform-specific name of the file. 
  !   This is useful if the filename is needed to pass to a platform-specific 
  !   call, such as exec under Windows or AppleScript on the Macintosh.
  ! Arguments:
  !   filename   Name of the file name to make platform-specific
  !
  function filedir_nativename ( filename )
    character(len=*), intent(in) :: filename
    character(len=len(filename)) :: filedir_nativename
    integer :: icharacter
    integer :: filelength
    character :: currentcharacter
    filelength = len(filename)
    do icharacter = 1 , filelength
       currentcharacter = filename ( icharacter : icharacter )
       if (currentcharacter=="/") then
          currentcharacter = filedir_separator ( )
       endif
       filedir_nativename ( icharacter : icharacter ) = currentcharacter
    enddo
  end function filedir_nativename
  !
  ! filedir_get_unit --
  !   Returns a free fortran unit.
  ! Arguments:
  !   no argument
  ! Note :
  !   A "free" FORTRAN unit number is an integer between 1 and MAX_UNIT_NUMBER which
  !   is not currently associated with an I/O device.  A free FORTRAN unit
  !   number is needed in order to open a file with the OPEN command.
  !
  !   If IUNIT = 0, then no free FORTRAN unit could be found, although
  !   all 99 units were checked (except for units 5, 6 and 9, which
  !   are commonly reserved for console I/O).
  !
  !   Otherwise, IUNIT is an integer between 1 and MAX_UNIT_NUMBER, representing a
  !   free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
  !   are special, and will never return those values.
  !
  !  Original Author : John Burkardt
  !
  integer function filedir_get_unit ( )
    integer :: iunit
    integer :: ios
    logical :: lopen
    logical :: unit_found
    iunit = 0
    unit_found = .false.
    filedir_get_unit = 0
    do iunit = 1, MAX_UNIT_NUMBER
       if ( iunit /= 5 .and. iunit /= 6 .and. iunit /= 9 ) then
          inquire ( UNIT = iunit, opened = lopen, iostat = ios )
          if ( ios == 0 ) then
             if ( .not. lopen ) then
                filedir_get_unit = iunit
                unit_found = .true.
                exit
             end if
          end if
       end if
    end do
    if (.NOT.unit_found) then
       call filedir_error ( "filedir_get_unit" , "No unit free." )
    endif
  end function filedir_get_unit
  !
  ! file_getallopenunits --
  !   Computes an array of integers made of all currently opened units.
  ! Output :
  !   nbunits : number of opened units
  !   units ( iunit ) : unit number for the opened unit #iunit with 1<= iunit <= nbunits
  !
  subroutine filedir_getallopenunits ( nbunits , units )
    implicit none
    integer, intent ( out ) :: nbunits
    integer , dimension(:) , pointer :: units
    integer :: iunit
    logical :: lopen
    integer :: step
    !
    ! Loop over the steps.
    ! Step #1 : count the number of opened units
    ! Step #2 : store the number of opened units
    !
    do step = 1 , 2
       nbunits = 0
       do iunit = 1, MAX_UNIT_NUMBER
          if ( iunit /= 5 .and. iunit /= 6 .and. iunit /= 9 ) then
             inquire ( UNIT = iunit, opened = lopen )
             if ( lopen ) then
                if ( step == 1 ) then
                   nbunits = nbunits + 1
                else
                   nbunits = nbunits + 1
                   units ( nbunits ) = iunit
                endif
             end if
          end if
       end do
       !
       ! At the end of step #1, allocate the array
       !
       if ( step == 1) then
          allocate ( units ( 1:nbunits ) )
       endif
    enddo
  end subroutine filedir_getallopenunits
  !
  ! filedir_displayopenunits --
  !   Displays on unit "unitnumber" the full list of opened units and their associated 
  !   filenames.
  ! Input :
  !   reportunitnumber : the unit number on which the report is displayed
  !
  subroutine filedir_displayopenunits ( reportunitnumber )
    implicit none
    integer, intent ( in ) :: reportunitnumber
    integer :: iunit
    integer :: nbunits
    integer , dimension(:) , pointer :: units
    call filedir_getallopenunits ( nbunits , units )
    write ( reportunitnumber , * ) "Number of units opened : ", nbunits
    do iunit = 1, nbunits
       write ( reportunitnumber , * ) "Unit # ", iunit , "/" , nbunits
       call filedir_reportunit ( reportunitnumber , units ( iunit ) )
    end do
    deallocate ( units )
  end subroutine filedir_displayopenunits
  !
  ! filedir_reportunit --
  !   Displays a full report on unit "unitnumber" for unit #iunit.
  ! Note :
  !   All possible features of the "inquire" intrinsic are used.
  !
  subroutine filedir_reportunit ( reportunitnumber , iunit )
    implicit none
    integer , intent (in) :: reportunitnumber
    integer , intent (in) :: iunit
    logical :: unit_exist
    logical :: unit_open
    logical :: unit_named
    integer :: unit_iostat
    integer :: unit_record_length
    integer :: unit_nextrec
    integer , parameter :: CHAR_LENGTH = 200
    character ( len= CHAR_LENGTH) :: unit_name
    character ( len= CHAR_LENGTH) :: unit_access
    character ( len= CHAR_LENGTH) :: unit_sequential
    character ( len= CHAR_LENGTH) :: unit_direct
    character ( len= CHAR_LENGTH) :: unit_form
    character ( len= CHAR_LENGTH) :: unit_formatted
    character ( len= CHAR_LENGTH) :: unit_position
    character ( len= CHAR_LENGTH) :: unit_action
    character ( len= CHAR_LENGTH) :: unit_read
    character ( len= CHAR_LENGTH) :: unit_write
    character ( len= CHAR_LENGTH) :: unit_readwrite
    character ( len= CHAR_LENGTH) :: unit_delim
    character ( len= CHAR_LENGTH) :: unit_pad
    character ( len= CHAR_LENGTH) :: unit_blank

    inquire ( UNIT = iunit, &
         iostat = unit_iostat , &
         exist = unit_exist , &
         opened = unit_open , &
         named = unit_named , &
         name = unit_name , &
         access = unit_access , &
         sequential = unit_sequential, &
         direct = unit_direct , &
         form = unit_form , &
         formatted = unit_formatted , &
         recl = unit_record_length , &
         nextrec = unit_nextrec , &
         blank = unit_blank , &
         position = unit_position , &
         action = unit_action , &
         read = unit_read , &
         write = unit_write , &
         readwrite = unit_readwrite , &
         delim = unit_delim , &
         pad = unit_pad )
    call filedir_reportunit_write_integer ( reportunitnumber , "iunit" , iunit )
    call filedir_reportunit_write_integer ( reportunitnumber , "iostat" , unit_iostat )
    call filedir_reportunit_write_logical ( reportunitnumber , "exist" , unit_exist )
    call filedir_reportunit_write_logical ( reportunitnumber , "opened" , unit_open )
    call filedir_reportunit_write_logical ( reportunitnumber , "named" , unit_named )
    if ( unit_named ) then
       call filedir_reportunit_write_character ( reportunitnumber , "filename" , trim(unit_name) )
    endif
    call filedir_reportunit_write_character ( reportunitnumber , "sequential" , trim(unit_sequential) )
    call filedir_reportunit_write_character ( reportunitnumber , "direct" , trim(unit_direct) )
    call filedir_reportunit_write_character ( reportunitnumber , "form" , trim(unit_form) )
    call filedir_reportunit_write_character ( reportunitnumber , "formatted" , trim(unit_formatted) )
    call filedir_reportunit_write_integer ( reportunitnumber , "record length" , unit_record_length )
    call filedir_reportunit_write_integer ( reportunitnumber , "last record" , unit_nextrec )
    call filedir_reportunit_write_character ( reportunitnumber , "blank" , trim(unit_blank) )
    call filedir_reportunit_write_character ( reportunitnumber , "position" , trim(unit_position) )
    call filedir_reportunit_write_character ( reportunitnumber , "action" , trim(unit_action) )
    call filedir_reportunit_write_character ( reportunitnumber , "read" , trim(unit_read) )
    call filedir_reportunit_write_character ( reportunitnumber , "write" , trim(unit_write) )
    call filedir_reportunit_write_character ( reportunitnumber , "readwrite" , trim(unit_readwrite) )
    call filedir_reportunit_write_character ( reportunitnumber , "delim" , trim(unit_delim) )
    call filedir_reportunit_write_character ( reportunitnumber , "pad" , trim(unit_pad) )
  contains
    subroutine filedir_reportunit_write_character ( reportunitnumber , key , value )
      implicit none
      integer , intent (in) :: reportunitnumber
      character (len= *), intent(in) :: key
      character (len= *), intent(in) :: value
      write ( reportunitnumber , * ) "  * ", trim(key), " : ", trim(value)
    end subroutine filedir_reportunit_write_character
    subroutine filedir_reportunit_write_integer ( reportunitnumber , key , value )
      implicit none
      integer , intent (in) :: reportunitnumber
      character (len= *), intent(in) :: key
      integer, intent(in) :: value
      write ( reportunitnumber , * ) "  * ", trim(key), " : ", value
    end subroutine filedir_reportunit_write_integer
    subroutine filedir_reportunit_write_logical ( reportunitnumber , key , value )
      implicit none
      integer , intent (in) :: reportunitnumber
      character (len= *), intent(in) :: key
      logical, intent(in) :: value
      write ( reportunitnumber , * ) "  * ", trim(key), " : ", value
    end subroutine filedir_reportunit_write_logical
  end subroutine filedir_reportunit
  !
  ! filedir_closeallopenunits --
  !   Close all currently opened units.
  !
  subroutine filedir_closeallopenunits ( )
    implicit none
    integer :: nbunits
    integer , dimension(:) , pointer :: units
    integer :: iunit
    call filedir_getallopenunits ( nbunits , units )
    do iunit = 1, nbunits
       close ( units ( iunit ) )
    enddo
  end subroutine filedir_closeallopenunits
  !
  ! filedir_error --
  !   Manage an error for the filedir module
  ! Arguments :
  !   origin : the name of the subroutine/function which generated the error.
  !   message : the message to display
  !
  subroutine filedir_error ( origin , message )
    character(len=*), intent(in) :: origin
    character(len=*), intent(in) :: message
    character(len=len(origin)+len(message)) :: directory
    write(*,*) "Internal error from: ", origin
    write(*,*) "Error: ", trim(adjustl(message))
    call filedir_pwd ( directory )
    write(*,*) "Current working directory: ", trim(directory)
    if ( filedir_stoponerror ) then
       stop
    endif
  end subroutine filedir_error
  ! 
  ! filedir_set_stoponerror --
  !   Configure the behaviour of the component whenever an 
  !   error is met.
  !   If stoponerror is true, then the execution stops if an error is encountered.
  !   If stoponerror is false, then the execution continues if an error is encountered.
  !   In both cases, a message is displayed on standard output.
  ! 
  subroutine filedir_set_stoponerror ( stoponerror )
    logical , intent(in) :: stoponerror
    filedir_stoponerror = stoponerror
  end subroutine filedir_set_stoponerror
end module m_filedir

