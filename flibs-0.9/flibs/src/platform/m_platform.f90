!
! m_platform.f90 --
!   The m_platform module contains parameters to identify the
!   platform the program is running on and provides 
!   several routines to interact with the operating system.
!
!   OVERVIEW
!
!   The goal is to make the client source code as independent as possible
!   from the particular environment, that is 
!   - independent from the operating system (OS), so that the 
!   binaries can be used under Windows, Linux or any other
!   specific OS without modifying the source code,
!   - independent from the fortran compiler, so that the 
!   binaries can be generated with gfortran Intel Fortran, Absoft,
!   or any other specific compiler without modifying the source code.
!
!   Very often, the features implemented in m_platform are developped
!   directly in the client source code, leading to confusing algorithm
!   and unnecessary complexity. Here the dependencies are concentrated
!   in m_platform so that the client code is much simpler and clearer.
!
!   The function platform_get_os() returns an integer which identifies 
!   the operating system and the function platform_get_platform() identifies 
!   the general category.
!   For example, the file separator is different on windows ("\"),
!   unix ("/") and Mac (":"). In the following example, extracted from the 
!   m_vfile module included with flibs, the platform_get_platform()
!   function is used to configure the separator for the current platform.
!
!     use m_platform, only : &
!       platform_get_platform , &
!       PLATFORM_PLATFORM_WINDOWS ,&
!       PLATFORM_PLATFORM_UNIX , &
!       PLATFORM_PLATFORM_MAC
!     integer :: platform
!     character (len=1) :: VFILE_PLATFORM_SEPARATOR
!     platform = platform_get_platform ()
!     select case ( platform )
!     case ( PLATFORM_PLATFORM_WINDOWS )
!       VFILE_PLATFORM_SEPARATOR = VFILE_PLATFORM_SEPARATOR_WINDOWS
!     case ( PLATFORM_PLATFORM_UNIX )
!       VFILE_PLATFORM_SEPARATOR = VFILE_PLATFORM_SEPARATOR_UNIX
!     case ( PLATFORM_PLATFORM_MAC )
!       VFILE_PLATFORM_SEPARATOR = VFILE_PLATFORM_SEPARATOR_MAC
!     case default
!       print *, "I come from Vega."
!       return
!     end select
!
!   The subroutine platform_system() allows to execute an external program at the 
!   system level. This routine is generally provided by the fortran compiler as an
!   extension to standard fortran. But some compilers provide the feature
!   as a subroutine (for example gfortran), while other compilers provide the 
!   feature as a function (for example Intel Fortran). In the following example,
!   one execute a Monte-Carlo simulation with no dependency on the specific 
!   compiler.
! 
!      use m_platform, only platform_system
!      call platform_system ( "montecarlo.exe" , status )
!
!   This is a sketch of available routines :
!
!     platform_system                       Executes an external command on the system
!     platform_get_os                       Returns the current operating system
!     platform_get_platform                 Returns the current platform
!     platform_get_environment_variable     Get to one environment variable
!     platform_cd                           Change the system current directory
!     platform_stat                         Get status of a file
!
!   Pre-processing macros
!
!   The source code of m_platform is based on pre-processing macro, 
!   which must be configured for the specific couple (OS,compiler) at use.
!   With most compilers, defining a pre-processing macro simply 
!   consists in enabling the pre-processing with a specific 
!   option and adding "-D<macro>" options on the command-line.
!
!   The only mandatory pre-processing macro which must be defined is 
!   the _PLATFORM_OS_<your OS> macro.
!   Optionnaly, other pre-processing macros may be defined so that 
!   the client code may access to additionnal features.
!   If a feature is used and the associated macros have not 
!   been defined, the "status" integer of the associated routine
!   will have the value PLATFORM_ERROR_UNDEFINED_SERVICE.
!
!   Compile
!
!   The "make" directory provided with flibs should help the 
!   use to compile m_platform. The "make/makefile" contains all the 
!   makefiles necessary for the project, include specific settings
!   for several compilers. the "make/visualstudio" directory include 
!   all projects .nfproj and solutions .sln files necessary to 
!   compile the project with Intel Fortran 8 and Visual Studio 2003.
!
! Operating System dependency
!
! The m_platform module MUST be informed of the specific OS for which 
! it is compiled. One of the following pre-processing macros MUST be 
! defined to set the spefic OS at use :
! _PLATFORM_OS_WINDOWS_95
! _PLATFORM_OS_WINDOWS_NT
! _PLATFORM_OS_MAC
! _PLATFORM_OS_SUN
! _PLATFORM_OS_LINUX
! _PLATFORM_OS_UNIX
!
! System fortran extension
!
! The SYSTEM fortran extension allows to execute an external program.
! Depending on the compiler, the SYSTEM fortran extension is provided
! as a subroutine or a function. The m_platform module MAY be informed
! of the particular version of the SYSTEM extension at use and one
! of the following pre-processing macro must be defined :
! _PLATFORM_SYSTEM_SUBROUTINE
! _PLATFORM_SYSTEM_FUNCTION
! See in your compiler manual for the specific settings.
! For example, this is a short list of compilers and the
! SYSTEM provided :
! - function : Intel Fortran, g95.
! - subroutine : gfortran,
!
! Environment variables extension
! The fortran 2003 standard introduces a standard way of accessing
! to the environment variables. Older compilers does not match
! that standard but provide extensions to access to environment variables.
! To inform the m_platform module of the particular environment
! variable extension, one of the following pre-processing macro MAY
! be defined :
! _PLATFORM_INTEL_FORTRAN_PORTABILITY_ROUTINES
! _PLATFORM_FORTRAN_2003
!
! Change directory fortran extension
! Depending on the compiler, the "CHDIR" fortran extension is provided 
! as a subroutine or a function. 
! To inform the m_platform module of the particular CHDIR extension, 
! one of the following pre-processing macro MAY be defined :
! _PLATFORM_CHDIR_SUBROUTINE
! _PLATFORM_CHDIR_FUNCTION
! See in your manual for the specific 
! settings.
! For example, this is a short list of compilers and their particular 
! CHDIR provided :
! - function : Intel Fortran, g95, gfortran
! - subroutine : gfortran
!
! File stat fortran extension
! Depending on the compiler, the "STAT" fortran extension is 
! provided as a subroutine or a function.
! For example, this is a short list of compilers and their particular 
! STAT provided :
! - function : Intel Fortran, g95
! - subroutine : gfortran
! To inform the m_platform module of the particular STAT extension, 
! one of the following pre-processing macro MAY be defined :
! _PLATFORM_STAT_SUBROUTINE
! _PLATFORM_STAT_FUNCTION
!
! Example of compiler settings
! This is an abstract of all pre-processing macros for several compilers.
!
! Compiler : gfortran
! _PLATFORM_FORTRAN_2003
! _PLATFORM_CHDIR_SUBROUTINE
! _PLATFORM_STAT_SUBROUTINE
! _PLATFORM_SYSTEM_SUBROUTINE
!
! Compiler : Intel Fortran
! _PLATFORM_INTEL_FORTRAN_PORTABILITY_ROUTINES
! _PLATFORM_CHDIR_FUNCTION
! _PLATFORM_STAT_FUNCTION
! _PLATFORM_SYSTEM_FUNCTION
!
! Compiler : g95
! _PLATFORM_FORTRAN_2003
! _PLATFORM_CHDIR_FUNCTION
! _PLATFORM_STAT_FUNCTION
! _PLATFORM_SYSTEM_FUNCTION
!
! Copyright (c) 2008 Arjen Markus arjenmarkus@sourceforge.net
! Copyright (c) 2008 Michael Baudin michael.baudin@gmail.com
!
! $Id: m_platform.f90,v 1.7 2008/06/17 12:46:07 relaxmike Exp $
!
module m_platform
#ifdef _PLATFORM_INTEL_FORTRAN_PORTABILITY_ROUTINES
  use ifport
#endif
  implicit none
  private
  !
  ! The name of the operating system running on this machine, such as Windows 95,
  ! Windows NT, MacOS, or SunOS. On UNIX machines, this is the value returned by
  ! uname -s. 
  !
  integer, parameter, public :: PLATFORM_OS_UNKNOWN = 0
  integer, parameter, public :: PLATFORM_OS_WINDOWS_95 = 1
  integer, parameter, public :: PLATFORM_OS_WINDOWS_NT = 2
  integer, parameter, public :: PLATFORM_OS_MACOS = 3
  integer, parameter, public :: PLATFORM_OS_SUNOS = 4
  integer, parameter, public :: PLATFORM_OS_LINUX = 5
  integer, parameter, public :: PLATFORM_OS_UNIX = 6
  integer, parameter, public :: PLATFORM_OS_NB = 6
#ifdef _PLATFORM_OS_WINDOWS_95
  integer, parameter :: PLATFORM_OS = PLATFORM_OS_WINDOWS_95
#endif
#ifdef _PLATFORM_OS_WINDOWS_NT
  integer, parameter :: PLATFORM_OS = PLATFORM_OS_WINDOWS_NT
#endif
#ifdef _PLATFORM_OS_MAC
  integer, parameter :: PLATFORM_OS = PLATFORM_OS_MACOS
#endif
#ifdef _PLATFORM_OS_SUN
  integer, parameter :: PLATFORM_OS = PLATFORM_OS_SUNOS
#endif
#ifdef _PLATFORM_OS_LINUX
  integer, parameter :: PLATFORM_OS = PLATFORM_OS_LINUX
#endif
#ifdef _PLATFORM_OS_UNIX
  integer, parameter :: PLATFORM_OS = PLATFORM_OS_UNIX
#endif
  !
  ! Either windows, macintosh, or unix. This identifies the general operating environment of the machine.
  !
  integer, parameter, public :: PLATFORM_PLATFORM_UNKNOWN = 0
  integer, parameter, public :: PLATFORM_PLATFORM_WINDOWS = 1
  integer, parameter, public :: PLATFORM_PLATFORM_MAC = 2
  integer, parameter, public :: PLATFORM_PLATFORM_UNIX = 3
  integer, parameter, public :: PLATFORM_PLATFORM_NB = 3
#ifdef _PLATFORM_OS_WINDOWS_95
  integer, parameter :: PLATFORM_PLATFORM = PLATFORM_PLATFORM_WINDOWS
#endif
#ifdef _PLATFORM_OS_WINDOWS_NT
  integer, parameter :: PLATFORM_PLATFORM = PLATFORM_PLATFORM_WINDOWS
#endif
#ifdef _PLATFORM_OS_MAC
  integer, parameter :: PLATFORM_PLATFORM = PLATFORM_PLATFORM_MAC
#endif
#ifdef _PLATFORM_OS_SUN
  integer, parameter :: PLATFORM_PLATFORM = PLATFORM_PLATFORM_UNIX
#endif
#ifdef _PLATFORM_OS_LINUX
  integer, parameter :: PLATFORM_PLATFORM = PLATFORM_PLATFORM_UNIX
#endif
#ifdef _PLATFORM_OS_UNIX
  integer, parameter :: PLATFORM_PLATFORM = PLATFORM_PLATFORM_UNIX
#endif
  !
  ! Public static methods
  !
  public :: platform_system
  public :: platform_get_os
  public :: platform_get_platform
  public :: platform_get_environment_variable
  public :: platform_cd
  public :: platform_stat
  public :: platform_osstring
  public :: platform_platformstring
  !
  ! Tags to manage errors.
  !
  integer , parameter, public :: PLATFORM_ERROR_OK = 0
  integer , parameter, public :: PLATFORM_ERROR_UNDEFINED_SERVICE = 1

contains
  !
  ! platform_system  --
  !   This subroutine allows to pass commands to the system
  !   and returns the execution status.
  !   Provides an interface to the system fortran extension.
  ! Arguments:
  !   command : Command to run (note: this is quite likely platform-dependent)
  !   status, optional : if supplied, it contains 0 on success or nonzero error code
  !     upon return. Notice that the information contained
  !     in it is not very reliable - some systems do not give any information.
  ! Caution !
  !   Under Windows the "call system" may generate the display of a console.
  !
  subroutine platform_system ( command , status )
    character (len=*), intent(in) :: command
    integer, intent ( out ), optional :: status
    integer :: local_status
#ifdef _PLATFORM_SYSTEM_SUBROUTINE
    call system ( command , local_status )
#endif
#ifdef _PLATFORM_SYSTEM_FUNCTION
    local_status = system ( command )
#endif
    !
    ! Case when no macro is defined
    !
#ifndef _PLATFORM_SYSTEM_SUBROUTINE
#ifndef _PLATFORM_SYSTEM_FUNCTION
    if ( present ( status ) ) then
       status = PLATFORM_ERROR_UNDEFINED_SERVICE
    else
       write(message,*) "The system service is not provided."
       call platform_error ( "platform_system" , message )
    endif
#endif
#endif
    if (present(status)) then
       status = local_status
    endif
  end subroutine platform_system
  !
  ! platform_get_os --
  !   Returns the operating system running on the current machine, 
  !   one of: PLATFORM_OS_WINDOWS_95, PLATFORM_OS_WINDOWS_NT,
  !   PLATFORM_OS_MACOS, PLATFORM_OS_SUNOS, PLATFORM_OS_LINUX,
  !   PLATFORM_OS_UNIX.
  !   The actual integer value should not be used directly ; instead, it 
  !   should be compared against the PLATFORM_OS_* public variables.
  ! Arguments:
  !   No argument
   function platform_get_os ( ) result ( currentos )
     integer :: currentos
    currentos = PLATFORM_OS
  end function platform_get_os
  !
  ! platform_osstring --
  !   Returns a string containing the current operating system running 
  !   on the current machine, one of "Windows 95", "Windows NT", "MacOS", "SunOS", 
  !   "Linux" or "Unix".
  ! Arguments:
  !   currentos, output : the current operating system string
  subroutine platform_osstring ( currentos )
    character (len=*) , intent(out) :: currentos
    select case ( PLATFORM_OS )
    case ( PLATFORM_OS_WINDOWS_95 )
       currentos = "Windows 95"
    case ( PLATFORM_OS_WINDOWS_NT )
       currentos = "Windows NT"
    case ( PLATFORM_OS_MACOS )
       currentos = "MacOS"
    case ( PLATFORM_OS_SUNOS )
       currentos = "SunOS"
    case ( PLATFORM_OS_LINUX )
       currentos = "Linux"
    case ( PLATFORM_OS_UNIX )
       currentos = "Unix"
    case default
       call platform_error ( "platform_osstring" , "Unknown operating system." )
    end select
  end subroutine platform_osstring
  !
  ! platform_get_platform --
  !   Returns the general operating system running on the current machine, 
  !   one of: PLATFORM_PLATFORM_WINDOWS, PLATFORM_PLATFORM_MAC, PLATFORM_PLATFORM_UNIX
  !   The actual integer value should not be used directly ; instead, it 
  !   should be compared against the PLATFORM_PLATFORM_* public variables.
  ! Arguments:
  !   No argument
   function platform_get_platform ( ) result ( currentplatform )
     integer :: currentplatform
     currentplatform = PLATFORM_PLATFORM
  end function platform_get_platform
  !
  ! platform_platformstring --
  !   Returns a string containing the current platform running on the current machine,
  !   one of "Windows", "Mac", "Unix".
  ! Arguments:
  !   currentplatform, output : the current platform string
  subroutine platform_platformstring ( currentplatform )
    character (len=*) , intent(out) :: currentplatform
    select case ( PLATFORM_PLATFORM )
    case ( PLATFORM_PLATFORM_WINDOWS )
       currentplatform = "Windows"
    case ( PLATFORM_PLATFORM_MAC )
       currentplatform = "Mac"
    case ( PLATFORM_PLATFORM_UNIX )
       currentplatform = "Unix"
    case default
       call platform_error ( "platform_platformstring" , "Unknown platform." )
    end select
  end subroutine platform_platformstring
  !
  ! platform_get_environment_variable --
  !   Returns the operating system running on the current machine
  ! Arguments:
  !   No argument
  subroutine platform_get_environment_variable ( envvar , value )
    character (len=*), intent(in) :: envvar
    character (len=*), intent(out) :: value
#ifdef _PLATFORM_INTEL_FORTRAN_PORTABILITY_ROUTINES
    integer :: environment_variable_length
#endif
#ifdef _PLATFORM_FORTRAN_2003
    call get_environment_variable ( envvar , value )
#endif
#ifdef _PLATFORM_INTEL_FORTRAN_PORTABILITY_ROUTINES
    environment_variable_length =  GETENVQQ ( envvar , value )
#endif
    !
    ! Case when no macro is defined
    !
#ifndef _PLATFORM_FORTRAN_2003
#ifndef _PLATFORM_INTEL_FORTRAN_PORTABILITY_ROUTINES
    if ( present ( status ) ) then
       status = PLATFORM_ERROR_UNDEFINED_SERVICE
    else
       write(message,*) "The get_environment_variable service is not provided."
       call platform_error ( "platform_get_environment_variable" , message )
    endif
#endif
#endif
  end subroutine platform_get_environment_variable
  !
  ! platform_cd --
  !   Change working directory to "dirname". 
  ! Arguments:
  !   dirname   Name of the directory in which to enter
  !   status, optional : if supplied, it contains 0 on success or nonzero error code
  !      upon return
  !
  subroutine platform_cd ( dirname , status )
    character(len=*), intent(in)             :: dirname
    integer, intent(out) , optional :: status
    integer :: local_status
#ifdef _PLATFORM_CHDIR_SUBROUTINE
    call chdir ( dirname , local_status )
#endif
#ifdef _PLATFORM_CHDIR_FUNCTION
    local_status = chdir ( dirname )
#endif
    !
    ! Case when no macro is defined
    !
#ifndef _PLATFORM_CHDIR_SUBROUTINE
#ifndef _PLATFORM_CHDIR_FUNCTION
    if ( present ( status ) ) then
       status = PLATFORM_ERROR_UNDEFINED_SERVICE
    else
       write(message,*) "The cd service is not provided."
       call platform_error ( "platform_cd" , message )
    endif
#endif
#endif
    if (present ( status )) then
       status = local_status
    endif
  end subroutine platform_cd
  !
  ! platform_stat --
  !   Get status of a file.
  ! Arguments:
  !   filename   Name of the file
  !   statarray  Array of size 13,
  !       statarray(1) Device ID
  !       statarray(2) Inode number
  !       statarray(3) File mode
  !       statarray(4) Number of links
  !       statarray(5) Owner’s uid
  !       statarray(6) Owner’s gid
  !       statarray(7) ID of device containing directory entry for file (0 if not available)
  !       statarray(8) File size (bytes)
  !       statarray(9) Last access time
  !       statarray(10) Last modification time
  !       statarray(11) Last file status change time
  !       statarray(12) Preferred I/O block size (-1 if not available)
  !       statarray(13) Number of blocks allocated (-1 if not available)
  !   status, optional : if supplied, it contains 0 on success or nonzero error code
  !      upon return
  !
  subroutine platform_stat ( filename , statarray , status )
    character(len=*), intent(in)             :: filename
    integer, dimension (1:13) , intent(out)  :: statarray
    integer, intent(out) , optional :: status
    integer  :: local_status
#ifdef _PLATFORM_STAT_SUBROUTINE
    call stat ( filename , statarray , local_status )
#endif
#ifdef _PLATFORM_STAT_FUNCTION
    local_status = stat ( filename , statarray )
#endif
    !
    ! Case when no macro is defined
    !
#ifndef _PLATFORM_STAT_SUBROUTINE
#ifndef _PLATFORM_STAT_FUNCTION
    if ( present ( status ) ) then
       status = PLATFORM_ERROR_UNDEFINED_SERVICE
    else
       write(message,*) "The stat service is not provided."
       call platform_error ( "platform_stat" , message )
    endif
#endif
#endif
    if (present ( status )) then
       status = local_status
    endif
  end subroutine platform_stat
  !
  ! platform_error --
  !   Manage an error for the m_platform module
  ! Arguments :
  !   origin : the name of the subroutine/function which generated the error.
  !   message : the message to display
  !
  subroutine platform_error ( origin , message )
    character(len=*), intent(in) :: origin
    character(len=*), intent(in) :: message
    write(6,*) "Error in m_platform."
    write(6,*) "Origin: ", trim(origin)
    write(6,*) "Error: ", trim(adjustl(message))
    stop
  end subroutine platform_error
end module m_platform

