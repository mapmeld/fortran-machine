!
! m_vstrplatform.f90 --
!   The m_vstrplatform module provides several routines, 
!   which take dynamic strings arguments, to interact
!   with the operating system.
!
!  OVERVIEW
!    The goal of this module is to provide system access with 
!    commands which take dynamic strings as arguments.
!    It is based on m_platform, which takes character-based arguments.
!
!    In the following example, extracted from the m_vfile module 
!    included in flibs, one sets the current working directory
!    by using vstrplatform_cd on a dynamic string variable.
!    
!        type ( t_vstring ) :: dirname
!        integer :: status
!        call vstrplatform_cd ( dirname , status )
!
!    This is a sketch of available routines :
!     vstrplatform_system                       Executes an external command on the system
!     vstrplatform_get_environment_variable     Get to one environment variable
!     vstrplatform_cd                           Change the system current directory
!     vstrplatform_stat                         Get status of a file
!
!   Dynamic or static buffer
!
!   The internal algorithms provided by m_vstrplatform are based on 
!   basic fortran character strings. In several situations, the 
!   dynamic vstring has to be converted into a basic fortran character
!   buffer string, which size has to be given explicitely in the source 
!   code, with the "len = <something>" statement (in the 
!   character ( len = <something>) ). 
!   If the _VSTRPLATFORM_STATIC_BUFFER macro is defined, then character strings of 
!   constant size VSTRPLATFORM_MAXIMUM_ENVVAR_LENGTH are used as buffers.
!   If the _VSTRPLATFORM_STATIC_BUFFER macro is not defined (which is the default), 
!   then character strings of dynamic size are used as buffers 
!   with the fortran 90 "len = vstring_length(this)" statement.
!
!   The second solution is more efficient, because the strings are not 
!   oversized or undersized, depending on the real number of characters
!   in the dynamic string. But the feature may not be provided 
!   by the compiler at hand. For example, problems with the dynamic 
!   length character string have been experienced with Intel Fortran 8.
!
!   Preprocessing
!   The following preprocessing macro may be defined :
!   _VSTRPLATFORM_STATIC_BUFFER : see  the section "Dynamic or static buffer"
!
! Copyright (c) 2008 Arjen Markus arjenmarkus@sourceforge.net
! Copyright (c) 2008 Michael Baudin michael.baudin@gmail.com
!
! $Id: m_vstrplatform.f90,v 1.4 2008/07/16 13:47:06 relaxmike Exp $
!
module m_vstrplatform
  use m_platform, only : &
       platform_system , &
       platform_cd , &
       platform_stat , &
       platform_osstring ,&
       platform_platformstring , & 
       platform_get_os , &
       platform_get_platform , &
       platform_get_environment_variable , &
       PLATFORM_OS_WINDOWS_95, &
       PLATFORM_OS_WINDOWS_NT, &
       PLATFORM_OS_MACOS , &
       PLATFORM_OS_SUNOS , &
       PLATFORM_OS_LINUX , &
       PLATFORM_OS_UNIX , &
       PLATFORM_PLATFORM_WINDOWS , &
       PLATFORM_PLATFORM_MAC , &
       PLATFORM_PLATFORM_UNIX
  use m_vstring, only : &
       t_vstring, &
       vstring_new , &
       vstring_free , &
       vstring_cast , &
       vstring_length , &
       vstring_trim, &
       vstring_append
  implicit none
  private
  !
  ! Public static methods
  !
  public :: vstrplatform_system
  public :: vstrplatform_get_environment_variable
  public :: vstrplatform_cd
  public :: vstrplatform_stat
  public :: vstrplatform_osstring
  public :: vstrplatform_platformstring
  !
  ! Constants
  !
  ! Maximum number of characters in an environment variable
  integer , parameter :: VSTRPLATFORM_MAXIMUM_ENVVAR_LENGTH = 10000
  !
  ! vstrplatform_get_environment_variable --
  !   Generic interface to get the value of an environment variable.
  !
  interface vstrplatform_get_environment_variable
     module procedure vstrplatform_getenvvar_vstring
     module procedure vstrplatform_getenvvar_charstring
  end interface vstrplatform_get_environment_variable
  !
  ! vstrplatform_cd --
  !   Generic interface to change the directory
  !
  interface vstrplatform_cd
     module procedure vstrplatform_cd_vstring
     module procedure vstrplatform_cd_charstring
  end interface vstrplatform_cd
  !
  ! Maximum number of characters in the buffer.
  !
  integer , parameter :: VSTRPLATFORM_BUFFER_SIZE = 1000
contains
  !
  ! vstrplatform_system --
  !   Consider the current string as a command and executes it 
  !   under the operating system.
  !   
  ! Arguments:
  !   status, optional : if supplied, it contains 0 on success or nonzero error code
  !     upon return
  !
  subroutine vstrplatform_system ( command , status )
    type ( t_vstring ), intent(in) :: command
    integer, intent(out) , optional :: status
#ifdef _VSTRPLATFORM_STATIC_BUFFER
    character ( len = VSTRPLATFORM_BUFFER_SIZE ) :: command_charstring
#else
    character(len=vstring_length( command)) :: command_charstring
#endif
    call vstring_cast ( command , command_charstring )
    call platform_system ( command_charstring , status )
  end subroutine vstrplatform_system
  !
  ! vstrplatform_osstring --
  !   Returns a string containing the current operating system running 
  !   on the current machine, one of "Windows 95", "Windows NT", "MacOS", "SunOS", 
  !   "Linux" or "Unix".
  !
  ! Arguments:
  !   currentos, output : the current operating system string
  function vstrplatform_osstring ( ) result ( currentos )
    type ( t_vstring ) :: currentos
    integer :: platform_os
    type ( t_vstring ) :: message
    platform_os = platform_get_os ( )
    select case ( platform_os )
    case ( PLATFORM_OS_WINDOWS_95 )
       call vstring_new ( currentos , "Windows 95" )
    case ( PLATFORM_OS_WINDOWS_NT )
       call vstring_new ( currentos , "Windows NT" )
    case ( PLATFORM_OS_MACOS )
       call vstring_new ( currentos , "MacOS" )
    case ( PLATFORM_OS_SUNOS )
       call vstring_new ( currentos , "SunOS" )
    case ( PLATFORM_OS_LINUX )
       call vstring_new ( currentos , "Linux" )
    case ( PLATFORM_OS_UNIX )
       call vstring_new ( currentos , "Unix" )
    case default
       call vstring_new ( message , "Unknown operating system in vstrplatform_osstring." )
       call vstrplatform_error ( message )
       call vstring_free ( message )
    end select
  end function vstrplatform_osstring
  !
  ! vstrplatform_platformstring --
  !   Returns a string containing the current platform running on the current machine,
  !   one of "Windows", "Mac", "Unix".
  ! Arguments:
  !   currentplatform, output : the current platform string
  function vstrplatform_platformstring (  ) result ( currentplatform )
    type ( t_vstring ) :: currentplatform
    integer :: platform_platform
    type ( t_vstring ) :: message
    platform_platform = platform_get_platform ()
    select case ( platform_platform )
    case ( PLATFORM_PLATFORM_WINDOWS )
       call vstring_new ( currentplatform , "Windows" )
    case ( PLATFORM_PLATFORM_MAC )
       call vstring_new ( currentplatform , "Mac" )
    case ( PLATFORM_PLATFORM_UNIX )
       call vstring_new ( currentplatform , "Unix" )
    case default
       call vstring_new ( message , "Unknown platform in vstrplatform_platformstring." )
       call vstrplatform_error ( message )
       call vstring_free ( message )
    end select
  end function vstrplatform_platformstring
  !
  ! vstrplatform_getenvvar_vstring --
  !   Returns the value of the environment variable envvar.
  ! Arguments:
  !   envvar : the name of the environment variable to get
  function vstrplatform_getenvvar_vstring ( envvar ) result ( value )
    type ( t_vstring ), intent(in) :: envvar
    type ( t_vstring ) :: value
    type ( t_vstring ) :: value_long
#ifdef _VSTRPLATFORM_STATIC_BUFFER
    character ( len = VSTRPLATFORM_BUFFER_SIZE ) :: envvar_charstring
#else
    character(len=vstring_length(envvar)) :: envvar_charstring
#endif
    ! TODO : remove the limitation that the environement variable is declared with a static size.
    character ( len=VSTRPLATFORM_MAXIMUM_ENVVAR_LENGTH) :: value_charstring
    call vstring_cast ( envvar , envvar_charstring )
    call platform_get_environment_variable ( envvar_charstring , value_charstring )
    call vstring_new ( value_long , value_charstring )
    value = vstring_trim ( value_long )
    call vstring_free ( value_long )
  end function vstrplatform_getenvvar_vstring
  !
  ! vstrplatform_getenvvar_charstring --
  !   Returns the value of the environment variable envvar.
  ! Arguments:
  !   envvar : the name of the environment variable to get
  function vstrplatform_getenvvar_charstring ( envvar ) result ( value )
    character(len=*), intent(in) :: envvar
    type ( t_vstring ) :: value
    type ( t_vstring ) :: envvar_string
    call vstring_new ( envvar_string , envvar )
    value = vstrplatform_getenvvar_vstring ( envvar_string )
    call vstring_free ( envvar_string )
  end function vstrplatform_getenvvar_charstring
  !
  ! vstrplatform_cd_vstring --
  !   Change working directory
  ! Arguments:
  !   filename   Name of the directory in which to enter
  !   status, optional : if supplied, it contains 0 on success or nonzero error code
  !      upon return
  !
  subroutine vstrplatform_cd_vstring ( dirname , status )
    type(t_vstring), intent(in) :: dirname
    integer, intent(out) , optional :: status
    integer :: local_status
    type ( t_vstring ) :: message
#ifdef _VSTRPLATFORM_STATIC_BUFFER
    character ( len = VSTRPLATFORM_BUFFER_SIZE ) :: dirname_charstring
#else
    character(len=vstring_length(dirname)) :: dirname_charstring
#endif
    call vstring_cast ( dirname , dirname_charstring )

    call platform_cd ( dirname_charstring , local_status )

    if ( present ( status ) ) then
       status = local_status
    elseif ( local_status /=0 ) then
       call vstring_new ( message , "Error while changing directory in vstrplatform_cd_vstring." )
       call vstrplatform_error ( message )
       call vstring_free ( message )
    endif
  end subroutine vstrplatform_cd_vstring
  !
  ! vstrplatform_cd_charstring --
  !   Change working directory
  ! Arguments:
  !   filename   Name of the directory in which to enter
  !   status, optional : if supplied, it contains 0 on success or nonzero error code
  !      upon return
  !
  subroutine vstrplatform_cd_charstring ( dirname , status )
    character(len=*), intent(in) :: dirname
    integer, intent(out) , optional :: status
    call platform_cd ( dirname , status )
  end subroutine vstrplatform_cd_charstring
  !
  ! vstrplatform_stat --
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
  subroutine vstrplatform_stat ( filename , statarray , status )
    type(t_vstring), intent(in)             :: filename
    integer, dimension (1:13) , intent(out)  :: statarray
    integer, intent(out) , optional :: status
    integer :: local_status
    type ( t_vstring ) :: message
#ifdef _VSTRPLATFORM_STATIC_BUFFER
    character ( len = VSTRPLATFORM_BUFFER_SIZE ) :: filename_charstring
#else
    character(len=vstring_length(filename)) :: filename_charstring
#endif
    call vstring_cast ( filename , filename_charstring )
    call platform_stat ( filename_charstring , statarray , local_status )
    if ( present ( status ) ) then
       status = local_status
    elseif ( local_status /=0 ) then
       call vstring_new ( message , "Error while computing stat for file ")
       call vstring_append ( message , filename )
       call vstring_append ( message , " in vstrplatform_stat." )
       call vstrplatform_error ( message )
       call vstring_free ( message )
    endif
  end subroutine vstrplatform_stat
  !
  ! vstrplatform_error --
  !   Manage an error for the filedir module
  ! Arguments :
  !   origin : the name of the subroutine/function which generated the error.
  !   message : the message to display
  !
  subroutine vstrplatform_error ( message )
    type(t_vstring), intent(in) :: message
#ifdef _VSTRPLATFORM_STATIC_BUFFER
    character ( len = VSTRPLATFORM_BUFFER_SIZE ) :: message_charstring
#else
    character(len=vstring_length(message)) :: message_charstring
#endif
    call vstring_cast ( message , message_charstring )
    write(*,*) "Error in m_vstrplatform."
    write(*,*) "Message: ", message_charstring
    stop
  end subroutine vstrplatform_error
end module m_vstrplatform

