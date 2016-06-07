! m_logger.f90 --
!
!   The module m_logger provides static methods to manage a log file, 
!   which is an execution report of the program.
!
! Overview
!
!   The goal of this component is to provide a way to write messages
!   both on standard output and on a log file, so that a trace 
!   of the execution can be read by the user after the execution.
!   The module m_logger therefore provides static methods to 
!   - connect a file to the logger,
!   - configure the logging process, for example disable the standard
!     output messages, 
!   - log messages.
!
!   The logger must be started up with "log_startup()" and shut down
!   with "log_shutdown()".
!   The static method "log_startup" takes the log file name as first argument :
!   it main purpose is to connect the logger to the file.
!   The messages are sent to the logger with the static method "log_msg".
!
!   In the following example, extracted from the unit tests of m_logger
!   provided with the project, one connects the file "test_m_logger.log" to the
!   logger, send several messages and shut down the logging system.
!
!      call log_startup ( 'test_m_logger.log' )
!      call log_msg ( 'First message' )
!      call log_msg ( 'Second message' )
!      call log_shutdown ()
!
!    By default, the logging is done both on file and on standard output.
!    The user may want to configure the behaviour of the logger so that message
!    are not written on standard output.
!    The static method "log_configure(option,value)" is the central point to configure the 
!    logger. It takes a character "option" string and a "value" as arguments.
!    In the following example, one selectively writes 
!    messages on standard output or on file, or both.
!
!      call log_startup ( 'test_m_logger.log' )
!      call log_configure ( "writeonstdout" , .false. )
!      call log_msg( 'This message is written only on file' )
!      call log_configure ( "writeonlogfile" , .false. )
!      call log_msg( 'This message is written nowhere' )
!      call log_configure ( "writeonstdout" , .true. )
!      call log_msg( 'This message is written only on screen' )
!      call log_configure ( "writeonlogfile" , .true. )
!      call log_msg( 'This message is written both on screen and on file' )
!      call log_shutdown ()
!
! TODO
!
! Author: Michael Baudin, 2008, michael.baudin@gmail.com
! Changes: Arjen Markus, 2008, arjenmarkus@sourceforge.net
!
!     $Id: m_logger.f90,v 1.3 2008/06/18 08:55:45 relaxmike Exp $
!
module m_logger
  implicit none
  private
  public :: log_msg
  public :: log_startup
  public :: log_shutdown
  public :: log_get_unit
  public :: log_delimiter
  public :: log_get_delimiter
  public :: log_isinitialized
  public :: log_set_stoponerror
  public :: log_configure
  public :: log_cget
  public :: log_reset
  !
  ! Deprecated methods
  !
  public :: log_inactivate_file
  public :: log_activate_file
  public :: log_inactivate_screen
  public :: log_activate_screen
  public :: log_init
  !
  ! Static fields
  !
  ! Logical unit associated with the log file
  integer :: log_fileunit = 6
  ! Logical unit associated with the standard output
  integer :: log_stdout = -1
  ! Logical set to false if the user wants to inactivate
  ! the logger ouput to screen
  ! The default value is true (logger activated).
  logical :: activate_screen = .true.
  ! Set to true to activate the logging into the file
  logical :: activate_file = .true.
  ! Set to true to include a timestamp
  logical :: log_timestamp = .false.
  ! Set to true when the logger is allready started up.
  logical :: logger_initialized = .false.
  !
  ! Strings used as delimiters
  !
  integer , parameter , public :: LOG_LEVEL_DELIMITER_LENGTH = 50
  character (len=LOG_LEVEL_DELIMITER_LENGTH) :: log_level_string_volume = "==============="
  character (len=LOG_LEVEL_DELIMITER_LENGTH) :: log_level_string_chapter = "---------------"
  character (len=LOG_LEVEL_DELIMITER_LENGTH) :: log_level_string_section = "***************"
  character (len=LOG_LEVEL_DELIMITER_LENGTH) :: log_level_string_subsection = "+++++++++++++++"
  !
  ! Set to true to stop whenever an error comes in the component.
  !
  logical, save :: logger_stoponerror = .true.
  !
  ! List of available delimiters levels
  !
  integer , parameter , public :: LOG_LEVEL_VOLUME = 1
  integer , parameter , public :: LOG_LEVEL_CHAPTER = 2
  integer , parameter , public :: LOG_LEVEL_SECTION = 3
  integer , parameter , public :: LOG_LEVEL_SUBSECTION = 4
  !
  ! Generic configuration method
  !
  interface log_configure
     module procedure log_configure_logical
     module procedure log_configure_integer
     module procedure log_configure_character
  end interface log_configure
  !
  ! Generic cget method
  !
  interface log_cget
     module procedure log_cget_logical
     module procedure log_cget_integer
     module procedure log_cget_character
  end interface log_cget

contains
  ! log_startup --
  !     Initialises the logging management and connect it to the 
  !     given filename.
  !
  ! Arguments:
  !     log_file           Name of the log file
  !   append, optional :
  !      - if present and true, then the logger appends the messages
  !        to the end of the log file.
  !      - if present and false, then the initialization of the
  !        logger overwrites the messages of the previous logging session.
  !      - if not provided, the default value is append=.true.
  !
  subroutine log_startup (log_file, append )
    character(len=*), intent(in) :: log_file
    logical, intent(in), optional :: append
    logical :: append_real
    !
    ! Process options
    !
    if ( present ( append ) ) then
       append_real = append
    else
       append_real = .true.
    endif
    if ( logger_initialized ) then
       call log_error ( "Logger is allready initialized in log_startup." )
    else
       log_fileunit = log_get_freeunit()
       if ( append_real ) then
          open (log_fileunit, FILE= log_file , ACTION='WRITE', STATUS='UNKNOWN', &
               POSITION ='APPEND')
       else
          open (log_fileunit, FILE= log_file , ACTION='WRITE', STATUS='UNKNOWN')
       endif
       logger_initialized = .true.
    endif
  end subroutine log_startup
  ! log_shutdown --
  !     Shutdown the logging management.
  !
  ! Arguments:
  !     None
  !
  subroutine log_shutdown ()
    close ( log_fileunit )
    logger_initialized = .false.
  end subroutine log_shutdown
  !
  ! log_reset --
  !    Set all internal settings to default values.
  !
  subroutine log_reset ()
    activate_screen = .true.
    activate_file = .true.
    log_timestamp = .false.
  end subroutine log_reset
  ! log_msg --
  !   Log the given character string to the logging units.
  !   If the logging to standard output is enabled, writes the message
  !   on standard output.
  !   If the logging to the log file is enabled, writes the message 
  !   into the log file.
  !   Before outputting directly the message string, the string is
  !   trimmed, that is to say that all trailing blanks are removed from
  !   the string.
  !   If the time stamp option is enabled, a time stamp with 
  !   format "year-month-day hh:mm:ss" is inserted before the message.
  !
  ! Arguments:
  !     msg           Log message to be written
  !
  ! Note :
  !    Before outputting directly the message string, the string is
  !    trimmed, that is to say that all trailing blanks are removed from
  !    the string.
  !
  subroutine log_msg( msg )
    character(len=*), intent(in) :: msg
    character(len=40)            :: date_string
    character(len=40)            :: time_string
    character(len=40)            :: stamp
    if ( log_timestamp ) then
       call date_and_time( date = date_string, time = time_string )
       write( stamp, '(11a)' ) &
            date_string(1:4), '-', date_string(5:6), '-', date_string(7:8), ' ',&
            time_string(1:2), ':', time_string(3:4), ':', time_string(5:6)
    else
       stamp = ' '
    endif
    if (activate_screen) then
       if ( log_timestamp ) then
          call log_write ( log_stdout, trim(stamp) // ' ' // msg )
       else
          call log_write ( log_stdout, msg )
       endif
    endif
    if (activate_file) then
       if ( log_timestamp ) then
          call log_write ( log_fileunit, trim(stamp) // ' ' // msg )
       else
          call log_write ( log_fileunit, msg )
       endif
    endif
  end subroutine log_msg
  ! log_write --
  !     Write the given log message "msg" of length "length"
  !     on the unit "unit".
  !     Trim the given string before writing the string.
  !
  ! Arguments:
  !     unit             LU-number to write to (-1 is the screen)
  !     msg              Message
  !
  subroutine log_write( unit, msg )
    integer, intent(in) :: unit
    character(len=*), intent(in) :: msg
    character(len=500)           :: filename
    if (  unit == -1 ) then
       write ( *, '(a)' ) trim(msg)
    else
       write ( unit, '(a)' ) trim(msg)
       !
       ! Flush the file
       !
       inquire( unit, name = filename )
       close( unit )
       open( unit, FILE=filename, ACTION='WRITE', STATUS='UNKNOWN', &
            POSITION ='APPEND')
    endif
  end subroutine log_write
  ! log_delimiter --
  !   Log a delimiter of given level, to make so that the log file
  !   contain different visual parts.
  !   Available values for level are : LOG_LEVEL_VOLUME, 
  !   LOG_LEVEL_CHAPTER, LOG_LEVEL_SECTION, LOG_LEVEL_SUBSECTION.
  !   If level is not provided, the default value for level is LOG_LEVEL_VOLUME.
  !
  ! Arguments:
  !     level            Level to be written
  !
  subroutine log_delimiter( level )
    integer , intent(in), optional :: level
    character(len=40)              :: msg
    integer                        :: used_level
    if (present(level)) then
       used_level = level
    else
       used_level = LOG_LEVEL_VOLUME
    endif
    call log_get_delimiter( used_level , msg )
    call log_msg( msg )
  end subroutine log_delimiter
  !
  ! log_get_freeunit --
  !   Returns a free logical unit.
  ! Note:
  !   Duplicated from m_fileunit so that m_logger is a stand-alone module.
  !   Do not maintain.
  !
  integer function log_get_freeunit ( )
    integer :: iunit
    integer :: ios
    logical :: lopen
    logical :: unit_found
    iunit = 0
    unit_found = .false.
    log_get_freeunit = 0
    do iunit = 1, 100
       if ( iunit /= 5 .and. iunit /= 6 .and. iunit /= 9 ) then
          inquire ( UNIT = iunit, opened = lopen, iostat = ios )
          if ( ios == 0 ) then
             if ( .not. lopen ) then
                log_get_freeunit = iunit
                unit_found = .true.
                exit
             endif
          endif
       endif
    enddo
    if (.NOT.unit_found) then
       write(*,*) "Logging: No free logical unit for log file"
    endif
  end function log_get_freeunit
  !
  ! log_isinitialized --
  !   Returns true if the logger is allready initialized.
  ! Note:
  !   That method may be useful in the case where several components
  !   use the logger and both contain a call to log_startup.
  !
  function log_isinitialized ( ) result ( isinitialized )
    implicit none
    logical :: isinitialized
    isinitialized = logger_initialized
  end function log_isinitialized
  !
  ! log_error --
  !   Generates an error for the logger..
  !
  subroutine log_error ( message )
    implicit none
    character (len=*), intent(in) :: message
    write ( 6, "(A)" ) "Error in m_logger."
    write ( 6 , "(A)" ) message
    call log_error_stop ( )
  end subroutine log_error
  !
  ! log_error_stop --
  !   Stop the execution if possible.
  !
  subroutine log_error_stop ( )
    if ( logger_stoponerror ) then
       stop
    endif
  end subroutine log_error_stop
  ! 
  ! log_set_stoponerror --
  ! 
  subroutine log_set_stoponerror ( stoponerror )
    logical , intent(in) :: stoponerror
    logger_stoponerror = stoponerror
  end subroutine log_set_stoponerror
  !
  ! log_configure_logical --
  !   Set the logical static "option" of the component to "value".
  !   The "option" may be one of the following.
  ! option = "timestamp"
  !   Disable or enable the insertion of time stamps.
  !   If the time stamp option is enabled, a time stamp with 
  !   format "year-month-day hh:mm:ss" is inserted before the message.
  ! option = "writeonstdout"
  !   Disable or enable the writing on standard output.
  ! option = "writeonlogfile"
  !   Disable or enable the writing on log file.
  ! option = "stoponerror"
  !   Configure the behaviour of the component whenever an
  !   error is met.
  !   If stoponerror is true, then the execution stops if an error is encountered.
  !   If stoponerror is false, then the execution continues if an error is encountered.
  !   In both cases, a message is displayed on standard output.
  !
  subroutine log_configure_logical ( option , value )
    implicit none
    character ( len = * ) , intent(in) :: option
    logical, intent(in) :: value
    character ( len = 500 ) :: message
    select case ( option )
    case ( "timestamp" )
       log_timestamp = value
    case ( "writeonstdout" )
       activate_screen = value
    case ( "writeonlogfile" )
       activate_file = value
    case ( "stoponerror" )
       logger_stoponerror = value
    case default
       write (message,"(A,A,A,l5,A)") "Unknown option ", option, &
            " for value ", value, " in log_configure_logical"
       call log_error ( message )
    end select
  end subroutine log_configure_logical
  !
  ! log_configure_integer --
  !   Set the integer static "option" of the component to "value".
  !   The "option" may be one of the following.
  ! option = "logfileunit"
  !   Force the logical unit for logging to be "value".
  !   Use this feature with caution, since the original
  !   logical unit is lost.
  !
  subroutine log_configure_integer ( option , value )
    implicit none
    character ( len = * ) , intent(in) :: option
    integer, intent(in) :: value
    character ( len = 500 ) :: message
    select case ( option )
    case ( "logfileunit" )
       log_fileunit = value
    case default
       write (message,"(A,A,A,I5,A)") "Unknown option ", option, &
            " for value ", value, " in log_configure_integer"
       call log_error ( message )
    end select
  end subroutine log_configure_integer
  !
  ! log_configure_character --
  !   Set the character static "option" of the component to "value".
  !   The "option" may be one of the following.
  ! option = "level_string_volume"
  !   Set the string used for volume delimiter.
  ! option = "level_string_chapter"
  !   Set the string used for chapter delimiter.
  ! option = "level_string_section"
  !   Set the string used for section delimiter.
  ! option = "level_string_subsection"
  !   Set the string used for subsection delimiter.
  !
  subroutine log_configure_character ( option , value )
    implicit none
    character ( len = * ) , intent(in) :: option
    character ( len = * ) , intent(in) :: value
    character ( len = 500 ) :: message
    select case ( option )
    case ( "level_string_volume" )
       log_level_string_volume = value
    case ( "level_string_chapter" )
       log_level_string_chapter = value
    case ( "level_string_section" )
       log_level_string_chapter = value
    case ( "level_string_subsection" )
       log_level_string_chapter = value
    case default
       write (message,"(A,A,A,A,A)") "Unknown option ", option, &
            " for value ", value, " in log_configure_character"
       call log_error ( message )
    end select
  end subroutine log_configure_character
  !
  ! log_cget_logical --
  !   Returns the value of the given logical option.
  !   The "option" may be one of the following.
  ! option = "timestamp"
  !   Current value of the option to enable / disable insertion of time stamps.
  ! option = "writeonstdout"
  !   Current value of the option to enable / disable writing on standard output.
  ! option = "writeonlogfile"
  !   Current value of the option to enable / disable writing on log file.
  ! option = "stoponerror"
  !   Current value of the option to enable / disable stopping when an error is met.
  !
  subroutine log_cget_logical ( option , value )
    implicit none
    character ( len = * ) , intent(in) :: option
    logical, intent(out) :: value
    character ( len = 500 ) :: message
    select case ( option )
    case ( "timestamp" )
       value = log_timestamp
    case ( "writeonstdout" )
       value = activate_screen
    case ( "writeonlogfile" )
       value = activate_file
    case ( "stoponerror" )
       value = logger_stoponerror
    case default
       write (message,"(A,l5,A)") "Unknown option ", option, &
            " in log_cget_logical"
       call log_error ( message )
    end select
  end subroutine log_cget_logical
  !
  ! log_cget_integer --
  !   Returns the value of the given integer option.
  !   The "option" may be one of the following.
  ! option = "logfileunit"
  !   Current logical unit connected to the logging system.
  !
  subroutine log_cget_integer ( option , value )
    implicit none
    character ( len = * ) , intent(in) :: option
    integer, intent(out) :: value
    character ( len = 500 ) :: message
    select case ( option )
    case ( "logfileunit" )
       value = log_fileunit
    case default
       write (message,"(A,I5,A)") "Unknown option ", option, &
            " in log_cget_integer"
       call log_error ( message )
    end select
  end subroutine log_cget_integer
  !
  ! log_cget_character --
  !   Returns the value of the given logical option.
  !   The "option" may be one of the following.
  ! option = "level_string_volume"
  !   Get the string used for volume delimiter.
  ! option = "level_string_chapter"
  !   Get the string used for chapter delimiter.
  ! option = "level_string_section"
  !   Get the string used for section delimiter.
  ! option = "level_string_subsection"
  !   Get the string used for subsection delimiter.
  !
  subroutine log_cget_character ( option , value )
    implicit none
    character ( len = * ) , intent(in) :: option
    character ( len = * ) , intent(out) :: value
    character ( len = 500 ) :: message
    select case ( option )
    case ( "level_string_volume" )
       value = log_level_string_volume 
    case ( "level_string_chapter" )
       value = log_level_string_chapter 
    case ( "level_string_section" )
       value = log_level_string_chapter
    case ( "level_string_subsection" )
       value = log_level_string_chapter
    case default
       write (message,"(A,A,A)") "Unknown option ", option, &
            " in log_cget_character"
       call log_error ( message )
    end select
  end subroutine log_cget_character
  !
  ! Deprecated methods : use "log_configure" instead
  !
  !
  ! log_inactivate_file --
  !   Allows to inactivate the logging into the file.
  !   Deprecated : use log_configure instead.
  !
  subroutine log_inactivate_file ()
    activate_file = .false.
  end subroutine log_inactivate_file
  !
  ! log_activate_file --
  !   Allows to activate the logging into the file.
  !   Deprecated : use log_configure instead.
  !
  subroutine log_activate_file ()
    activate_file = .true.
  end subroutine log_activate_file
  !
  ! log_inactivate_screen --
  !   Allows to inactivate the logging on screen.
  !   Deprecated : use log_configure instead.
  !
  subroutine log_inactivate_screen ()
    activate_screen = .false.
  end subroutine log_inactivate_screen
  !
  ! log_activate_screen --
  !   Allows to activate the logging on screen.
  !   Deprecated : use log_configure instead.
  !
  subroutine log_activate_screen ()
    activate_screen = .true.
  end subroutine log_activate_screen
  !
  ! log_init --
  !   Deprecated, use log_startup instead.
  !   Initialises the logging management and connect it to the 
  !   given filename.
  !
  subroutine log_init (log_file, append )
    character(len=*), intent(in) :: log_file
    logical, intent(in), optional :: append
    call log_startup ( log_file, append)
  end subroutine log_init
  !
  ! log_get_delimiter --
  !   Deprecated : use log_cget instead.
  !   Fills msg with a log delimiter of given level.
  !   Available values for level are : LOG_LEVEL_VOLUME, 
  !   LOG_LEVEL_CHAPTER, LOG_LEVEL_SECTION, LOG_LEVEL_SUBSECTION
  !
  ! Arguments:
  !     level             Level in question
  !     msg               Corresponding string
  !
  subroutine log_get_delimiter ( level , msg )
    implicit none
    integer , intent(in) :: level
    character(len=*), intent(out) :: msg
    select case (level)
    case (LOG_LEVEL_VOLUME)
       write(msg,*) "==============="
    case (LOG_LEVEL_CHAPTER)
       write(msg,*) "---------------"
    case (LOG_LEVEL_SECTION)
       write(msg,*) "***************"
    case (LOG_LEVEL_SUBSECTION)
       write(msg,*) "+++++++++++++++"
    case default
       ! NOTE :
       ! We do not use m_exception here to limit the dependencies of
       ! such a low level utility.
       write(*,*) "Bad value for the message level:" , level
       write(*,*)
       stop
    end select
  end subroutine log_get_delimiter
  ! log_get_unit --
  !   Deprecated : use log_cget instead.
  !   Returns the unit number used in the logger
  !
  ! Arguments:
  !     None
  !
  function log_get_unit () result ( logger_unit )
    integer :: logger_unit
    logger_unit = log_fileunit
  end function log_get_unit
end module m_logger

