!
! m_exception --
!
!   Provides services to generate different levels of exceptions
!   and display the message of the exception.
!   Five levels of exceptions can be generated, from the lowest 
!   to the highest level :
!   - information, warning : just print a message and continue
!   - error, fatal_error and failure : prints a message and stop the execution
!
! OVERVIEW
!
!   Simple use-case
!
!   Suppose that one would like to compute the square root of one 
!   real value. The "compute_sqrt" function takes one positive real argument,
!   and if the argument is negative, one cannot compute the square root so 
!   that one would generate an error. In the following example, extracted from the 
!   unit tests included in the project, one uses the static method "exception_raiseError" 
!   to display a user-friendly message and stop the execution of the program
! 
!    function compute_sqrt ( value ) result ( root )
!      use m_exception
!      implicit none
!      real, intent(in) :: value
!      real :: root
!      if ( value < 0. ) then
!        call exception_raiseError ( "Value is negative in compute_sqrt" )
!      else 
!        root = sqrt ( value )
!      endif
!   end function compute_sqrt
!
!   real :: root
!   root = compute_sqrt ( -1. )
! 
!   In the previous example, the standard output is written so that the 
!   following message appears on screen :
!
!   Error.
!   Message: Value is negative in compute_sqrt
!
!   Controlling the execution
! 
!   The client code can control the behaviour of the component each time 
!   an exception is raised.
!   The default behaviour is to stop the execution. This can be modified
!   by calling "exception_setstoponerror" in order to continue the execution,
!   even if error, fatal error or failure exceptions are raised.
!
!   In the following example, the static method "exception_setstoponerror" is 
!   called so that an error does not interrupt the execution.
!
!   call exception_setstoponerror ( .false. )
!   call exception_raiseError ( "There is an error, but the execution will continue." )
!
! Controlling output
!
!   The default behaviour is to write messages onto the standard output
!   each time an exception is raised.
!   This can be modified in two ways :
!   * the first possibility is to disable the writing of the messages 
!     with "exception_logactive". This feature might be useful in the case
!     where a component has known bugs but generates lots of unwanted
!     exceptions messages.
!   * the second possibility is to connect the component to an existing unit
!     with "exception_setlogunit", so that the messages are written
!     on the given logical unit number. 
!     This allows for example to write on an existing log file, may be the log 
!     file manage by the m_logger component included in the project.
!
!   In the following example, the client code first disables all output,
!   set "stoponerror" to false and generates an error which is not displayed
!   and does not interrupt the execution.
!
!   call exception_setstoponerror ( .false. )
!   call exception_logactive ( .false. )
!   call exception_raiseError ( "This message will not be displayed and the execution will continue." )
!   call exception_logactive ( .true. )
!   call exception_raiseError ( "This message WILL be displayed and the execution will continue." )
!
!   In the following example, the client code connects the m_exception component to 
!   an existing unit so that the exception messages are written onto a client log file.
!
!     log_fileunit = 12
!     call exception_setstoponerror ( .false. )
!     open ( log_fileunit , FILE= "log_file.log" )
!     call exception_setlogunit ( log_fileunit )
!     call exception_raiseError ( "This message will be written in log_file.log and the execution will continue." )
!     call exception_setlogunit ( 0 )
!     call exception_raiseError ( "This message will be written on standard output and the execution will continue." )
!     close ( log_fileunit )
!
!   In the following example, the client code connects the m_exception component to 
!   the logfile manage by m_logger. This way, the exception messages are collected in the 
!   unique log file of the client code.
!
!     call log_startup ( "log_file.log" , append=.true. )
!     call log_cget ( "logfileunit" , log_fileunit )
!     call exception_setstoponerror ( .false. )
!     call exception_setlogunit ( log_fileunit )
!     call exception_raiseError ( "This message will be written in log_file.log and the execution will continue." )
!     call log_shutdown ()
!
! Pseudo-catch
!
!   The client code can use a pseudo-catch system which provides
!   a simple way to manage exceptions which are raised at a lower 
!   level in the call stack. This allows to provide special 
!   treatments when exceptions are generated, without modifiying
!   all lower level subroutines/function, but just by inserting 
!   exception management when needed.
!   Suppose that you have a subroutine which source code is :
!     subroutine yoursubroutine ()
!       use m_exception, only : exception_raiseFatalError
!       implicit none
!       [...]
!       call exception_raiseFatalError ( "Wrong blabla in yoursubroutine" )
!       [...]
!     end subroutine yoursubroutine
!   When calling the subroutine "yoursubroutine", one may wonder if exceptions
!   have been generated so that these errors may be processed, or not.
!   One can use the "exception_catch" service to compute the status 
!   of one subroutine and manage that status :
!     use m_exception, only : exception_catch, &
!         EXCEPTION_INFORMATION, &
!         EXCEPTION_WARNING &
!         EXCEPTION_ERROR &
!         EXCEPTION_FATAL_ERROR &
!         EXCEPTION_FAILURE
!     integer :: status
!     call exception_catch ( yoursubroutine , status )
!     select case ( status )
!     case ( EXCEPTION_INFORMATION )
!        write(6,*) "Information"
!     case ( EXCEPTION_WARNING )
!        write(6,*) "Warning"
!     case ( EXCEPTION_ERROR , EXCEPTION_FATAL_ERROR , EXCEPTION_FAILURE )
!        write(6,*) "Fatal error"
!     case default
!        write(6,*) "No problem, continue."
!     end select
!
! TODO
!   - design a more powerful exception management system, which manages exceptions
!   through the call stack
!
! Copyright (c) 2008 Michael Baudin
!
! $Id: m_exception.f90,v 1.4 2008/06/18 10:35:22 relaxmike Exp $
!
module m_exception
  implicit none
  private
  !
  ! Public methods
  !
  public :: exception_getcounter
  public :: exception_getlogunit
  public :: exception_initcounter
  public :: exception_islogactive
  public :: exception_logactive
  public :: exception_raiseError
  public :: exception_raiseFailure
  public :: exception_raiseFatalError
  public :: exception_raiseInformation
  public :: exception_raiseWarning
  public :: exception_report
  public :: exception_setlogunit
  public :: exception_setstoponerror
  public :: exception_catch
  !
  ! Tags to manage error
  !
  integer, parameter, public :: EXCEPTION_OK = 0
  integer, parameter, public :: EXCEPTION_INFORMATION = 1
  integer, parameter, public :: EXCEPTION_WARNING = 2
  integer, parameter, public :: EXCEPTION_ERROR = 3
  integer, parameter, public :: EXCEPTION_FATAL_ERROR = 4
  integer, parameter, public :: EXCEPTION_FAILURE = 5
  integer, parameter, public :: EXCEPTION_SIZE = 5
  !
  ! Set to true to stop the execution after a stoping error.
  !
  logical :: exception_stoponerror = .true.
  !
  ! Counters for the different levels of errors
  !
  integer :: exception_nbinformation = 0
  integer :: exception_nbwarning = 0
  integer :: exception_nberror = 0
  integer :: exception_nbfatalerror = 0
  integer :: exception_nbfailure = 0
  !
  ! Maximum number of characters in a message generated by the exception
  !
  integer , parameter :: EXCEPTION_MAXIMUM_LENGTH = 500
  !
  ! Set exception_log_active to true to activate the exception logging
  ! to the unit # exception_log_unit
  !
  integer :: exception_log_unit = 0
  logical :: exception_log_unit_active = .false.
  !
  ! Set exception_log_active to true to display the exception messages
  !
  logical :: exception_log_active = .true.
contains
  !
  ! exception_raiseFatalError --
  !   Generates a fatal error message and stops the execution.
  !
  subroutine exception_raiseFatalError ( message )
    character(len=*), intent(in) :: message
    exception_nbfatalerror = exception_nbfatalerror + 1
    call exception_print(EXCEPTION_FATAL_ERROR, message )
    call exception_stop ()
  end subroutine exception_raiseFatalError
  !
  ! exception_raiseError --
  !   Generates an error message and stops the execution
  !
  subroutine exception_raiseError ( message )
    character(len=*), intent(in) :: message
    exception_nberror = exception_nberror + 1
    call exception_print(EXCEPTION_ERROR, message )
    call exception_stop ()
  end subroutine exception_raiseError
  !
  ! exception_raiseWarning --
  !   Generates a warning message
  !
  subroutine exception_raiseWarning ( message )
    character(len=*), intent(in) :: message
    exception_nbwarning = exception_nbwarning + 1
    call exception_print(EXCEPTION_WARNING, message )
  end subroutine exception_raiseWarning
  !
  ! exception_raiseInformation --
  !   Generates an information message.
  !
  subroutine exception_raiseInformation ( message )
    character(len=*), intent(in) :: message
    exception_nbinformation = exception_nbinformation + 1
    call exception_print(EXCEPTION_INFORMATION, message )
  end subroutine exception_raiseInformation
  !
  ! exception_raiseFailure --
  !   Generates a failure message and stops the execution
  !
  subroutine exception_raiseFailure ( message )
    character(len=*), intent(in) :: message
    exception_nbfailure = exception_nbfailure + 1
    call exception_print(EXCEPTION_FAILURE, message )
    call exception_stop ()
  end subroutine exception_raiseFailure
  !
  ! exception_print --
  !   Prints a message on the standard output about 
  !   the current exception type, the origin of the error and the 
  !   message of the error.
  ! Arguments :
  !   exceptionCode : an integer exception code between : EXCEPTION_INFORMATION,
  !     EXCEPTION_WARNING, EXCEPTION_ERROR, EXCEPTION_FATAL_ERROR, EXCEPTION_FAILURE
  !   origin : the name of the element generating the exception
  !   message : a message to display
  !
  subroutine exception_print ( exceptionCode , message )
    integer, intent(in) :: exceptionCode
    character(len=*), intent(in) :: message
    character ( len = EXCEPTION_MAXIMUM_LENGTH ) :: msg
    select case (exceptionCode)
    case (EXCEPTION_INFORMATION)
       write ( msg , * ) "Information."
    case (EXCEPTION_WARNING)
       write(msg,*) "Warning."
    case (EXCEPTION_ERROR)
       write(msg,*) "Error."
    case (EXCEPTION_FATAL_ERROR)
       write(msg,*) "Fatal error."
    case (EXCEPTION_FAILURE)
       write(msg,*) "Failure."
    case default
       write(msg,*) "Bad value for the exception code."
    end select
    call exception_logmsg ( msg )
    write(msg,*) "Message: ", trim(message)
    call exception_logmsg ( msg )
  end subroutine exception_print
  !
  ! exception_stop --
  !   Stop the execution after an exception has been raised.
  ! Note :
  !   In debug mode, put a breakpoint here allows to see the exception
  !   before stopping.
  !
  subroutine exception_stop ()
    if ( exception_stoponerror ) then
       STOP
    endif
  end subroutine exception_stop
  !
  ! exception_setstoponerror --
  !   Configure the component to manage the behaviour when an exception 
  !   is generated.
  ! Arguments :
  !   stoponerror : if true, then when an error, a fatal_error or a failure
  !     is generated, then stop the execution.
  !     If .false., then the same kinds of exceptions do not stop the 
  !     execution
  !
  subroutine exception_setstoponerror ( stoponerror )
    logical, intent(in) :: stoponerror
    exception_stoponerror = stoponerror
  end subroutine exception_setstoponerror
  !
  ! exception_logmsg --
  !   Write the given message onto the default unit.
  !
  subroutine exception_logmsg ( message )
    character(len=*), intent(in) :: message
    if ( exception_log_active ) then
       if ( exception_log_unit_active ) then
          write(exception_log_unit,*) trim(message)
       else
          write(*,*) trim(message)
       endif
    endif
  end subroutine exception_logmsg
  !
  ! exception_initcounter --
  !   Reset to 0 all the exceptions counters
  !
  subroutine exception_initcounter ()
    exception_nbinformation = 0
    exception_nbwarning = 0
    exception_nberror = 0
    exception_nbfatalerror = 0
    exception_nbfailure = 0
  end subroutine exception_initcounter
  !
  ! exception_getcounter --
  !   Returns an array of size EXCEPTION_SIZE indicating the number
  !   of type of exceptions.
  ! Arguments :
  !   excepcounters ( 1:EXCEPTION_SIZE) : array of integers
  !     excepcounters ( iexcept ) is the total number of exceptions
  !     of type #iexcept generated since the begining of the execution.
  !     The possible values for iexcept are the following.
  !     - counter ( EXCEPTION_INFORMATION ) : total number of information exceptions raised
  !     - counter ( EXCEPTION_WARNING ) : total number of warning exceptions raised
  !     - counter ( EXCEPTION_ERROR ) : total number of error exceptions raised
  !     - counter ( EXCEPTION_FATAL_ERROR ) : total number of fatal error exceptions raised
  !     - counter ( EXCEPTION_FAILURE ) : total number of failure exceptions raised
  !
  subroutine exception_getcounter ( counter )
    integer, dimension(1:EXCEPTION_SIZE), intent(out) :: counter
    counter ( EXCEPTION_INFORMATION ) = exception_nbinformation
    counter ( EXCEPTION_WARNING ) = exception_nbwarning
    counter ( EXCEPTION_ERROR ) = exception_nberror
    counter ( EXCEPTION_FATAL_ERROR ) = exception_nbfatalerror
    counter ( EXCEPTION_FAILURE ) = exception_nbfailure
  end subroutine exception_getcounter
  !
  ! exception_report --
  !   Writes on the current exception unit number a report
  !   which details the number of exceptions of all types
  !   since the begining of the execution or the last reset of
  !   all counters.
  !
  subroutine exception_report ()
    character ( len = 300 ) :: message
    call exception_logmsg ( "Exception report" )
    write ( message , * ) "Number of informations :" , exception_nbinformation
    call exception_logmsg ( message )
    write ( message , * ) "Number of warnings :" , exception_nbwarning
    call exception_logmsg ( message )
    write ( message , * ) "Number of errors :" , exception_nberror
    call exception_logmsg ( message )
    write ( message , * ) "Number of fatal errors :" , exception_nbfatalerror
    call exception_logmsg ( message )
    write ( message , * ) "Number of failures :" , exception_nbfailure
    call exception_logmsg ( message )
  end subroutine exception_report
  !
  ! exception_setlogunit --
  !   Set the unit number onto which the messages are output.
  !   If the unitnumber is negative or 0, the messages are written
  !   to the standard output (unit *).
  ! Arguments :
  !   unitnumber : the unit number
  ! Note :
  !   A unitnumber equals to 6 is generally the standard output, 
  !   but this may depend on the fortran compiler.
  !
  subroutine exception_setlogunit ( unitnumber )
    integer, intent(in) :: unitnumber
    if ( unitnumber <= 0 ) then
       exception_log_unit_active = .false.
       exception_log_unit = unitnumber
    else
       exception_log_unit_active = .true.
       exception_log_unit = unitnumber
    endif
  end subroutine exception_setlogunit
  !
  ! exception_getlogunit --
  !   Returns the positive unit number onto which the messages are output,
  !   if enabled or a negative integer if the feature is disabled.
  ! Arguments :
  !   
  !
  function exception_getlogunit ( ) result ( logunit )
    integer :: logunit
    logunit = exception_log_unit
  end function exception_getlogunit
  !
  ! exception_logactive --
  !   If the boolean argument bool is true, enable the logging 
  !   of the exceptions messages.
  !   If the boolean argument bool is false, disable the logging 
  !   of the exceptions messages.
  ! Arguments :
  !   bool : if true, active the logging of the messages
  !
  subroutine exception_logactive ( bool )
    logical, intent(in) :: bool
    exception_log_active = bool
  end subroutine exception_logactive
  !
  ! exception_islogactive --
  !   Returns .true. if the current exception messages are written,
  !   either on standard output or into a log file.
  !
  function exception_islogactive ( ) result ( islogactive )
    logical :: islogactive
    islogactive = exception_log_active
  end function exception_islogactive
  !
  ! exception_catch --
  !   Calls the given subroutine and returns the integer associated 
  !   with last exception, higher level, code or 0 if no exception
  !   was raised.
  ! Argument :
  !   callback : the subroutine to call back
  !   status : the integer corresponding to the last exception, if any.
  ! Caution !
  !   The internal algorithm is based on the exception counters,
  !   which implies that any call to exception_initcounter in the client
  !   code can result in a wrong status.
  !
  subroutine exception_catch ( callback , status )
    interface interfacecallback
       subroutine callback ()
       end subroutine callback
    end interface interfacecallback
    integer, intent(out) :: status
    integer, dimension(1:EXCEPTION_SIZE) :: counter_before
    integer, dimension(1:EXCEPTION_SIZE) :: counter_after
    integer :: exception_code
    !
    ! Get the counters before
    !
    call exception_getcounter ( counter_before )
    !
    ! Call the callback
    !
    call callback ()
    !
    ! Get the counters after
    !
    call exception_getcounter ( counter_after )
    !
    ! Compare before / after and set the corresponding status.
    !
    ! Process the loop, from the highest level of exception to the lowest.
    status = EXCEPTION_OK
    do exception_code = EXCEPTION_SIZE , 1 , -1
       if ( counter_after ( exception_code ) > counter_before ( exception_code ) ) then
          status = exception_code
          exit
       endif
    enddo
  end subroutine exception_catch
end module m_exception

