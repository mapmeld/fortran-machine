! test_logging.f90 --
!     Small test program for the report module
!
!     $Id: test_m_logger.f90,v 1.1 2008/06/18 09:00:51 relaxmike Exp $
!
program test_logging
  use m_logger
  integer :: logunit
  !
  ! Initialize the log file with append disabled.
  !
  call log_startup( 'test_m_logger.log', append=.false.)
  call log_configure ( "timestamp" , .true. )
  call log_msg( 'First message' )
  call log_msg( 'Second message' )
  call log_shutdown ()
  !
  ! Messages will be appended
  !
  call log_startup( 'test_m_logger.log' )
  call log_configure ( "timestamp" , .false. )
  call log_msg( 'Second part: no timestamp' )
  call log_msg( 'First message' )
  call log_msg( 'Second message' )
  !
  ! Add delimiter to structure the log file
  !
  call log_delimiter( LOG_LEVEL_VOLUME )
  call log_msg( 'Volume title' )
  call log_delimiter( LOG_LEVEL_CHAPTER )
  call log_msg( 'Chapter title' )
  call log_delimiter( LOG_LEVEL_SECTION )
  call log_msg( 'Section title' )
  call log_delimiter( LOG_LEVEL_SUBSECTION )
  call log_msg( 'Subsection title' )
  !
  ! Get the log unit and write directly to the logfile
  !
  call log_cget ( "logfileunit" , logunit)
  write ( logunit , "(A)") "This is my manually written message, written only on file"
  !
  ! Enable / disable writing on file/standard output
  !
  call log_configure ( "writeonstdout" , .false. )
  call log_msg( 'This message is written only on file' )
  call log_configure ( "writeonlogfile" , .false. )
  call log_msg( 'This message is written nowhere' )
  call log_configure ( "writeonstdout" , .true. )
  call log_msg( 'This message is written only on screen' )
  call log_configure ( "writeonlogfile" , .true. )
  call log_msg( 'This message is written both on screen and on file' )
  call log_shutdown ()
  !
  ! Check that we cannot init two times.
  !
  call log_configure ( "stoponerror" , .false. )
  call log_startup( 'test_m_logger2.log' )
  call log_startup( 'test_m_logger2.log' )
  call log_shutdown ()
end program test_logging

