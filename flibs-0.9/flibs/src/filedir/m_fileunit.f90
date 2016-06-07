!
! m_fileunit.f90 --
!
!   The component provides services to manage fortran file units.
!
! Overview
!
!   The function fileunit_getfreeunit returns an integer representing
!   a fortran unit which is available for opening a file.
!   The typical use of this function is to manage the files dynamically,
!   without any database of file units in the library/software.
!   In the following example, one opens a file with a dynamical
!   file unit.
! 
!     integer :: fileunit
!     fileunit = fileunit_getfreeunit ()
!     open ( unit = fileunit , file = "data.txt" )
!     [etc...]
!
!   If several files are to be opened, the "fileunit_getfreeunit" 
!   method has to be inserted between the "open" statements.
!   This is because two consecutive calls to "fileunit_getfreeunit"
!   will return the same integer, as expected : if a unit is available
!   the first time, it will also be available the second time.
!   In the following example, several files are opened and connected
!   to several files.
! 
!     integer :: fileunit1
!     integer :: fileunit2
!     fileunit1 = fileunit_getfreeunit ()
!     open ( unit = fileunit1 , file = "data.txt" )
!     fileunit2 = fileunit_getfreeunit ()
!     open ( unit = fileunit2 , file = "data2.txt" )
!     [etc...]
!
!   In a large fortran software, it may be difficult to see if some 
!   bug has been introduced in the file management, especially
!   when the software is the composition of several libraries.
!   The subroutines fileunit_getallopen , fileunit_closeallopen , 
!   fileunit_report , fileunit_displayopen allow to manage for 
!   the units currently used in the software.
!   The fileunit_getallopen returns an array of integers which 
!   contains all the currently opened units. The fileunit_closeallopen
!   subroutine close all currently opened units. The fileunit_report
!   displays a full report about a given unit number by using the 
!   "inquire" fortran intrinsic statement.
!
! TODO:
! - allow to "lock" a collection of logical units, so that an 
! external library which may use constant units can be linked.
! - allow to "unlock" one unit, or all units at once.
!
! Copyright (c) 2008 Michael Baudin michael.baudin@gmail.com
!
module m_fileunit
  implicit none
  private
  !
  ! Public methods
  !
  public :: fileunit_getfreeunit
  public :: fileunit_getallopen
  public :: fileunit_closeallopen
  public :: fileunit_displayopen
  public :: fileunit_report
  public :: fileunit_set_stoponerror
  !
  ! Constants
  !
  ! Maximum number of units when searching for an unused file unit
  integer, parameter, public :: FILEUNIT_MAX_UNIT_NUMBER = 1000
  !
  ! Set to true to stop whenever an error comes in the vstring component.
  !
  logical, save :: fileunit_stoponerror = .true.
contains
  !
  ! fileunit_getallopen --
  !   Computes an array of integers made of all currently opened units.
  ! Output :
  !   nbunits : number of opened units
  !   units ( iunit ) : unit number for the opened unit #iunit with 1<= iunit <= nbunits
  !
  subroutine fileunit_getallopen ( nbunits , units )
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
       do iunit = 1, FILEUNIT_MAX_UNIT_NUMBER
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
  end subroutine fileunit_getallopen
  !
  ! fileunit_displayopen --
  !   Writes on unit "unitnumber" the full list of opened units and their associated 
  !   filenames.
  ! Input :
  !   reportunitnumber : the unit number on which the report is written
  !
  subroutine fileunit_displayopen ( reportunitnumber )
    implicit none
    integer, intent ( in ) :: reportunitnumber
    integer :: iunit
    integer :: nbunits
    integer , dimension(:) , pointer :: units
    call fileunit_getallopen ( nbunits , units )
    write ( reportunitnumber , * ) "Number of units opened : ", nbunits
    do iunit = 1, nbunits
       write ( reportunitnumber , * ) "Unit # ", iunit , "/" , nbunits
       call fileunit_report ( reportunitnumber , units ( iunit ) )
    end do
    deallocate ( units )
  end subroutine fileunit_displayopen
  !
  ! fileunit_report --
  !   Compute report about logical unit iunit and write it on
  !   unit unitnumber.
  ! Note :
  !   All possible features of the "inquire" intrinsic are used.
  !
  subroutine fileunit_report ( reportunitnumber , iunit )
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
    call fileunit_report_write_integer ( reportunitnumber , "iunit" , iunit )
    call fileunit_report_write_integer ( reportunitnumber , "iostat" , unit_iostat )
    call fileunit_report_write_logical ( reportunitnumber , "exist" , unit_exist )
    call fileunit_report_write_logical ( reportunitnumber , "opened" , unit_open )
    call fileunit_report_write_logical ( reportunitnumber , "named" , unit_named )
    if ( unit_named ) then
       call fileunit_report_write_character ( reportunitnumber , "filename" , trim(unit_name) )
    endif
    call fileunit_report_write_character ( reportunitnumber , "sequential" , trim(unit_sequential) )
    call fileunit_report_write_character ( reportunitnumber , "direct" , trim(unit_direct) )
    call fileunit_report_write_character ( reportunitnumber , "form" , trim(unit_form) )
    call fileunit_report_write_character ( reportunitnumber , "formatted" , trim(unit_formatted) )
    call fileunit_report_write_integer ( reportunitnumber , "record length" , unit_record_length )
    call fileunit_report_write_integer ( reportunitnumber , "last record" , unit_nextrec )
    call fileunit_report_write_character ( reportunitnumber , "blank" , trim(unit_blank) )
    call fileunit_report_write_character ( reportunitnumber , "position" , trim(unit_position) )
    call fileunit_report_write_character ( reportunitnumber , "action" , trim(unit_action) )
    call fileunit_report_write_character ( reportunitnumber , "read" , trim(unit_read) )
    call fileunit_report_write_character ( reportunitnumber , "write" , trim(unit_write) )
    call fileunit_report_write_character ( reportunitnumber , "readwrite" , trim(unit_readwrite) )
    call fileunit_report_write_character ( reportunitnumber , "delim" , trim(unit_delim) )
    call fileunit_report_write_character ( reportunitnumber , "pad" , trim(unit_pad) )
  contains
    subroutine fileunit_report_write_character ( reportunitnumber , key , value )
      implicit none
      integer , intent (in) :: reportunitnumber
      character (len= *), intent(in) :: key
      character (len= *), intent(in) :: value
      write ( reportunitnumber , * ) "  * ", trim(key), " : ", trim(value)
    end subroutine fileunit_report_write_character
    subroutine fileunit_report_write_integer ( reportunitnumber , key , value )
      implicit none
      integer , intent (in) :: reportunitnumber
      character (len= *), intent(in) :: key
      integer, intent(in) :: value
      write ( reportunitnumber , * ) "  * ", trim(key), " : ", value
    end subroutine fileunit_report_write_integer
    subroutine fileunit_report_write_logical ( reportunitnumber , key , value )
      implicit none
      integer , intent (in) :: reportunitnumber
      character (len= *), intent(in) :: key
      logical, intent(in) :: value
      write ( reportunitnumber , * ) "  * ", trim(key), " : ", value
    end subroutine fileunit_report_write_logical
  end subroutine fileunit_report
  !
  ! fileunit_closeallopen --
  !   Close all currently opened units.
  !
  subroutine fileunit_closeallopen ( )
    implicit none
    integer :: nbunits
    integer , dimension(:) , pointer :: units
    integer :: iunit
    call fileunit_getallopen ( nbunits , units )
    do iunit = 1, nbunits
       close ( units ( iunit ) )
    enddo
  end subroutine fileunit_closeallopen
  !
  ! fileunit_getfreeunit --
  !   Returns a free fortran unit as an integer between 1 and FILEUNIT_MAX_UNIT_NUMBER, 
  !   representing a free FORTRAN logical unit.
  !   If no free unit can be found, generates an error.
  ! Arguments:
  !   no argument
  ! Note :
  !   A "free" FORTRAN unit number is an integer between 1 and FILEUNIT_MAX_UNIT_NUMBER which
  !   is not currently associated with an I/O device.  A free FORTRAN unit
  !   number is needed in order to open a file with the OPEN command.
  !
  !   Note that fileunit_getfreeunit assumes that units 5 and 6
  !   are special, and will never return those values.
  !
  !  Original Author : John Burkardt
  !
  function fileunit_getfreeunit ( ) result ( freeunit )
    integer :: freeunit
    integer :: iunit
    integer :: ios
    logical :: lopen
    logical :: unit_found
    iunit = 0
    unit_found = .false.
    freeunit = 0
    do iunit = 1, FILEUNIT_MAX_UNIT_NUMBER
       if ( iunit /= 5 .and. iunit /= 6 .and. iunit /= 9 ) then
          inquire ( UNIT = iunit, opened = lopen, iostat = ios )
          if ( ios == 0 ) then
             if ( .not. lopen ) then
                freeunit = iunit
                unit_found = .true.
                exit
             end if
          end if
       end if
    end do
    if (.NOT.unit_found) then
       call fileunit_error ( message = "No unit free in fileunit_getfreeunit." )
    endif
  end function fileunit_getfreeunit
  !
  ! fileunit_error --
  !   Manage an error for the module with only a message
  ! Arguments :
  !   message : the message to display
  !
  subroutine fileunit_error ( message )
    character(len=*), intent(in) :: message
    write(*,*) "Error: ", trim(message)
    call fileunit_stop ()
  end subroutine fileunit_error
  !
  ! fileunit_stop --
  !   Stop the execution if possible.
  !
  subroutine fileunit_stop ( )
    if ( fileunit_stoponerror ) then
       stop
    endif
  end subroutine fileunit_stop
  ! 
  ! fileunit_set_stoponerror --
  !   Configure the behaviour of the component whenever an 
  !   error is met.
  !   If stoponerror is true, then the execution stops if an error is encountered.
  !   If stoponerror is false, then the execution continues if an error is encountered.
  !   In both cases, a message is displayed on standard output.
  ! 
  subroutine fileunit_set_stoponerror ( stoponerror )
    logical , intent(in) :: stoponerror
    fileunit_stoponerror = stoponerror
  end subroutine fileunit_set_stoponerror
end module m_fileunit

