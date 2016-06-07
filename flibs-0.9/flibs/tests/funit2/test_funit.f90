! test_funit.f90 --
!     Program to demonstrate the use of the "funit" framework:
!     It is inspired by the well-known JUnit framework for
!     integrating unit tests in a Java application.
!
!     The program that is being tested is built from three
!     subroutines:
!     - open_files
!     - process_data
!     - report_results
!     The idea is simple:
!     Read a file with data (one number per line) and
!     report some basic statistics.
!
!     While it is a small program, so that it could easily
!     do without separate routines, splitting it into routines
!     makes setting up unit tests much easier.
!
!     The unit tests consist of:
!     1. Try to read a non-existing data file
!     2. Try to read an empty data file
!     3. Try to read a file that does not just contain numbers
!     4. Read a regular file with numbers
!
!     To illustrate the way run-time errors are handled via
!     the framework, some of these tests result in runtime errors
!
!     $Id$
!
module data_processing
    implicit none

contains

! open_files --
!     Subroutine to read the file names and open them
! Arguments:
!     None
! Side effects:
!     The input file with data is opened at LUN 10,
!     the report file is opened at LUN 20
!
subroutine open_files
    character(len=40) :: infile
    integer           :: ierr

    open( 10, file = 'test_funit.inp' )
    open( 20, file = 'test_funit.out' )
    read( 10, '(a)' ) infile
    close( 10 )

    open( 11, file = infile, status = 'old', iostat = ierr )

    if ( ierr /= 0 ) then
        write( 20, * ) 'Input file could not be opened:'
        write( 20, * ) '    ', infile
        write( 20, * ) 'Stopping the program'
        stop
    endif
end subroutine

! process_data --
!     Subroutine to read the data file and produce the statistics
! Arguments:
!     nodata            Number of data
!     vmean             Mean value
!     vmin              Minimum value
!     vmax              Maximum value
!
subroutine process_data( nodata, vmean, vmin, vmax )
    integer, intent(out)  :: nodata
    real, intent(out)     :: vmean
    real, intent(out)     :: vmin
    real, intent(out)     :: vmax

    real                  :: value

    nodata = 0
    vmean  = 0.0
    vmin   = 0.0
    vmax   = 0.0

    !
    ! This loop will fail with invalid data
    !
    do
        read( 11, *, end = 900 ) value

        if ( nodata == 0 ) then
            vmin = value
            vmax = value
        else
            vmin = min( vmin, value )
            vmax = max( vmax, value )
        endif

        nodata = nodata + 1
        vmean  = vmean  + value

    enddo

900 continue
    vmean = vmean / nodata  ! This will fail with an empty file
end subroutine

! report_results --
!     Subroutine to write the output
! Arguments:
!     nodata            Number of data
!     vmean             Mean value
!     vmin              Minimum value
!     vmax              Maximum value
!
subroutine report_results( nodata, vmean, vmin, vmax )
    integer, intent(in)  :: nodata
    real, intent(in)     :: vmean
    real, intent(in)     :: vmin
    real, intent(in)     :: vmax

    if ( nodata > 0 ) then
        write( 20, '(a,i5)'    ) 'Number of data: ', nodata
        write( 20, '(a,f10.5)' ) 'Mean value:     ', vmean
        write( 20, '(a,f10.5)' ) 'Minimum value:  ', vmin
        write( 20, '(a,f10.5)' ) 'Maximum value:  ', vmax
    else
        write( 20, '(a,i5)' ) 'The file was empty - no data were read'
    endif
    close( 20 )
end subroutine

end module data_processing


! dataproc_testing --
!     Module for defining the unit tests
!
!     Expected routines:
!     - prolog: common code for preparing the unit tests
!     - epilog: common code for cleaning up the unit tests
!     - test: use the appropriate include file
!     - test_all (or a similar name): run the individual tests
!
!     All other routines form individual unit tests
!
module dataproc_testing
    use funit
    use data_processing
    implicit none
    private                 ! This is needed to make sure we can easily
    public :: test_all      ! use several such modules. The only external
                            ! name is "test_all"

    character(len=40) :: datafile
contains

!
! The code is generic, but this way we can access the private routines
! in this testing module. And we do not get any name conflicts
!
include "funit_test.f90"

subroutine prolog
    ! Nothing in this case
end subroutine prolog

subroutine epilog
    ! Remove the data file
    call funit_remove_file( datafile )
end subroutine epilog


! test_all --
!     Routine that simply runs all unit tests via the
!     general routine "test"
! Arguments:
!     None
!
subroutine test_all

    call test( test_no_file, "Read non-existent file" )
    call test( test_empty_file, "Read an empty file" )
    call test( test_invalid_file, "Read an invalid file" )
    call test( test_ordinary_file, "Read an ordinary file" )

end subroutine test_all

! write_name --
!     Small auxiliary routine (write the file with the file name)
! Arguments:
!     name         Name of the file
! Side effects:
!     Store the name in the datafile variable, so that it
!     can later be removed.
!
subroutine write_name( name )
    character(len=*) :: name

    open(  11, file = 'test_funit.inp' )
    write( 11, '(a)' ) name
    close( 11 )

    datafile = name
end subroutine write_name

! test_no_file --
!     Test: try to read a file that does not exist
! Arguments:
!     None
!
subroutine test_no_file

    integer :: nodata
    real    :: vmean, vmin, vmax

    call funit_remove_file( 'no_such_file' )
    call write_name( 'no_such_file' )

    call open_files
    call process_data( nodata, vmean, vmax, vmin )

    call assert_true( nodata == 0, "No data read" )

end subroutine test_no_file

! test_empty_file --
!     Test: try to read a file that is empty
! Arguments:
!     None
!
subroutine test_empty_file

    integer :: nodata
    real    :: vmean, vmin, vmax

    call funit_make_empty_file( 'empty_file' )
    call write_name( 'empty_file' )

    call open_files
    call process_data( nodata, vmean, vmax, vmin )

    call assert_true( nodata == 0, "No data read" )

end subroutine test_empty_file

! test_invalid_file --
!     Test: try to read a file that is not valid
! Arguments:
!     None
!
subroutine test_invalid_file

    integer :: nodata
    real    :: vmean, vmin, vmax

    open( 11, file = 'invalid_file' )
    write( 11, '(f10.3)' ) 1.0, 2.0, 3.0, 4.0
    write( 11, '(a)' ) 'AAAA'
    write( 11, '(f10.3)' ) 5.0, 6.0, 7.0, 8.0
    close( 11 )

    call write_name( 'invalid_file' )

    call open_files
    call process_data( nodata, vmean, vmax, vmin )

    call assert_true( nodata == 0, "No data read" )

end subroutine test_invalid_file

! test_ordinary_file --
!     Test: try to read a valid file and check the results
! Arguments:
!     None
!
subroutine test_ordinary_file

    integer :: nodata
    real    :: vmean, vmin, vmax

    open( 11, file = 'valid_file' )
    write( 11, '(f10.3)' ) 1.0, 2.0, 3.0, 4.0
    write( 11, '(f10.3)' ) 5.0, 6.0, 7.0, 8.0
    close( 11 )

    call write_name( 'valid_file' )

    call open_files
    call process_data( nodata, vmean, vmin, vmax )
    call report_results( nodata, vmean, vmin, vmax )

    call assert_true( nodata == 8, "All data are read correctly" )
    call assert_true( vmean  == 4.5, "Correct mean value" )
    call assert_true( vmin   == 1.0, "Correct minimum value" )
    call assert_true( vmax   == 8.0, "Correct maximum value" )

end subroutine test_ordinary_file

end module dataproc_testing

! program ---
!     Main program:
!     Do the ordinary processing and run the unit tests
!
program dataproc
    use funit
    use data_processing
    use dataproc_testing ! Module with specific unit tests

    implicit none

    integer :: nodata
    real    :: vmean, vmin, vmax

!
! The routine runtests will check if unit tests are requested
! If not, it will return immediately. This way we make sure
! the unit tests remain part of the program.
!
! The routine test_all runs all unit tests
! (see the dataproc_testing module)
!
    call runtests( test_all )

!
! Ordinary processing
!
    call open_files
    call process_data( nodata, vmean, vmin, vmax )
    call report_results( nodata, vmean, vmin, vmax )

end program
