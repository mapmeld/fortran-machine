! table_verification.f90 --
!     Module to facilitate unit testing of numerical routines by means
!     of tables.
!
!     Note:
!     This module was inspired by the article ... by Bil Kleb and ...
!
!     It contains two public routines:
!     - test_table: Reads a file with one or more tables with test cases
!                   and their results. Runs the tests via a user-defined
!                   routine.
!     - fillin_table: Reads a similar file with one or more tables with
!                   test cases but fills in the results.
!
!     The report from the first routine contains detailed information
!     about the tests: how well the results fit to the nominal results.
!     Output can be in plain text or HTML format.
!
!     The input file may look like this:
!
!     table: EULER-STANDARD
!     title: Tests for Euler method, using standard equation: y' = lambda * y
!     ! This set of test cases is meant to test the implementation of
!     ! the numerical integration method by Euler with fixed step sizes
!     !
!     absolute-tolerance: 1.0e-5
!     relative-tolerance: 1.0e-5
!     number-outputs: 1
!     !
!        h  xend   y0  lambda    y
!      [-]   [-]  [-]     [-]  [-]
!      0.1   1.0  1.0     1.0   0.21
!     0.01   1.0  1.0     1.0   0.35
!     ...
!
!     table: HEUN-STANDARD
!     title: Tests for Heun predictor-corrector method, using standard equation: y' = lambda * y
!     ! This set of test cases is meant to test the implementation of
!     ! the two-step numerical integration method by Heun with fixed step sizes
!     ...
!
!     In this file:
!     - all lines are at most 200 characters long
!     - lines starting with ! are explanatory text, they are copied
!       verbatim to the output
!     - lines which match the pattern "keyword: value" define the value
!       of some parameter.
!       Currently the keywords are:
!       table - signals the beginning of a new table, required
!       title - title for the report
!       absolute-tolerance - maximum allowed absolute difference from reference value
!               (defaults to "no check". Use "N/A" if you want to make that explicit)
!       relative-tolerance - maximum allowed relative difference
!               (if the reference value is zero, ignored; defaults to "no check"
!               Use "N/A" if you want to make that explicit)
!       number-outputs - number of columns representing output
!               parameters (defaults to 1)
!     - lines not starting with a "!" or a keyword represent the actual table
!       The first two lines are the names of the parameters and the units
!       These are useful for checking the order of the parameters for
!       instance. The first line also identifies the total number of columns
!       All other lines should contain numbers for the given parameters
!       and each line defines one test case. Output columns may also
!       contain a "-" or "Error" to indicate that the routine under test
!       should flag an out-of-range or other error.
!
!
!     $Id: table_verification.f90,v 1.1 2007/02/16 12:45:00 arjenmarkus Exp $
!
module table_verification_data
    implicit none

    real, parameter :: missing_value = -999.0

    type TABLE_PARAMETERS
        character(len=20), dimension(:), pointer :: name        => null()
        character(len=20), dimension(:), pointer :: unit        => null()
        integer                                  :: no_outputs
        real, dimension(:), pointer              :: values      => null()
        real                                     :: abs_tolerance
        real                                     :: rel_tolerance
        character(len=40)                        :: tablename
        character(len=200)                       :: title
    end type

contains

! init_parameters --
!     Routine to initialise the test parameters
! Arguments:
!     test_params      Parameters for testing
!
subroutine init_parameters( test_params )
    type(TABLE_PARAMETERS), intent(inout) :: test_params

    test_params%tablename     = ' '
    test_params%title         = ' '
    test_params%no_outputs    = 1
    test_params%abs_tolerance = -999.0
    test_params%rel_tolerance = -999.0

    deallocate( test_params%name   )
    deallocate( test_params%unit   )
    deallocate( test_params%values )
end subroutine init_parameters

end module table_verification_data

module table_verification
    use table_verification_data

    implicit none

    private
    public :: TABLE_PARAMETERS, missing_value, test_table, fillin_table

    integer, parameter :: maxlen = 200


contains

subroutine fillin_table( lurep, table_file, testfunc )
    integer, intent(in)          :: lurep
    character(len=*), intent(in) :: table_file

    interface
        subroutine testfunc( test_params, tablename, params, result, &
                       out_of_range, error )
            use table_verification_data
            implicit none
            type(TABLE_PARAMETERS)          :: test_params
            character(len=*), intent(in)    :: tablename
            real, dimension(:), intent(in)  :: params
            real, dimension(:), intent(out) :: result
            logical, intent(out)            :: out_of_range
            logical, intent(out)            :: error
        end subroutine testfunc
    end interface

    ! TODO

end subroutine fillin_table

! test_table --
!     Read the file with test cases and compare the output from the
!     routine under test with the reference values
! Arguments:
!     lurep            LU-number for report file
!     table_file       Name of the file containing the tables
!     testfunc         Subroutine to run the actual tests
!
subroutine test_table( lurep, table_file, testfunc )
    integer, intent(in)          :: lurep
    character(len=*), intent(in) :: table_file

    interface
        subroutine testfunc( test_params, tablename, params, result, &
                       out_of_range, error )
            use table_verification_data
            implicit none
            type(TABLE_PARAMETERS)          :: test_params
            character(len=*), intent(in)    :: tablename
            real, dimension(:), intent(in)  :: params
            real, dimension(:), intent(out) :: result
            logical, intent(out)            :: out_of_range
            logical, intent(out)            :: error
        end subroutine testfunc
    end interface

    integer                      :: luinp
    integer                      :: ierr
    integer                      :: no_params
    character(len=maxlen)        :: line
    type(TABLE_PARAMETERS)       :: test_params
    real, dimension(:), pointer  :: params
    real, dimension(:), pointer  :: result
    logical                      :: out_of_range
    logical                      :: error

    call free_lun( luinp )
    open( luinp, file=table_file )

LINES: do
        read( luinp, '(a)', iostat = ierr ) line
        if ( ierr /= 0 ) then
            close( luinp )
            exit
        endif

        !
        ! Identify the type of line and act accordingly
        !
        line = adjustl( line )
        select case ( identify_type( line ) )
            case( 'TEXT' )
                call write_text( lurep, line )
            case( 'KEYWORD' )
                call set_keyword( lurep, test_params, line )
            case( 'NAMEUNIT' )
                call set_names_units( lurep, test_params, line )
            case( 'TESTCASE' )
                out_of_range = .false.
                error        = .false.
                call get_parameters( lurep, test_params, line, error )
                if ( error ) cycle LINES

                no_params = size(test_params%values) - test_params%no_outputs
                params => test_params%values(1:no_params)
                result => test_params%values(no_params+1:)

                call testfunc( test_params, test_params%tablename, &
                         params, result, out_of_range, error )

                call write_testcase( lurep, test_params, params, result, &
                         out_of_range, error )
            case default
                call write_text( lurep, 'Programming error: unknown type' &
                         // identify_type( line ) )
        end select
    enddo &
LINES

end subroutine test_table

! free_lun --
!     Find a free LU-number
! Arguments:
!     lun          Resulting LU-number
!
subroutine free_lun( lun )
    integer, intent(out) :: lun

    integer              :: i
    logical              :: opend

    do i = 10,99
        inquire( unit = i, opened = opend )
        if ( .not. opend ) then
            lun = i
            exit
        endif
    enddo
end subroutine

! identify_type --
!     Identify the type of line
! Arguments:
!     line         The line in question
! Result:
!     Keyword identifying the type of input line
!
character(len=10) function identify_type( line )
    character(len=*) :: line

    integer          :: ierr
    integer          :: k
    real             :: x

    if ( line(1:1) == '!' ) then
        identify_type = 'TEXT'
    else if ( index( line, ':' ) > 0 ) then
        identify_type = 'KEYWORD'
    else
        read( line, *, iostat = ierr ) x
        if ( ierr == 0 ) then
            identify_type = 'TESTCASE'
        else
            identify_type = 'NAMEUNIT'
        endif
    endif
end function identify_type

! set_keyword --
!     Set the value for the keyword
! Arguments:
!     test_params  Test parameters
!     line         The line in question
!
subroutine set_keyword( lurep, test_params, line )
    integer, intent(in)                    :: lurep
    type( TABLE_PARAMETERS), intent(inout) :: test_params
    character(len=*)                       :: line

    character(len=20)                      :: keyword
    character(len=100)                     :: value
    integer                                :: ierr
    integer                                :: k

    k = index( line, ':' )

    keyword = line(1:k-1)
    value   = adjustl( line(k+1:) )
    ierr    = 0

    select case ( keyword )
        case( 'table' )
            call init_parameters( test_params )
            test_params%tablename = value
            test_params%title     = value
        case( 'absolute-tolerance' )
            if ( value == '-' .or. value == 'N/A' ) then
                test_params%abs_tolerance = -999.0
            else
                read( value, *, iostat=ierr ) test_params%abs_tolerance
            endif

        case( 'relative-tolerance' )
            if ( value == '-' .or. value == 'N/A' ) then
                test_params%rel_tolerance = -999.0
            else
                read( value, *, iostat=ierr ) test_params%rel_tolerance
            endif

        case( 'title' )
            test_params%title = value

        case( 'number-outputs' )
            read( value, *, iostat=ierr ) test_params%no_outputs

        case default
            write( lurep, * ) 'Unknown keyword: ', trim(keyword), ' (value: ', &
                trim(value), ')'
    end select

    if ( ierr /= 0 ) then
        write( lurep, * ) 'Error setting ', keyword, ' (value: ', &
            trim(value), ') Not a number?'
    endif

end subroutine set_keyword

! set_names_units --
!     Set the names or the units for the input parameters and the
!     results alike
! Arguments:
!     lurep        LU-number for reporting errors
!     test_params  Test parameters
!     line         The line in question
!
subroutine set_names_units( lurep, test_params, line )
    integer, intent(in)                      :: lurep
    type( TABLE_PARAMETERS), intent(inout)   :: test_params
    character(len=*)                         :: line

    character(len=20)                        :: pname
    character(len=20), dimension(:), pointer :: string
    integer                                  :: i
    integer                                  :: ierr
    integer                                  :: count

    !
    ! First find the number of items
    !
    count = 1
    do
        read( line, *, iostat = ierr ) ( pname, i=1,count )
        if ( ierr == 0 ) then
            count = count + 1
        else
            count = count - 1
            exit
        endif
    enddo

    !
    ! Then store them in te relevant arrays
    !
    allocate( string(1:count) )
    read( line, *, iostat = ierr ) ( string(i), i=1,count )

    if ( .not. associated(test_params%name) ) then
        test_params%name => string
        string => null()
    endif

    if ( .not. associated(test_params%unit) ) then
        test_params%unit => string
        string => null()
        if ( size(test_params%name) /= size(test_params%unit) ) then
            call write_error( lurep, "Number of units not equal to the &
                     &number of parameter names" )
            call write_error( lurep, "Line: "// trim(line) )
        endif
    endif

    if ( associated(string) ) then
        deallocate( string )
    endif
end subroutine set_names_units

! get_parameters --
!     Get the values of the parameters and the reference output
! Arguments:
!     lurep        LU-number for reporting errors
!     test_params  Test parameters
!     line         The line in question
!     error        Whether there was an error or not
!
subroutine get_parameters( lurep, test_params, line, error )
    integer, intent(in)                      :: lurep
    type( TABLE_PARAMETERS), intent(inout)   :: test_params
    character(len=*), intent(in)             :: line
    logical, intent(out)                     :: error

    character(len=20)                            :: pvalue
    character(len=20), dimension(:), allocatable :: string
    integer                                      :: i
    integer                                      :: ierr
    integer                                      :: count

    !
    ! First check the number of items
    !
    count = size( test_params%name )
    read( line, *, iostat = ierr ) ( pvalue, i=1,count )

    if ( ierr /= 0 ) then
        call write_error( lurep, "Number of values smaller than the &
                 &number of parameter names" )
        call write_error( lurep, "Line: "// trim(line) )
        return
    endif

    !
    ! Store the values
    !

    allocate( string(1:count) )
    read( line, * ) ( string(i), i=1,count )

    do i = 1,count
        if ( string(i) == '-' .or. string(i) == 'error' ) then
            test_params%values(i) = missing_value
        else
            read( string(i), *, iostat = ierr ) test_params%values(i)

            if ( ierr /= 0 ) then
                call write_error( lurep, "Invalid number for parameter" )
                call write_error( lurep, "Line: "// trim(line) )
                error = .true.
                return
            endif
        endif
    enddo

end subroutine get_parameters

! write_text --
!     Write text to the report file
! Arguments:
!     lurep        LU-number for reporting
!     text         Text to be written
!
subroutine write_text( lurep, text )
    integer, intent(in)          :: lurep
    character(len=*), intent(in) :: text

    write( lurep, '(a)' ) text
end subroutine write_text

! write_error --
!     Write error text to the report file
! Arguments:
!     lurep        LU-number for reporting
!     text         Text to be written
!
subroutine write_error( lurep, text )
    integer, intent(in) :: lurep
    character(len=*), intent(in) :: text

    write( lurep, '(2a)' ) 'ERROR: ', text
end subroutine

! write_testcase --
!     Write the result of a test case to the report file
! Arguments:
!     lurep          LU-number for reporting
!     test_params    All test parameters
!     params         Array of input parameters
!     result         Array of results
!     out_of_range   Indicates if the input parameters were out of range
!     error          Indicates some other error
!
subroutine write_testcase( lurep, test_params, params, result, out_of_range, error )
    integer, intent(in)                  :: lurep
    type(TABLE_PARAMETERS), intent(inout) :: test_params
    real, dimension(:), intent(in)       :: params
    real, dimension(:), intent(in)       :: result
    logical, intent(in)                  :: out_of_range
    logical, intent(in)                  :: error

    write( lurep, '(100g12.4)') params, result

    ! TODO: much more sophistication!
end subroutine write_testcase

end module table_verification
