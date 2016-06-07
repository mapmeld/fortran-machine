! editcode.f90 --
!     A preprocessor for Fortran code: its purpose is to transform
!     the code according to certain simple rules:
!     - Add an "IMPLICIT NONE" statement at the beginning of a module or
!       routine
!     - Replace one type by another (for instance: real ==> real(dp))
!     - Instrument the code (add extra statements)
!     - Enable preconditions/postconditions/assertions
!
!     It reads an input file called "editcode.inp" which should contain
!     keywords and possibly parameters, steering the transformation of
!     the code.
!
!     Here is a list of the keywords and their parameters:
! .   INPUT-DIRECTORY dirname         - Directory to expect the input
!                                       source files
! .   OUTPUT-DIRECTORY dirname        - Directory to put the output
!                                       source files in
! .   FILE filename                   - Name of the source file to process
! .   ADD-CODE-START code             - Line of code to add at the start
!                                       of a routine. If more than one
!                                       line is required, then just use
!                                       several such keywords
! .   ADD-CODE-END code               - Ditto at the end of a routine
!                                       (this includes: RETURN, STOP and END)
! .   ADD-CODE-STATEMENT code         - Line of code to add _after_ each statement
! .   ADD-USE code                    - Add a USE statement
! .   REPLACE-TYPE old new            - Replace variable type "old" by variable type "new"
!     REPLACE-STRING old new          - Replace any string "old" by string "new"
! .   ENABLE-IMPLICIT-NONE yes/no     - Add an IMPLICIT NONE statement
!                                       if none is present
! .   ENABLE-PRECONDITIONS yes/no     - Enable preconditions
! .   ENABLE-POSTCONDITIONS yes/no    - Ditto for postconditions
! .   ENABLE-ASSERTIONS yes/no        - Ditto for assertions
! .   CLEAR-ALL-SETTINGS              - Re-initialise the preprocessing information
!                                       (everything is set to the default again)
! .   INCLUDE filename                - Read keywords from the given
!                                       file before processing the rest
!                                       of this input file (multiple levels possible)
! .   __FILE__                        - Macro replaced by the name of the current
!                                       source file
! .   __LINE__                        - Macro replaced by the current line number
! .   __ROUTINE__                     - Macro replaced by the current routine name
! .   __MODULE__                      - Macro replaced by the current module name
!
!     Preconditions, postconditions and assertions are implemented as
!     special comments:
!
!         ! pre: x > 0.0
!         ! post: x > 0.0
!         ! assert: x > 0.0
!
!     If a condition is longer than one line, simply use & like any
!     ordinary continuation line:
!
!     assert: x > 0.0 .and. &
!             y < 0.0
!
!     If the condition type is enabled, the condition is transformed
!     into code like this:
!
!     if ( .not. ( &
!         x > 0.0 .and. &
!         y > 0.0 &
!     ) then
!         write(*,*) 'Assertion failed at line 10 in file myprog.f90:'
!         write(*,*) 'x > 0.0 .and. &'
!         write(*,*) 'y > 0.0'
!     endif
!
! ==> The program also handles a simple form of exceptions via
!     try/catch statements:
!     PM
!
!     Note:
!     - Each argument must be surrounded by " or ' if it contains spaces.
!       (The lines are read via list-directed input)
!     - The INCLUDE statement is treated in the main program, all the
!       others are treated by the preprocessor module.
!     - Comment lines begin with !
!     - The default settings are such that _nothing_ is done.
!
!     To make this preprocessing facility flexible, it consists of a
!     main program and a module that does the actual work:
!
!     - preprocess_init sets or resets the preprocessor data
!     - preprocess_input takes a line of code and handles the
!       information it contains
!     - preprocess_file processes the given input file
!
program editcode
    use preprocessor_module

    type(preprocessor_type) :: preprocessor
    integer                 :: ierr
    integer                 :: luinp
    integer                 :: lurep
    character(len=200)      :: line
    character(len=fnamelen) :: filename

    call preprocess_init( preprocessor )

    luinp = 21
    lurep = 20
    open( luinp, file = 'editcode.inp', status = 'old', iostat = ierr )
    open( lurep, file = 'editcode.rep' )

    call preprocess_set( preprocessor, report = lurep )

    if ( ierr /= 0 ) then
        write(lurep,'(a)') 'Error: Could not open input file "editcofe.inp". Nothing to do!'
        write(*,'(a)')     'Error: Could not open input file "editcofe.inp". Nothing to do!'
        stop
    endif

    !
    ! Read the input file line by line:
    ! - Echo what we have read
    ! - Skip comments
    ! - Handle INCLUDE statements
    ! - Pass all others to the module
    !
    do
        read( luinp, '(a)', iostat = ierr ) line

        if ( ierr < 0 ) then
            close( luinp )
            write( lurep, '(a)' ) 'Closing input file'
            write( lurep, '(a)' ) ' '

            if ( luinp == 21 ) then
                exit
            else
                luinp = luinp - 1
            endif
        endif

        write( lurep, '(a,a)' ) 'INPUT: ',line

        k  = index( line, '!' )
        if ( k > 0 ) then
            if ( line(1:k-1) == ' ' ) then
                cycle
            endif
        endif

        k  = index( line, 'INCLUDE' )
        if ( k > 0 ) then
            if ( line(1:k-1) == ' ' ) then
                luinp = luinp + 1
                read( line, * ) keyword, filename
                open( luinp, file = filename, status = 'old', iostat = ierr )

                if ( ierr /= 0 ) then
                    write(lurep,'(a)') 'Error: Could not open input file ', trim(filename)
                    write(lurep,'(a)') 'Program stopped'
                    write(*,'(a)')     'Error: Could not open input file ', trim(filename)
                    stop
                endif

                cycle ! Done with this line
            endif
        endif

        !
        ! Any other line: pass it on
        !
        call preprocess_input( preprocessor, line )
    enddo

    write(*,'(a)') 'Preprocessing done'

end program
