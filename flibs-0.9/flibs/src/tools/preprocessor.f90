! preprocessor.f90 --
!     Preprocessor module for the program editcode
!
!     Notes on limitations:
!     - Assuming free-form code
!     - Keywords are not used as variable names
!
module preprocessor_module

    implicit none

    private

    integer, parameter, public :: fnamelen = 300
    integer, parameter, public :: codelen = 200
    integer, parameter, public :: wordlen = 32

    type preprocessor_type
        integer                 :: lurep              = 0
        integer                 :: luinp              = 10
        integer                 :: luout              = 11
        logical                 :: changed            = .false.
        logical                 :: preconditions      = .false.
        logical                 :: postconditions     = .false.
        logical                 :: assertions         = .false.
        logical                 :: implicit_none      = .false.
        character(len=fnamelen) :: input_directory    = '.'
        character(len=fnamelen) :: output_directory   = '.'
        character(len=codelen), dimension(:), pointer :: code_start        => null()
        character(len=codelen), dimension(:), pointer :: code_end          => null()
        character(len=codelen), dimension(:), pointer :: code_statement    => null()
        character(len=codelen), dimension(:), pointer :: use_statement     => null()
        character(len=codelen), dimension(:,:), pointer :: replace_type    => null()
        character(len=codelen), dimension(:,:), pointer :: replace_string  => null()
    end type preprocessor_type

    character(len=fnamelen)                       :: filename_macro
    character(len=codelen), dimension(40), target :: statement
    character(len=wordlen)                        :: module_name
    character(len=wordlen)                        :: routine_name
    integer                                       :: lineno

    public :: preprocessor_type
    public :: preprocess_init
    public :: preprocess_set
    public :: preprocess_report
    public :: preprocess_file
    public :: preprocess_input

contains

! preprocess_init --
!     Initialise the preprocessor structure
!
! Arguments:
!     preprocessor          The preprocessor structure in question
!
subroutine preprocess_init( preprocessor )
    type(preprocessor_type), intent(inout) :: preprocessor

    ! Note: leave lurep as it is

    preprocessor%changed            = .true.
    preprocessor%preconditions      = .false.
    preprocessor%postconditions     = .false.
    preprocessor%assertions         = .false.
    preprocessor%implicit_none      = .false.
    preprocessor%input_directory    = ' '
    preprocessor%output_directory   = ' '

    if ( associated( preprocessor%code_start     ) ) deallocate( preprocessor%code_start     )
    if ( associated( preprocessor%code_end       ) ) deallocate( preprocessor%code_end       )
    if ( associated( preprocessor%code_statement ) ) deallocate( preprocessor%code_statement )
    if ( associated( preprocessor%use_statement  ) ) deallocate( preprocessor%use_statement  )
    if ( associated( preprocessor%replace_type   ) ) deallocate( preprocessor%replace_type   )
    if ( associated( preprocessor%replace_string ) ) deallocate( preprocessor%replace_string )

end subroutine preprocess_init

! preprocess_input --
!     Handle the input string
!
! Arguments:
!     preprocessor          The preprocessor structure in question
!     line                  The input string containing a command
!
subroutine preprocess_input( preprocessor, line )
    type(preprocessor_type), intent(inout) :: preprocessor
    character(len=*)                       :: line

    character(len=codelen)                 :: keyword
    character(len=fnamelen)                :: filename

    if ( line == ' ' ) return   ! Nothing to do

    read( line, * ) keyword

    select case ( keyword )
        case( 'CLEAR-ALL-SETTINGS' )
            call preprocess_init( preprocessor )
            write( preprocessor%lurep, '(a)' ) 'All settings cleared'

        case( 'ENABLE-PRECONDITIONS' )
            preprocessor%changed       = .true.
            preprocessor%preconditions = logical_value( line )

        case( 'ENABLE-POSTCONDITIONS' )
            preprocessor%changed        = .true.
            preprocessor%postconditions = logical_value( line )

        case( 'ENABLE-ASSERTIONS' )
            preprocessor%changed    = .true.
            preprocessor%assertions = logical_value( line )

        case( 'ENABLE-IMPLICIT-NONE' )
            preprocessor%changed       = .true.
            preprocessor%implicit_none = logical_value( line )

        case( 'INPUT-DIRECTORY' )
            preprocessor%changed         = .true.
            preprocessor%input_directory = string_value( line )

        case( 'OUTPUT-DIRECTORY' )
            preprocessor%changed          = .true.
            preprocessor%output_directory = string_value( line )

        case( 'ADD-CODE-START' )
            preprocessor%changed = .true.
            call add_code( preprocessor%code_start, line )

        case( 'ADD-CODE-END' )
            preprocessor%changed = .true.
            call add_code( preprocessor%code_end, line )

        case( 'ADD-CODE-STATEMENT' )
            preprocessor%changed = .true.
            call add_code( preprocessor%code_statement, line )

        case( 'ADD-USE' )
            preprocessor%changed = .true.
            call add_code( preprocessor%use_statement, line )

        case( 'REPLACE-TYPE' )
            preprocessor%changed = .true.
            call add_replace( preprocessor%replace_type, line )

        case( 'REPLACE-STRING' )
            preprocessor%changed = .true.
            call add_replace( preprocessor%replace_string, line )

        case( 'FILE' )
            read( line, * ) keyword, filename
            write( preprocessor%lurep, '(2a)' ) 'Processing: ', trim(filename)
            write( preprocessor%lurep, '(2a)' ) '------------', &
                 repeat( '-', len_trim(filename) )
            write( *, '(2a)' ) 'Processing: ', trim(filename)
            if ( preprocessor%changed ) then
                call preprocess_report( preprocessor )
            endif
            call preprocess_file( preprocessor, filename )

        case( ' ' )
            ! Ignore empty lines

        case default
            write( preprocessor%lurep, '(2a)' ) 'Unknown keyword: ', trim(keyword)
    end select

end subroutine preprocess_input

! preprocess_set --
!     Set a parameter
!
! Arguments:
!     preprocessor          The preprocessor structure in question
!     report                LU-number for report file (optional)
!
subroutine preprocess_set( preprocessor, report )
    type(preprocessor_type), intent(inout) :: preprocessor
    integer, optional                      :: report

    if ( present( report ) ) then
        preprocessor%lurep = report
    endif
end subroutine preprocess_set

! logical_value --
!     Retrieve a logical value from the input
!
! Arguments:
!     line                  Line containing the logical value
!
logical function logical_value( line )
    character(len=*)                  :: line

    character(len=20)                 :: dummy
    character(len=20)                 :: value
    integer                           :: ierr

    read( line, *, iostat = ierr ) dummy, value
    if ( ierr /= 0 ) then
        value = 'NO'    ! TODO: report this!
    endif

    if ( value == 'yes' .or. value == 'Yes' .or. value == 'YES' ) then
        logical_value = .true.
    else
        logical_value = .false.
    endif

end function logical_value

! string_value --
!     Retrieve a string value from the input
!
! Arguments:
!     line                  Line containing the logical value
!
function string_value( line )
    character(len=*)                  :: line
    character(len=len(line))          :: dummy
    character(len=len(line))          :: string_value
    integer                           :: ierr

    read( line, *, iostat = ierr ) dummy, string_value
    if ( ierr /= 0 ) then
        string_value = ' '    ! TODO: report this!
    endif

end function string_value

! get_name --
!     Retrieve the name (truncated at open paren)
!
! Arguments:
!     line                  Line containing the logical value
!     name                  Name (second word in the line)
!
subroutine get_name( line, name )
    character(len=*)                  :: line
    character(len=*)                  :: name
    character(len=len(line))          :: dummy
    integer                           :: ierr
    integer                           :: k

    read( line, *, iostat = ierr ) dummy, name
    if ( ierr /= 0 ) then
        name = ' '    ! TODO: report this!
    endif

    k = index( name, '(' )
    if ( k > 0 ) then
        name = name(1:k-1)
    endif

end subroutine get_name

! get_first_word --
!     Retrieve the first word (truncated at open paren, etc.)
!
! Arguments:
!     line                  Line containing the logical value
!     name                  Name (first word in the line)
!
! Note:
!     Could partly be merged with get_name
!
subroutine get_first_word( line, word )
    character(len=*)                  :: line
    character(len=*)                  :: word
    character(len=len(line))          :: dummy
    integer                           :: ierr
    integer                           :: k

    read( line, *, iostat = ierr ) word
    if ( ierr /= 0 ) then
        word = ' '    ! TODO: report this!
    endif

    k = index( word, '(' )
    if ( k > 0 ) then
        word = word(1:k-1)
    endif

end subroutine get_first_word

! tolower --
!     Convert to lower case
!
! Arguments:
!     line                  Line to be converted
!
function tolower( line )
    character(len=*)                          :: line
    character(len=len(line))                  :: tolower

    integer                                   :: i
    integer, parameter                        :: offset = iachar('A') - iachar('a')

    tolower = line
    do i = 1,len_trim(line)
        if ( iachar(line(i:i)) >= iachar('A') .and. iachar(line(i:i)) <= iachar('Z') ) then
            tolower(i:i) = achar(iachar(line(i:i))-offset)
        endif
    enddo
end function tolower

! add_code --
!     Add a line of code to the array
!
! Arguments:
!     array                 Array with code lines
!     line                  Line containing the code
!
subroutine add_code( array, line )
    character(len=*), dimension(:), pointer :: array
    character(len=*)                        :: line

    character(len=1)                                      :: dummy
    character(len=len(array(1))), dimension(:), pointer :: new_array
    integer                                             :: length

    length = 1
    if ( associated(array) ) then
        length = size(array) + 1
    endif
    allocate( new_array(length) )

    if ( length > 1 ) then
        new_array(1:length-1) = array
    endif

    read( line, * ) dummy, new_array(length)

    if ( associated(array) ) then
        deallocate( array )
    endif
    array => new_array

end subroutine add_code

! add_replace --
!     Add a replace pair to the array
!
! Arguments:
!     array                 Array with replace pairs
!     line                  Line containing the code
!
subroutine add_replace( array, line )
    character(len=*), dimension(:,:), pointer :: array
    character(len=*)                          :: line

    character(len=1)                                        :: dummy
    character(len=len(array(1,1))), dimension(:,:), pointer :: new_array
    integer                                                 :: length

    length = 1
    if ( associated(array) ) then
        length = size(array,2) + 1
    endif
    allocate( new_array(2,length) )

    if ( length > 1 ) then
        new_array(:,1:length-1) = array
    endif

    read( line, * ) dummy, new_array(1,length), new_array(2,length)

    if ( associated(array) ) then
        deallocate( array )
    endif
    array => new_array

end subroutine add_replace

! preprocess_report --
!     Produce a report of the preprocessing parameters
!
! Arguments:
!     preprocessor          The preprocessor structure in question
!
subroutine preprocess_report( preprocessor )
    type(preprocessor_type), intent(inout) :: preprocessor

    integer                                :: i

    write( preprocessor%lurep, '(a,a)'  ) 'Input directory:  ', trim(preprocessor%input_directory)
    write( preprocessor%lurep, '(a,a)'  ) 'Output directory: ', trim(preprocessor%output_directory)
    write( preprocessor%lurep, '(a,l5)' ) 'Preconditions:        ', preprocessor%preconditions
    write( preprocessor%lurep, '(a,l5)' ) 'Postconditions:       ', preprocessor%postconditions
    write( preprocessor%lurep, '(a,l5)' ) 'Assertions:           ', preprocessor%assertions
    write( preprocessor%lurep, '(a,l5)' ) 'Insert IMPLICIT NONE: ', preprocessor%implicit_none

    if ( associated(preprocessor%code_start) ) then
        write( preprocessor%lurep, '(a)' ) 'Code inserted at the start:'
        write( preprocessor%lurep, '(4x,a)' ) &
            ( trim(preprocessor%code_start(i)), i=1,size(preprocessor%code_start) )
    else
        write( preprocessor%lurep, '(a)' ) 'Code inserted at the start: none'
    endif

    if ( associated(preprocessor%code_end) ) then
        write( preprocessor%lurep, '(a)' ) 'Code inserted at the end (RETURN, STOP, END):'
        write( preprocessor%lurep, '(4x,a)' ) &
            ( trim(preprocessor%code_end(i)), i=1,size(preprocessor%code_end) )
    else
        write( preprocessor%lurep, '(a)' ) 'Code inserted at the end: none'
    endif

    if ( associated(preprocessor%code_statement) ) then
        write( preprocessor%lurep, '(a)' ) 'Code inserted after each executable statement:'
        write( preprocessor%lurep, '(4x,a)' ) &
            ( trim(preprocessor%code_statement(i)), i=1,size(preprocessor%code_statement) )
    else
        write( preprocessor%lurep, '(a)' ) 'Code inserted after each executable statement: none'
    endif

    if ( associated(preprocessor%replace_type) ) then
        write( preprocessor%lurep, '(a)' ) 'Replace the following types:'
        write( preprocessor%lurep, '(4x,3a)' ) &
            ( trim(preprocessor%replace_type(1,i)), ' ==> ', &
              trim(preprocessor%replace_type(2,i)) ,i=1,size(preprocessor%replace_type,2) )
    else
        write( preprocessor%lurep, '(a)' ) 'No type replacements'
    endif

    if ( associated(preprocessor%replace_string) ) then
        write( preprocessor%lurep, '(a)' ) 'Replace the following strings:'
        write( preprocessor%lurep, '(4x,3a)' ) &
            ( trim(preprocessor%replace_string(1,i)), ' ==> ', &
              trim(preprocessor%replace_string(2,i)) ,i=1,size(preprocessor%replace_string,2) )
    else
        write( preprocessor%lurep, '(a)' ) 'No string replacements'
    endif

end subroutine preprocess_report

! open_files --
!     Open the input and output file
!
! Arguments:
!     preprocessor          The preprocessor structure in question
!     filename              Name of the input file
!     success               Whether successful or not
!
subroutine open_files( preprocessor, filename, success )
    type(preprocessor_type), intent(inout) :: preprocessor
    character(len=*)                       :: filename
    logical                                :: success

    integer                                :: i
    integer                                :: ierr
    character(len=2), dimension(2), save   :: dirsep
    data dirsep / '/', '\\' /                 ! Prevent errors from compilers that
                                              ! use a backslash as an escape

    success = .false.
    do i = 1,2
        open( preprocessor%luinp, &
              file = trim(preprocessor%input_directory) // dirsep(i)(1:1) // &
                     trim(filename), iostat = ierr, status = 'old' )
        if ( ierr == 0 ) then
            success = .true.
            exit
        endif
    enddo

    if ( .not. success ) then
        write( *, '(2a)' ) '==> Error while processing ', trim(filename)
        write( preprocessor%lurep, '(a,/,3x,a)' ) 'ERROR: Problem opening the input file:', &
            trim(preprocessor%input_directory) // dirsep(1)(1:1) // trim(filename)
        return
    endif

    success = .false.
    do i = 1,2
        open( preprocessor%luout, &
              file = trim(preprocessor%output_directory) // dirsep(i)(1:1) // &
                     '_' // trim(filename), iostat = ierr, status = 'new' )
        if ( ierr == 0 ) then
            success = .true.
            exit
        endif
    enddo

    if ( .not. success ) then
        write( *, '(2a)' ) '==> Error while processing ', trim(filename)
        write( preprocessor%lurep, '(a,/,3x,a)' ) 'ERROR: Problem opening the output file as new:', &
            trim(preprocessor%output_directory) // dirsep(1)(1:1) // '_' // trim(filename)
        write( preprocessor%lurep, '(a,/,3x,a)' ) '   (Note: the file should not exist)'
        return
    endif

    !
    ! Success, report it
    !
     write( preprocessor%lurep, '(2a)' ) '   Input:', &
            trim(preprocessor%input_directory) // dirsep(1)(1:1) // trim(filename), &
            '   Output:', &
            trim(preprocessor%output_directory) // dirsep(1)(1:1) // '_' // trim(filename)

end subroutine

! get_statement --
!     Get the complete statement from the file
!
! Arguments:
!     luinp                 LU-number input file
!     line                  Line of lines containing the statement
!     lineno                Line number in the file
!     eof                   End of file?
!
subroutine get_statement( luinp, line, lineno, eof )
    integer                                 :: luinp
    character(len=*), dimension(:), pointer :: line
    integer                                 :: lineno
    logical                                 :: eof

    integer                                 :: length
    integer                                 :: ierr
    character(len=1)                        :: lastc
    integer                                 :: i

    eof = .false.

    do i = 1,size(statement)
        read( luinp, '(a)', iostat = ierr ) statement(i)
        if ( ierr /= 0 ) then
            eof = .true.
            return
        endif
        lineno = lineno + 1

        length = len_trim(statement(i))
        lastc  = statement(i)(length:length)
        if ( lastc /= '&' ) then
            line => statement(1:i)
            return
        endif
    enddo

end subroutine get_statement

! write_statement --
!     Write the statement (possibly with replacements) to the file
!
! Arguments:
!     luout                 LU-number output file
!     line                  Line of lines containing the statement
!     replace               Replace the macros?
!
subroutine write_statement( luout, line, replace )
    integer                         :: luout
    character(len=*), dimension(:)  :: line
    logical                         :: replace

    integer                         :: i

    if ( replace ) then
        do i = 1,size(line)
            write( luout, '(a)' ) trim(macros_replaced(line(i)))
        enddo
    else
        do i = 1,size(line)
            write( luout, '(a)' ) trim(line(i))
        enddo
    endif

end subroutine write_statement

! transform_condition --
!     Rewrite pre/postconditions and assertions
!
! Arguments:
!     line                  Line of lines containing the statement
!
! Note:
!     Use the variable statement directly!
!
subroutine transform_condition( line )
    character(len=*), dimension(:), pointer :: line  ! We expand the array of strings

    integer                                 :: i
    integer                                 :: k

    do i = 1,size(line)
        k = index( line(i), '!' )
        if ( k > 0 ) then          ! This always is the case!
            line(i)(1:k) = ' '
        endif
    enddo

    k = index( line(1), 'assert:' )
    if ( k > 0 ) then
        line(1)(1:k+6) = ' '
    endif

    k = index( line(1), 'pre:' )
    if ( k > 0 ) then
        line(1)(1:k+3) = ' '
    endif

    k = index( line(1), 'post:' )
    if ( k > 0 ) then
        line(1)(1:k+4) = ' '
    endif

    do i = size(line)+1,1,-1
        statement(i) = '   ' // adjustl(statement(i-1))
    enddo

    statement(1) = 'if ( &'
    statement(size(line)+2) = ') then'
    statement(size(line)+3) = 'write(*,*) "ERROR: condition failed at line __LINE__ in file __FILE__"'

    do i = 1,size(line)
        statement(size(line)+3+i) = 'write(*,*) "' // trim(statement(i+1)) // '"'
    enddo
    statement(2*size(line)+4) = 'stop'
    statement(2*size(line)+5) = 'endif'

    line => statement(1:2*size(line)+5)

end subroutine transform_condition

! macros_replaced --
!     Replace the standard macros
!
! Arguments:
!     line                  Line possibly containing macros
!
function macros_replaced( input )
    character(len=*)          :: input
    character(len=len(input)) :: macros_replaced

    character(len=len(input)) :: line

    character(len=20)         :: lineno_macro

    logical                   :: replaced
    integer                   :: k
    integer                   :: length

    line = input

    do
        replaced = .false.
        k = index( line, '__LINE__' )
        if ( k > 0 ) then
            replaced = .true.
            write( lineno_macro, '(i0)' ) lineno
            length = len_trim(lineno_macro)
            line(k+length:)    = line(k+8:)
            line(k:k+length-1) = lineno_macro
        endif

        k = index( line, '__FILE__' )
        if ( k > 0 ) then
            replaced = .true.
            length = len_trim(filename_macro)
            line(k+length:)    = line(k+8:)
            line(k:k+length-1) = filename_macro
        endif

        k = index( line, '__MODULE__' )
        if ( k > 0 ) then
            replaced = .true.
            length = len_trim(module_name)
            line(k+length:)    = line(k+10:)
            line(k:k+length-1) = module_name
        endif

        k = index( line, '__ROUTINE__' )
        if ( k > 0 ) then
            replaced = .true.
            length = len_trim(routine_name)
            line(k+length:)    = line(k+11:)
            line(k:k+length-1) = routine_name
        endif

        if ( .not. replaced ) then
            exit
        endif

    enddo

    macros_replaced = line

end function macros_replaced

! replace_type --
!     Replace the declared type by another type
!
! Arguments:
!     replaces              List of types to be replaced
!     line                  Line or lines containing the statement
!
! Note:
!     Only the first line of the declaration is treated.
!     So it won't work on:
!         real :: k; real :: x
!
!     Take care that we replace the complete type, not simply a substring
!
subroutine replace_type( replaces, line )
    character(len=*), dimension(:,:)  :: replaces
    character(len=*), dimension(:)    :: line

    character(len=len(line(1)))       :: part
    integer                           :: i
    integer                           :: k
    integer                           :: length
    integer                           :: length_replace

    part = tolower(line(1))
    do i = 1,size(replaces,2)
        k = index( part, trim(replaces(1,i)) )
        if ( k > 0 ) then
            if ( part(1:k-1) == ' ' ) then
                length = len_trim(replaces(1,i))
                part   = adjustl(part(k+length:))

                if ( part(1:1) /= '(' ) then
                    length_replace = len_trim(replaces(2,i))
                    line(1)(k+length_replace:)    = line(1)(k+length:)
                    line(1)(k:k+length_replace-1) = replaces(2,i)
                endif
            endif
        endif
    enddo

end subroutine replace_type

! check_return_stop --
!     Check if the statement is of the form "if (...) return"
!
! Arguments:
!     line                  Line of lines containing the statement
!     keyword               Keyword with which the statement ends
!     transformed           Set to true if we have to write "if (...) then"
!
subroutine check_return_stop( line, keyword, transformed )
    character(len=*), dimension(:)  :: line
    character(len=*)                :: keyword
    logical                         :: transformed

    character(len=len(line(1)))     :: line_copy
    integer                         :: k
    integer                         :: kparen

    transformed = .false.
    line_copy = tolower(line(size(line)))

    kparen = index( line_copy, ')', .true. )

    k      = index( line_copy, 'return' )

    if ( k > 0 ) then
        if ( line_copy(k:) == 'return' .and. kparen < k ) then
            line(size(line))(k:) = 'then'
            keyword     = 'return'
            transformed = .true.
        endif
    endif

    k = index( line_copy, 'stop' )

    if ( k > 0 ) then
        if ( line_copy(k:) == 'stop' .and. kparen < k ) then
            line(size(line))(k:) = 'then'
            keyword     = 'stop'
            transformed = .true.
        endif
    endif

end subroutine check_return_stop

! preprocess_file --
!     Preprocess an input file
!
! Arguments:
!     preprocessor          The preprocessor structure in question
!     filename              Name of the input file
!
! Notes:
!     The routine assumes that the code is in free-form!
!
!     It seems that multiple identical use statements are okay
!     (checked with gfortran and g95). Therefore no check for this
!
subroutine preprocess_file( preprocessor, filename )
    type(preprocessor_type), intent(inout) :: preprocessor
    character(len=*)                       :: filename

    character(len=codelen), dimension(:), pointer :: line
    character(len=codelen)                 :: line_copy
    character(len=wordlen)                 :: first_word
    character(len=wordlen)                 :: dummy
    character(len=wordlen)                 :: keyword
    logical                                :: success
    logical                                :: add_use
    logical                                :: add_start
    logical                                :: add_end
    logical                                :: use_added
    logical                                :: start_added
    logical                                :: in_module
    logical                                :: in_routine
    logical                                :: in_contains
    logical                                :: implicit_found
    logical                                :: in_decl
    logical                                :: eof
    logical                                :: written
    logical                                :: transformed
    integer                                :: luinp
    integer                                :: luout
    integer                                :: ierr
    integer                                :: k
    integer                                :: in_try
    integer                                :: try_label
    character(len=wordlen), dimension(2)   :: keywords = &
        (/ 'subroutine', 'function  ' /)

    call open_files( preprocessor, filename, success )
    if ( .not. success ) return

    filename_macro = filename
    luinp          = preprocessor%luinp
    luout          = preprocessor%luout

    lineno         = 0

    add_use        = .false.
    add_start      = .false.
    add_end        = .false.
    use_added      = .false.
    start_added    = .false.
    in_try         = 0
    in_decl        = .true.   ! To avoid comments at the start

    eof            = .false.
    do while ( .not. eof )
        written = .false.
        call get_statement( luinp, line, lineno, eof )
        if ( eof ) then
            cycle
        endif

        line_copy = adjustl( tolower(line(1)) )

        if ( line_copy(1:1) /= '!' .and. &
             line_copy      /= ' '       ) then
            !
            ! Quick characterisation of the line
            !
            call get_first_word( line_copy, first_word )

            select case ( first_word )
                case ( 'module', 'program' )
                    in_module      = .true.
                    in_contains    = .false.
                    implicit_found = .false.
                    in_routine     = .false.
                    add_use        = .true.
                    use_added      = .false.
                    in_decl        = .true.

                    if ( first_word == 'program' ) then
                        add_start   = .true.
                        start_added = .false.
                        call get_name( line_copy, routine_name )
                    endif

                    call get_name( line_copy, module_name )

                case ( 'use' )
                    ! TODO: check against the ones we need to add?

                case ( 'contains' )
                    if ( preprocessor%implicit_none .and. &
                         .not. implicit_found             ) then
                        write(luout,*) 'implicit none'
                    endif
                    in_contains = .true.
                    in_decl     = .true.

                case ( 'subroutine' )
                    in_routine = .true.
                    add_use    = .not. in_module
                    use_added  = .false.
                    add_start  = .true.
                    in_decl    = .true.
                    in_try     = 0
                    try_label  = 1111
                    call get_name( line_copy, routine_name )

                case ( 'function' )
                    ! More required!
                    in_routine = .true.
                    add_use    = .not. in_module
                    use_added  = .false.
                    add_start  = .true.
                    in_decl    = .true.
                    in_try     = 0
                    try_label  = 1111
                    call get_name( line_copy, routine_name )

                case ( 'endsubroutine', 'endfunction' )
                    in_decl    = .true.
                    in_routine = .false.

                case ( 'endmodule', 'endprogram' )
                    in_decl    = .true.
                    in_module   = .false.
                    in_contains = .false.

                case ( 'end' )
                    call get_name( line_copy, keyword )
                    if ( keyword == 'module' .or. keyword == 'program' ) then
                        in_decl     = .true.
                        in_module   = .false.
                        in_contains = .false.
                    elseif ( keyword == ' ' ) then
                        in_decl = .true.
                        add_end = .true.
                    elseif ( keyword == 'subroutine' .or. keyword == 'function' ) then
                        in_decl    = .true.
                        in_routine = .false.
                        add_end    = .true.
                    endif

                case ( 'integer', 'real', 'character', 'logical', 'type', &
                       'parameter', 'private', 'public', 'interface', 'data' )
                    if ( preprocessor%implicit_none .and. &
                         .not. implicit_found       .and. &
                         .not. in_contains                ) then
                        implicit_found = .true.
                        write(luout,*) 'implicit none'
                    endif

                    in_decl = .true.
                    if ( associated(preprocessor%replace_type) ) then
                        call replace_type( preprocessor%replace_type, line )
                    endif

                case ( 'return', 'stop' )
                    add_end = .true.

                case ( 'try' )
                    in_try = in_try + 1
                    write( luout, '(a)' ) '! TRY'
                    write( luout, '(a)' ) 'exception_catch = exception_catch + 1'

                case ( 'endtry' )
                    in_try = in_try - 1
                    try_label = try_label + 1
                    write( luout, '(a)' ) '! ENDTRY'
                    write( luout, '(i5,a)' ) try_label, ' continue'
                    write( luout, '(a)' ) 'exception_catch = exception_catch - 1'

                case ( 'catch' )
                    in_try = in_try - 1
                    write( luout, '(a)' )    '! CATCH'
                    write( luout, '(a,i5)' ) 'goto ', try_label+1
                    write( luout, '(i5,a)' ) try_label, ' continue'
                    write( luout, '(a)' )    'exception_thrown = .false.'

                case ( 'throw' )
                    write( luout, '(3a,i5,a)' ) 'call exception_throw( ''', &
                        trim(filename), ''',', lineno, ',&'
                    line(1) = adjustl(line(1))
                    line(1) = line(1)(7:)
                    call write_statement( luout, line, .false. )
                    write( luout, '(a)' ) 'return'
                    written = .true.

                case default
                    in_decl = .false.

                    ! Detect such lines as: if ( ... ) return
                    if ( associated(preprocessor%code_end) ) then
                        call check_return_stop( line, keyword, transformed )
                        if ( transformed ) then
                            written = .true.
                            call write_statement( luout, line, .true. )
                            call write_statement( luout, preprocessor%code_end, .true. )
                            write( luout, '(a)' ) keyword
                            write( luout, '(a)' ) 'endif'
                        endif
                    endif

            end select
        elseif ( line_copy(1:1) == '!' ) then
            if ( preprocessor%assertions .and. &
                 index( line_copy, 'assert:' ) > 0 ) then
                call transform_condition( line )
            endif
            if ( preprocessor%preconditions .and. &
                 index( line_copy, 'pre:' ) > 0 ) then
                call transform_condition( line )
            endif
            if ( preprocessor%postconditions .and. &
                 index( line_copy, 'post:' ) > 0 ) then
                call transform_condition( line )
            endif
        endif

        !
        ! Add the lines that should come after the statement
        !
        if ( in_try > 0 ) then
            write( luout, '(a,i5)' )    'if ( exception_thrown ) then'
            write( luout, '(3a,i5,a)' ) '     call exception_setpos( ''', &
                trim(filename), ''',', lineno, ')'
            write( luout, '(a,i5)' )    '     goto ', try_label
            write( luout, '(a,i5)' )    'endif'
        endif

        if ( add_use ) then
            add_use = .false.
            if ( associated(preprocessor%use_statement) .and. .not. use_added ) then
                use_added = .true.
                written   = .true.
                call write_statement( luout, line, .true. )
                call write_statement( luout, preprocessor%use_statement, .false. )
            endif
        endif

        if ( add_start .and. .not. in_decl ) then
            add_start = .false.
            if ( associated(preprocessor%code_start) .and. .not. start_added ) then
                start_added = .true.
                call write_statement( luout, preprocessor%code_start, .true. )
            endif
        endif

        if ( add_end ) then
            add_end = .false.
            if ( associated(preprocessor%code_end) ) then
                call write_statement( luout, preprocessor%code_end, .true. )
            endif
        endif

        if ( .not. in_decl ) then
            if ( associated(preprocessor%code_statement) ) then
                written = .true.
                call write_statement( luout, line, .true. )
                call write_statement( luout, preprocessor%code_statement, .true. )
            endif
        endif

        if ( .not. written ) then
            call write_statement( luout, line, .true. )
        endif

    enddo

    close( luinp )
    close( luout )

end subroutine preprocess_file

end module preprocessor_module
