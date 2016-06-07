! fwrapper.f90 --
!     Program to generate a C wrapper libeary from Fortran source
!     code, such that the Fortran routines can be used from C.
!
!     TODO:
!     Almost everything
!
!     Easier parsing: one space between words instead of none
!
!     Watch out for moduleprocedure
!     Separate definition of parameter, private, public
!     Private and public in all cases
!     Continued lines: as one line in the stripped version
!     Valid Fortran names that are reserved words in C
!     Dimension(:)
!     Kind
!     Old style declarations: character*20
!     Fortran pointers and allocatables in derived types
!     Interfaces for function/subroutine arguments
!     Split statements via ;
!
!     $Id: fwrapper.f90,v 1.3 2008/03/17 17:57:56 arjenmarkus Exp $
!
program fwrapper
    implicit none
    character(len=20) :: srcname
    character(len=20) :: filename
    integer           :: lufiles    = 10
    integer           :: lusrc      = 11
    integer           :: luchead    = 12
    integer           :: lucwrap    = 13
    integer           :: lufwrap    = 14
    integer           :: lufiso     = 15
    integer           :: lufstrip   = 16
    integer           :: ierr
    integer           :: k
    logical           :: first      = .true.

    open( lufiles, file = 'fwrapper.inp', status = 'old' )

    do
        read( lufiles, '(a)', iostat = ierr ) srcname
        if ( ierr /= 0 ) exit

        !
        ! First step: open the source file, and if not done yet,
        ! the files with the resulting C and Fortran wrapper code
        !
        open( lusrc, file = srcname, status = 'old' )

        k       = index( srcname, '.', .true. )
        srcname = srcname(1:k-1)

        if ( first ) then
            first = .false.
            filename = trim(srcname) // "_wrap.h"
            open( luchead, file = filename )
            filename = trim(srcname) // "_wrap.c"
            open( lucwrap, file = filename )
            filename = trim(srcname) // "_wrap.f90"
            open( lufwrap, file = filename )
            filename = trim(srcname) // "_iso.f90"  ! F2003 style
            open( lufiso, file = filename )

            write( luchead, '(a)' ) &
                '#ifdef __cplusplus', &
                '    extern "C" {', &
                '#endif'
        endif

        !
        ! Second step: strip the source code
        !
        open( lufstrip, file = 'fwrapper.stripped' )
        call strip_source( lusrc, lufstrip )
        close( lusrc )
        rewind( lufstrip )

        !
        ! Third step: analyse the stripped code and
        ! generate the wrapper
        !
        call analyse_source( lufstrip, luchead, lucwrap, lufwrap, lufiso )
    enddo

    write( luchead, '(a)' ) &
         '#ifdef __cplusplus', &
         '    }', &
         '#endif'
contains

! lower_case --
!     Return the lower case version of a character
!
! Arguments:
!     ch              Character to be converted
!
character(len=1) function lower_case( ch )
    character(len=1), intent(in) :: ch

    integer, parameter :: shift = iachar('a') - iachar('A')

    if ( iachar(ch) >= iachar('A') .and. iachar(ch) <= iachar('Z') ) then
        lower_case = achar(iachar(ch)+shift)
    else
        lower_case = ch
    endif
end function lower_case

! all_caps --
!     Return the upper case version of an entire string
!
! Arguments:
!     string          String to be converted
!
function all_caps( string ) result(caps)
    character(len=*), intent(in) :: string
    character(len=len(string))   :: caps

    character(len=1)             :: ch
    integer                      :: i

    integer, parameter :: shift = iachar('A') - iachar('a')

    do i = 1,len(string)
        ch = string(i:i)
        if ( iachar(ch) >= iachar('a') .and. iachar(ch) <= iachar('z') ) then
            caps(i:i) = achar(iachar(ch)+shift)
        else
            caps(i:i) = ch
        endif
    enddo
end function all_caps

! strip_source --
!     Strip the source code from all uninteresting bits
!
! Arguments:
!     lusrc           LU-number of the source code
!     lufstrip        LU-number of the temporary file
!
! Note:
!     For the moment only free-form source files!
!
subroutine strip_source( lusrc, lufstrip )
    integer, intent(in) :: lusrc
    integer, intent(in) :: lufstrip

    character(len=150)  :: line         ! More than enough, even for wide source files
    character(len=150)  :: compact_line
    integer             :: ierr
    integer             :: i
    integer             :: j
    integer             :: k
    integer             :: length
    logical             :: continued

    character(len=20), dimension(23) :: keyword = &
        (/ 'type', 'result', 'subroutine', 'function', 'recursive', 'module', &
           'integer', 'real', 'doubleprecision', 'logical', 'character',      &
           'complex', 'endfunction', 'endsubroutine', 'endtype', 'endmodule', &
           'parameter', 'dimension', 'contains', 'interface', 'endinterface', &
           'private', 'public'                                                /)

    continued = .false.
    do
        read( lusrc, '(a)', iostat = ierr ) line
        if ( ierr /= 0 ) exit

        !
        ! Remove any comments and empty lines
        !
        k = index( line, '!' )
        if ( k > 0 ) then
            line = line(1:k-1)
        endif
        if ( line == ' ' ) cycle

        !
        ! Compact the source line
        !
        ! TODO:
        ! parameter(k=1)
        ! splitting on semicolon
        !
        compact_line = compact_source_line( line )
        write(*,'(a)') compact_line

        !
        ! Now: filter out those lines that are not part of a declaration
        !

        if ( compact_line == 'end' .or. continued ) then
            continued = index( compact_line, '&' ) > 0
            write( lufstrip, '(a)' ) compact_line
            cycle
        endif
        do j = 1,size(keyword)
            if ( index( compact_line, trim(keyword(j)) ) == 1 ) then
                continued = index( compact_line, '&' ) > 0
                write( lufstrip, '(a)' ) compact_line
                exit
            endif
        enddo
    enddo

end subroutine strip_source

! compact_source_line --
!     Change the source line so that it is easier to parse later on
!
! Arguments:
!     line            Line to be treated
!
function compact_source_line( line )
    character(len=*), intent(inout) :: line
    character(len=len(line))        :: compact_source_line

    integer :: i
    integer :: j
    integer :: open_parens
    integer :: colons_added
    logical :: in_quotes
    logical :: previous_space
    character(len=9), parameter :: before_colon = ')rlxn1248'

    j              = 0
    open_parens    = 0
    colons_added   = 0
    in_quotes      = .false.
    previous_space = .false.
    do i = 1,len_trim(line)
        if ( line(i:i) == ' ' ) then
            if ( compact_source_line(1:j) == 'double' ) then
                previous_space = .false.
            else
                previous_space = .true.
            endif
            cycle
        else
            !
            ! TODO: parameter(k=1)
            !
            j = j + 1
            if ( previous_space .and. &
                 index( before_colon, compact_source_line(j:j) ) > 0 ) then
                colons_added = colons_added ! Sanity check
                compact_source_line(j:) = '::'
                j = j + 2
            endif
            previous_space   = .false.

            if ( in_quotes ) then
                compact_source_line(j:) = line(i:i)
            else
                compact_source_line(j:) = lower_case(line(i:i))
            endif
        endif

        if ( line(i:i) == '(' ) then
            open_parens = open_parens + 1
        endif

        if ( line(i:i) == ')' ) then
            open_parens = open_parens - 1
            if ( open_parens < 0 ) then
                write(*,*) 'Incorrect number of parentheses: '
                write(*,*) '    ', trim(line)
            endif
        endif

        if ( line(i:i) == '''' .or. line(i:i) == '"' ) then
            in_quotes = .not. in_quotes
        endif

        if ( line(i:i) == ',' .and. open_parens > 0 ) then
            compact_source_line(j:j) = '|'
        endif
    enddo

    if ( colons_added > 1 ) then
        write(*,*) 'More than one pair of colons added:'
        write(*,*) '    Original: ', trim(line)
        write(*,*) '    Result:', trim(compact_source_line)
    endif

end function compact_source_line


! analyse_source --
!     Analyse the source code and delegate the actual wrapper generation
!
! Arguments:
!     lufstrip        LU-number of the temporary file
!     luchead         LU-number of the C header file
!     lucwrap         LU-number of the file with C wrapper code
!     lufwrap         LU-number of the file with Fortran wrapper code
!     lufiso          LU-number of the file with Fortran wrapper code (F2003)
!
subroutine analyse_source( lufstrip, luchead, lucwrap, lufwrap, lufiso )
    integer, intent(in) :: lufstrip, luchead, lucwrap, lufwrap, lufiso

    character(len=150) :: line
    character(len=64)  :: module_name
    integer            :: ierr
    logical            :: in_module
    logical            :: module_header
    logical            :: type_definition
    logical            :: public_items

    in_module     = .false.
    module_header = .false.
    public_items  = .true.
    do
        read( lufstrip, '(a)', iostat = ierr ) line
        if ( ierr /= 0 ) exit

        if ( line(1:6) == 'module' .and. .not. module_header ) then
            module_name   = line(7:)
            in_module     = .true.
            module_header = .true.
        endif
        if ( line(1:7) == 'contains' ) then
            module_header = .false.
        endif
        if ( line(1:9) == 'endmodule' ) then
            in_module     = .false.
            module_name   = ' '
        endif

        !
        ! TODO: allow this to transpire!
        !
        if ( line(1:7) == 'public ' ) then ! Note the space!
            public_items = .true.
        endif
        if ( line(1:7) == 'private ' ) then ! Note the space!
            public_items = .false.
        endif

        if ( line(1:10) == 'subroutine' ) then
            call define_subr( line, public_items, module_name, lufstrip, &
                     luchead, lucwrap, lufwrap, lufiso )
            cycle
        endif
        if ( line(1:4) == 'type' .and. line(5:5) /= '(' ) then
            call define_type( line, public_items, lufstrip, luchead, lucwrap, &
                     lufwrap, lufiso )
            cycle
        endif
        if ( data_type(line) ) then
            if ( index( line, 'function' ) <= 0 ) then
                write( luchead, '(2a)' ) define_c_var( line ), ';'
!           else
!               call define_function( line, public_items, lufstrip, luchead, lucwrap, &
!                    lufwrap, lufiso )
            endif
            cycle
        endif
    enddo
end subroutine analyse_source

! data_type --
!     Does the line define a new variable (it starts with a data type)?
!
! Arguments:
!     line            Line of code to examine
!
logical function data_type( line )
    character(len=*) :: line

    integer          :: i
    character(len=20),dimension(7) :: type_keyword = &
        (/ 'integer', 'logical', 'real', 'doubleprecision', 'complex', &
           'character', 'type('                                        /)

    data_type = .false.
    do i = 1,size(type_keyword)
        if ( index( line, trim(type_keyword(i)) ) == 1 ) then
            data_type = .true.
            exit
        endif
    enddo
end function data_type

! define_type --
!     Define the derived type
!
! Arguments:
!     line            Line to examine
!     public_items    Is the default "public access"?
!     lufstrip        LU-number of the temporary file
!     luchead         LU-number of the C header file
!     lucwrap         LU-number of the file with C wrapper code
!     lufwrap         LU-number of the file with Fortran wrapper code
!     lufiso          LU-number of the file with Fortran wrapper code (F2003)
!
subroutine define_type( line, public_items, lufstrip, luchead, lucwrap, lufwrap, lufiso )
    character(len=*)    :: line
    logical             :: public_items
    integer, intent(in) :: lufstrip, luchead, lucwrap, lufwrap, lufiso

    character(len=64)        :: typename
    character(len=len(line)) :: next_line
    character(len=len(line)) :: c_definition
    integer                  :: ierr

    typename = line(5:)

    write( luchead, '(a)' ) 'typedef struct {'

    do
        read( lufstrip, '(a)', iostat = ierr ) next_line
        if ( ierr /= 0 ) exit

        if ( next_line(1:7) == 'endtype' ) then
            write( luchead, '(3a)' ) '} ', trim(typename), ';'
            exit
        else
            c_definition = define_c_var( next_line )
            if ( c_definition == ' ' ) cycle
            if ( c_definition == '--' ) then
                write( *, * ) 'Impossible conversion: ', next_line
            else
                write( luchead, '(3a)' ) '    ', trim(c_definition), ';'
            endif
        endif
    enddo

end subroutine define_type

! define_c_var --
!     Convert the Fortran definition into a C equivalent
!
! Arguments:
!     line            Line to examine
!
! Result:
!     Equivalent C definition or empty if it is not necessary (public :: x)
!     or "--" if not possible
!
function define_c_var( line ) result(c_var)
    character(len=*)         :: line
    character(len=len(line)) :: c_var

    integer                         :: i
    integer                         :: k
    integer                         :: k2
    integer                         :: skip
    integer                         :: length
    character(len=20)               :: typename
    character(len=20), dimension(7) :: ftype
    character(len=20), dimension(7) :: ctype
    logical                         :: ischar

    data ( ftype(i), ctype(i), i = 1,7 ) &
        / 'integer',            'int',    &
          'real',               'float',  &
          'doubleprecision',    'double', &
          'logical',            'int',    &
          'character',          'char',   &
          'complex',            'complx', &
          'type(',              ''        /

    c_var = ' '
    !
    ! TODO: kind
    !
    typename = '?'
    skip     = 0
    do i = 1,size(ftype)
        length = len_trim(ftype(i))
        if ( line(1:length) == ftype(i) ) then
            typename = ctype(i)
            skip     = length
            exit
        endif
    enddo

    if ( line(1:7)  == 'private'           ) return
    if ( line(1:6)  == 'public'            ) return
    if ( index( line, ',pointer' ) > 0 ) then
        c_var = '--'
        return
    endif

    !
    ! Handle:
    ! - double colon
    ! - parameter
    ! - string length
    ! - derived types
    ! - intent
    !
    k = index( line, '::' )
    if ( k > 0 ) then
        line(k:) = line(k+2:)
    endif

    ischar = .false.
    k = index( line, '(len=' )
    if ( k > 0 ) then
        k2 = index( line, ')' )
        if ( line(k+5:k2-1) /= '*' ) then
            line = line(1:k-1) // trim(line(k2+1:)) // '[' // line(k+5:k2-1) // ']'
        else
            line = line(1:k-1) // '*' // trim(line(k2+1:))
        endif
        ischar = .true.
    endif

    k = index( line, ',parameter' )
    if ( k > 0 ) then
        line(k:) = line(k+10:)
        k = index( line, '=' )
        line(k:k) = ' '
        if ( ischar ) then
            c_var = '#define "' // trim(line(skip+1:)) // '"'
        else
            c_var = '#define ' // trim(line(skip+1:))
        endif
        return
    endif

    if ( line(1:5) == 'type(' ) then
        k = index( line, ')' )
        line = line(1:k-1) // '* ' // line(k+1:)
    endif

    k = index(line, ',intent(' )
    if ( k > 0 ) then
        k2 = k - 1 + index( line(k:), ')' )
        line = line(1:k-1) // line(k2+1:)
    endif

    c_var = trim(typename) // ' ' // trim(line(skip+1:))

end function define_c_var

! define_subr --
!     Define the subroutine
!
! Arguments:
!     line            Line to examine
!     public_items    Is the default "public access"?
!     lufstrip        LU-number of the temporary file
!     luchead         LU-number of the C header file
!     lucwrap         LU-number of the file with C wrapper code
!     lufwrap         LU-number of the file with Fortran wrapper code
!     lufiso          LU-number of the file with Fortran wrapper code (F2003)
!
subroutine define_subr( line, public_items, module_name, lufstrip, &
               luchead, lucwrap, lufwrap, lufiso )
    character(len=*)    :: line
    logical             :: public_items
    character(len=*)    :: module_name
    integer, intent(in) :: lufstrip, luchead, lucwrap, lufwrap, lufiso

    character(len=len(line)) :: next_line
    character(len=len(line)) :: c_definition
    character(len=64)        :: full_name
    character(len=64)        :: subname
    character(len=64)        :: varname
    character(len=64)        :: dummy
    character(len=4)         :: indent
    integer                  :: k

    k       = index( line, '(' )
    subname = line(11:k-1)
    indent  = '    '


    if ( module_name /= ' ' ) then
        full_name = 'mod_' // trim(module_name) // '_' // trim(subname)
    else
        full_name = trim(subname)
    endif

    write( luchead, '(10a)' ) ' '
    write( luchead, '(10a)' ) '#ifdef FTN_ALL_CAPS'
    write( luchead, '(10a)' ) '#define ', trim(full_name), ' ', trim(all_caps(full_name))
    write( luchead, '(10a)' ) '#endif'
    write( luchead, '(10a)' ) ' '
    write( luchead, '(10a)' ) '#define ', trim(subname), ' ', trim(full_name)
    write( luchead, '(10a)' ) 'void ', trim(full_name), '('

    write( lufwrap, '(10a)' ) 'subroutine ', trim(full_name), '( &'
!   call write_fort_arglist( lufwrap, line )
    write( lufwrap, '(a)'   ) '    )'

    if ( module_name /= ' ' ) then
        write( lufwrap, '(10a)' ) '    use ', trim(module_name)
    endif

    !
    ! NOTE: This assumes the arguments are defined in the order
    !       given in the subroutine declaration!!

    do
        read( lufstrip, '(a)', iostat = ierr ) next_line
        if ( ierr /= 0 ) exit

        if ( next_line(1:13) == 'endsubroutine' .or. next_line == 'end') then
            write( luchead, '(3a)' ) ');'
            write( lufwrap, '(3a)' ) 'endsubroutine ', trim(subname)
            exit
        else
            c_definition = define_c_var( next_line )
            if ( c_definition == ' ' ) cycle
            if ( c_definition == '--' ) then
                write( *, * ) 'Unsupported conversion: ', next_line
            else
                read( c_definition, * ) dummy, varname
                if ( varname(1:1) == '*' ) then
                    varname = varname(2:)
                endif

                !
                ! Is the variable name part of the declaration?
                !
                ! NOTE: This must be much subtler!
                !
                if ( index( line, trim(varname) ) > 0 ) then
                    write( luchead, '(3a)' ) indent, trim(c_definition)
                    indent = '   ,'
                endif
            endif
        endif
    enddo

end subroutine define_subr

end program
