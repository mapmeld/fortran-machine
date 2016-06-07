! codefrag.f90 --
!     Auxiliary program to write a module that contains code fragments
!
!     The idea:
!     If you write a program generator, you need a lot of code
!     fragments, possibly with simple keywords that are to be replaced
!     by actual names and so on.
!     Putting the lines of code in a program is tedious:
!
!     character(len=80), dimension(4) :: code_write_header =   &
!     (/ 'open( 10, file = "XXXX" )                          ',&
!        'write( 10, * ) ''Program started at TIME''         ',&
!        'write( 10, * ) ''Input file: FILE''                ',&
!        'write( 10, * ) '' ''                               '/)
!
!     This program helps to automate the process:
!     - It creates a module with only one subroutine: to initialise the
!       string arrays holding the information
!     - It takes care of replacing embedded quotes by double quotes
!     - It writes all required declarations
!
!     The program takes an input file like:
!     MODULENAME := report
!     write_header :=
!         open( 10, file = "XXXX" )
!         write( 10, * ) 'Program started at TIME'
!         write( 10, * ) 'Input file: FILE'
!         write( 10, * ) ' '
!     write_footer :=
!         write( 10, * ) '--------------'
!         close( 10 )
!     The line "MODULENAME := ..." must appear before the other lines
!     (any occurrence of it starts a new module)
!
!     A line like "write_header =" causes the declaration of a variable
!     "write_header" of type character(len=80), dimension(...)
!     (The length is 80 by default, but a line "LENGTH := 100" will
!     change that. Lines should not be longer than 200 characters).
!
!     Comments start with ! in the first column (so that embedded
!     exclamation marks do not require special treatment)
!
!     The subroutine to initialise the variables is called init_<modulename>
!
program codefrag

    use string_manipulation

    implicit none

    integer                   :: length = 80
    character(len=80)         :: modulename
    character(len=80)         :: prevmodule
    character(len=80)         :: variable
    character(len=80)         :: prevvar = ' '
    character(len=80)         :: input_file
    character(len=200)        :: code_line
    logical                   :: in_module = .false.
    integer                   :: count = 0
    integer                   :: ierr
    integer                   :: k

    !
    ! Open the input file
    !
    open( 10, file = 'codefrag.inp', status = 'old', iostat = ierr )
    if ( ierr /= 0 ) then
        write( *, * ) 'Could not open file "codefrag.inp"!'
        stop
    endif

    read( 10, '(a)' ) input_file
    open( 11, file = input_file, status = 'old', iostat = ierr )
    if ( ierr /= 0 ) then
        write( *, * ) 'Could not open file "',trim(input_file), '"'
        stop
    endif

    close( 10 )

    open( 20, file = 'module_text' )  ! TODO!
    open( 21, file = 'module1') !status = 'scratch' )
    open( 22, file = 'module2') !status = 'scratch' )

    !
    ! Read the input file and generate the module
    !
    do
        read( 11, '(a)', iostat = ierr ) code_line
        write(*,*) trim(code_line)
        if ( ierr /= 0 ) exit

        if ( code_line(1:1) == '!' ) cycle

        if ( index( code_line, 'MODULENAME' ) == 1 ) then
            write(*,*) 'Module started'
            k = index( code_line, ':=' )
            modulename = adjustl(code_line(k+2:))
            call start_module( modulename, in_module )
        elseif ( index( code_line, 'LENGTH' ) == 1 ) then
            k = index( code_line, ':=' )
            read( code_line(k+2:), * ) length
        elseif ( index( code_line, ':=' ) > 0 ) then
            k     = index( code_line, ':=' )
            variable = adjustl(code_line(1:k-1))
            write(*,*) 'Variable: ', trim(variable)
            call handle_variable( variable, length, count )
        else
            write(*,*) 'Code: ', trim(code_Line)
            call handle_code( code_line, variable, count )
        endif
    enddo

    call handle_variable( ' ', length, count )
    call append_files

    close( 11 )
contains

subroutine append_files
    character(len=200) :: line
    integer            :: ierr

    rewind( 21 )
    rewind( 22 )

    !
    ! Declaration of the variables
    !
    do
        read( 21, '(a)', iostat = ierr ) line
        if ( ierr /= 0 ) exit
        write( 20, '(a)' ) trim(line)
    enddo
    !
    ! Implementation of the initialisation routine
    !
    write( 20, '(a)' ) 'contains'
    write( 20, '(a)' ) 'subroutine init_' // prevmodule
    do
        read( 22, '(a)', iostat = ierr ) line
        if ( ierr /= 0 ) exit
        write( 20, '(a)' ) trim(line)
    enddo

    write( 20, '(a)' ) 'end subroutine init_' // prevmodule
    write( 20, '(a)' ) 'end module ' // prevmodule

end subroutine append_files

subroutine start_module( name, in_module )
    character(len=*), intent(in) :: name
    logical, intent(inout)       :: in_module

    if ( in_module ) then
        call append_files
    endif

    in_module = .true.
    prevmodule = name
    write( 20, '(2a)' ) 'module ',trim(name)
end subroutine

subroutine handle_variable( name, length, count )
    character(len=*), intent(in) :: name
    integer, intent(in)          :: length
    integer, intent(inout)       :: count

    if ( prevvar /= ' ' ) then
        write( 21, '(a,i0,a,i0,a,a)' ) &
        'character(len=', length, '), dimension(',count, ') :: ', trim(prevvar)
    endif

    count   = 0
    prevvar = name

end subroutine handle_variable

subroutine handle_code( line, variable, count )
    character(len=*), intent(inout) :: line
    character(len=*), intent(in)    :: variable
    integer, intent(inout)          :: count

    count = count + 1

    write(*,*) 'In code'
    call string_map( line, "'", "''" )
    write(*,*) 'In code - na string_map'

    !
    ! TODO: very long lines!
    !
    write( 22, '(2a,i0,a)' ) &
        trim(variable), '(', count, ') = &'
    write( 22, '(3a)' ) &
        "'",trim(line),"'"

    write(*,*) 'Uit code'
end subroutine handle_code
end program
