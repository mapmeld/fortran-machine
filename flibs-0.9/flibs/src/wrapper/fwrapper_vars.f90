! fwrapper_vars.f90 --
!     Module for managing the declarations of variables.
!     Belongs to fwrapper.f90
!
! variables --
!     Module that keeps track of variable declarations and
!     offers a number of query facilities
!
!     Note:
!     This could be the basis for a documentation tool too
!
!     Note:
!     The current implementation breaks when variable names
!     contain complete keywords like "dimension" or "pointer".
!     It is not that robust.
!
module variables

    implicit none

    character(len=40), dimension(:,:), allocatable, private :: type_conversion

    integer, private, parameter :: max_length = 64
    type var_info
        character(len=max_length) :: name
        character(len=max_length) :: c_name
        character(len=max_length) :: type_name
        character(len=max_length) :: dimensions
        character(len=max_length) :: char_length
        logical                   :: bind_c
        logical                   :: by_value
        logical                   :: intent_in
        logical                   :: is_parameter
    end type var_info

!!  private

contains

! init_variables --
!     Initialisation routine
!
subroutine init_variables

    call read_table( 'fwrapper.table' )

end subroutine init_variables

! read_table --
!     Read the table to convert Fortran types to C
!
! Arguments:
!     filename         Name of the file storing the conversion information
!
subroutine read_table( filename )

    character(len=*), intent(in) :: filename

    integer           :: i
    integer           :: ierr
    integer           :: k
    integer           :: count
    character(len=80) :: line

    open( 10, file = filename, status = 'old' )
    open( 11, status = 'scratch' )

    count = 0
    do
        read( 10, '(a)', iostat = ierr ) line
        if ( ierr /= 0 ) exit

        k = index( line, '!' )
        if ( k > 0 ) then
            line = line(1:k-1)
        endif
        if ( line /= ' ' ) then
            count = count + 1
            write( 11, '(a)' ) line
        endif
    enddo

    allocate( type_conversion(2,count) )
    rewind( 11 )

    do i = 1,count
        read( 11, * ) ( type_conversion(k,i), k = 1,2 )
    enddo

    close( 10 )
    close( 11 )

end subroutine read_table


! add_var --
!     Add new variable information to the list of variables
!
! Arguments:
!     vars              Array of variable information structures (output)
!     info              New information structure
!
subroutine add_var( vars, info )

    type(var_info), dimension(:), pointer :: vars
    type(var_info)                        :: info

    type(var_info), dimension(:), pointer :: newvars
    integer :: i

    allocate( newvars(1:size(vars)+1) )

    newvars(1:size(vars)) = vars
    newvars(size(vars)+1) = info

    if ( associated(vars) ) then
        deallocate( vars )
    endif
    vars => newvars

end subroutine add_var


! convert_declarations --
!     Convert the declarations from a Fortran source file to something
!     more suitable for analysis
!
! Arguments:
!     declarations      (Preprocessed) source lines to be analysed
!     vars              Array of variable information structures (output)
!
! Note:
!     Only the default implicit rules are considered, if an explicit
!     declaration was omitted. (Not the implicit statement)
!
subroutine convert_declarations( declarations, vars )

    character(len=*), dimension(:)        :: declarations
    type(var_info), dimension(:), pointer :: vars

    character(len=max_length), dimension(:), pointer :: name
    type(var_info)                                   :: info

    integer :: i
    integer :: j

    nullify( name )
    allocate( name(0) )

    do i = 1,size(declarations)
        call get_varnames( declarations(i), name )

        do j = 1,size(name)
            write(*,*) name(j)
            info%name       = name(j)
            info%type_name  = default_type(name(j))
            info%c_name     = name(j)
            info%dimensions = ' '
            info%bind_c     = .false.
            info%intent_in  = .false.
            info%by_value   = .false.

            call get_type(         declarations, info )
            call get_dimensions(   declarations, info )
          !!call get_c_name(       declarations, info )
!!          call get_bind_c(       declarations, info )
!!          call get_intent_in(    declarations, info )
!!          call get_by_value(     declarations, info )
!!          call get_is_parameter( declarations, info )
          !!call get_char_length(  declarations, info )

            call add_var( vars, info )
        enddo
        deallocate( name )
    enddo
end subroutine convert_declarations


! find_varname --
!     Find a variable name in a list of declarations
!
! Arguments:
!     declarations      (Preprocessed) source lines to be analysed
!     varname           Name of the variable to be found
!     start             Start index in source lines
!     line_index        Index of the line containing it
!     decoration_index  Index of the decoration of the variable name in the line
!
subroutine find_varname( declarations, varname, start, line_index, decoration_index )

    character(len=*), dimension(:),intent(in) :: declarations
    character(len=*), intent(in)              :: varname
    integer, intent(in)                       :: start
    integer, intent(out)                      :: line_index
    integer, intent(out)                      :: decoration_index

    integer :: i
    integer :: j
    integer :: k

    line_index       = -1
    decoration_index = -1


    do i = start,size(declarations)
        j = index( declarations(i), '::' )
        do
            k = index( declarations(i)(j:), trim(varname) )
            if ( k > 0 ) then
                j = j + k - 1
                k = j + len_trim(varname)
                if ( index( ',:',  declarations(i)(j-1:j-1) ) > 0 .and. &
                     index( ',*( ', declarations(i)(k:k) )    > 0 ) then
                    line_index       = i
                    decoration_index = k
                    if ( index( '*(', declarations(i)(k:k) ) <= 0 ) then
                        decoration_index = -1 ! No particular decoration
                    endif
                    exit
                else
                    j = k + 1
                endif
            else
                exit
            endif
        enddo
        if ( line_index > 0 ) then
            exit ! We are done
        endif
    enddo
end subroutine find_varname


! default_type --
!     Use the default rules to determine the type of a variable
!
! Arguments:
!     varname           Name of the variable
!
! Returns:
!     A string 'integer' or 'real'
!     Only the default implicit rules are considered, if an explicit
!     declaration was omitted. (Not the implicit statement)
!
character(len=10) function default_type( varname )
    character(len=*), intent(in) :: varname

    if ( index( 'ijklmn', varname(1:1) ) > 0 ) then
        default_type = 'integer'
    else
        default_type = 'real'
    endif
end function default_type


! get_type --
!     Extract the Fortran (basic) type of a variable from a set of
!     declarations
!
! Arguments:
!     declarations      List of lines containing the relevant declarations
!     info              Information about the variable
!
! Note:
!     Only the default implicit rules are considered, if an explicit
!     declaration was omitted. (Not the implicit statement)
!
subroutine get_type( declarations, info )

    character(len=*), dimension(:), intent(in) :: declarations
    type(var_info), intent(inout)              :: info

    character(len=max_length)                  :: varname

    logical           :: found
    integer           :: i
    integer           :: start
    integer           :: line_index
    integer           :: decoration_index
    character(len=20), dimension(7), save :: basic_type = &
        (/ 'integer', 'real', 'doubleprecision', 'complex', &
           'logical', 'character', 'type' /)

    varname    = info%name

    !
    ! Assumption:
    ! - Either the variable's type is given on the first line to hold its name
    ! - Or the default rules should be used
    !
    found = .false.
    start      = 1
    call find_varname( declarations, varname, start, line_index, decoration_index )

    write(*,*) 'Varname: ', trim(varname), line_index
    write(*,*) 'Line: ',trim(declarations(line_index)), ' - ', decoration_index
    if ( line_index > 0 ) then
        read( declarations(line_index), * ) info%type_name

        do i = 1,size(basic_type)
            if ( index( info%type_name, trim(basic_type(i)) ) == 1 ) then
                found = .true.
                exit
            endif
        enddo
    endif
    if ( .not. found ) then
        info%type_name = default_type( info%name )
    endif

    if ( info%type_name(1:9) == 'character' ) then
        info%type_name = 'character'
    endif

end subroutine get_type


! get_dimensions --
!     Retrieve the dimension specification - if any - of a variable
!
! Arguments:
!     declarations      List of lines containing the relevant declarations
!     info              Information about the variable
!
subroutine get_dimensions( declarations, info )

    character(len=*), dimension(:), intent(in) :: declarations
    type(var_info), intent(inout)              :: info

    character(len=max_length)                  :: varname

    logical           :: found
    integer           :: start
    integer           :: kcolon
    integer           :: kdim
    integer           :: line_index
    integer           :: decoration_index

    varname    = info%name
    start      = 1
    found      = .false.
    do
        call find_varname( declarations, varname, start, line_index, decoration_index )
        if ( line_index > 0 ) then
            write(*,*) 'Declaration: ',trim(declarations(line_index))
            if ( decoration_index > 0 ) then
                if ( declarations(line_index)(decoration_index:decoration_index) == '(' ) then
                    read( declarations(line_index)(decoration_index:), * ) &
                        info%dimensions
                    found = .true.
                    write(*,*) 'Found - ', trim(declarations(line_index)(decoration_index:))
                endif
            else
                kcolon = index( declarations(line_index), '::' )
                kdim   = index( declarations(line_index)(1:kcolon), 'dimension(' )
                if ( kdim > 0 ) then
                    read( declarations(line_index)(kdim+9:kcolon-1), * ) &
                        info%dimensions
                    found = .true.
                    write(*,*) 'Found 2 - ', trim(declarations(line_index)(1:kcolon))
                endif
            endif

            if ( found ) then
                exit
            else
                start = line_index + 1
            endif
        else
            exit
        endif
    enddo

end subroutine get_dimensions


! get_intent_in --
!     Determine if the variable has the intent(in) attribute
!
! Arguments:
!     declarations      List of lines containing the relevant declarations
!     info              Information about the variable
!
! Note:
!     The routine fails if the string "intent(in)" is actually used as
!     a length or dimension - unlikely, but it could happen.
!     The same for the other routines of this type. They could fail
!     because of this.
!     It does not seem worthwhile though to guard against that:
!     the chances of this happening should be small and I regard
!     the use of keywords as a variable/parameter name as unappropriate
!
subroutine get_intent_in( declarations, info )

    character(len=*), dimension(:), intent(in) :: declarations
    type(var_info), intent(inout)              :: info

    character(len=max_length)                  :: varname

    integer           :: start
    integer           :: kcolon
    integer           :: line_index
    integer           :: decoration_index

    varname    = info%name
    start      = 1
    do
        call find_varname( declarations, varname, start, line_index, decoration_index )
        if ( line_index > 0 ) then
            kcolon             = index( declarations(line_index), '::' )
            info%intent_in = &
                index( declarations(line_index)(1:kcolon), 'intent(in)' ) > 0
            if ( info%intent_in ) then
                exit
            endif
        else
            exit
        endif
    enddo

end subroutine get_intent_in


! get_is_parameter --
!     Determine if the variable has the parameter attribute
!
! Arguments:
!     declarations      List of lines containing the relevant declarations
!     info              Information about the variable
!
subroutine get_is_parameter( declarations, info )

    character(len=*), dimension(:), intent(in) :: declarations
    type(var_info), intent(inout)              :: info

    character(len=max_length)                  :: varname

    integer           :: start
    integer           :: kcolon
    integer           :: line_index
    integer           :: decoration_index

    varname    = info%name
    start      = 1
    do
        call find_varname( declarations, varname, start, line_index, decoration_index )
        if ( line_index > 0 ) then
            kcolon             = index( declarations(line_index), '::' )
            info%is_parameter = &
                index( declarations(line_index)(1:kcolon), 'parameter' ) > 0
            if ( info%is_parameter ) then
                exit
            endif
        else
            exit
        endif
    enddo

end subroutine get_is_parameter


! get_by_value --
!     Determine if the variable has the value attribute
!
! Arguments:
!     declarations      List of lines containing the relevant declarations
!     info              Information about the variable
!
subroutine get_by_value( declarations, info )

    character(len=*), dimension(:), intent(in) :: declarations
    type(var_info), intent(inout)              :: info

    character(len=max_length)                  :: varname

    integer           :: start
    integer           :: kcolon
    integer           :: line_index
    integer           :: decoration_index

    varname    = info%name
    start      = 1
    do
        call find_varname( declarations, varname, start, line_index, decoration_index )
        if ( line_index > 0 ) then
            kcolon             = index( declarations(line_index), '::' )
            info%by_value = &
                index( declarations(line_index)(1:kcolon), 'value' ) > 0
            if ( info%by_value ) then
                exit
            endif
        else
            exit
        endif
    enddo

end subroutine get_by_value


! get_bind_c --
!     Determine if the variable has the bind(c) attribute
!
! Arguments:
!     declarations      List of lines containing the relevant declarations
!     info              Information about the variable
!
subroutine get_bind_c( declarations, info )

    character(len=*), dimension(:), intent(in) :: declarations
    type(var_info), intent(inout)              :: info

    character(len=max_length)                  :: varname

    integer           :: start
    integer           :: kcolon
    integer           :: line_index
    integer           :: decoration_index

    varname    = info%name
    start      = 1
    do
        call find_varname( declarations, varname, start, line_index, decoration_index )
        if ( line_index > 0 ) then
            kcolon             = index( declarations(line_index), '::' )
            info%bind_c = &
                index( declarations(line_index)(1:kcolon), 'bind(' ) > 0
            if ( info%bind_c ) then
                exit
            endif
        else
            exit
        endif
    enddo

end subroutine get_bind_c


! number_dimensions --
!     Extract the number of dimensions of a variable from a set of
!     declarations
!
! Arguments:
!     declarations      List of lines containing the relevant declarations
!     varname           Name of the variable
!
! Returns:
!     Number of dimensions
!
integer function number_dimensions( declarations, varname )

    character(len=*), dimension(:), intent(in) :: declarations
    character(len=*), intent(in)               :: varname

    integer           :: i
    integer           :: idx
    integer           :: nextpos
    integer           :: start
    integer           :: k
    integer           :: k1
    integer           :: k2
    character(len=1)  :: c
    character(len=len(declarations)) :: dim_string

    idx   = -1

    !
    ! TODO: in separate function
    !
find_var: &
    do i = 1,size(declarations)

        start =  1
        k = index( declarations(i)(start:), ' ' // trim(varname) )

        do while ( k > 0 )
            nextpos = start+k+len_trim(varname)
            c = declarations(i)(nextpos:nextpos)
            if ( c == '(' .or. c == '*' .or. c == ',' .or. c == ' ' ) then
                if ( index( declarations(i), 'dimension' ) > 0 ) then
                    idx = i
                    exit find_var
                else
                    if ( c == '(' ) then
                        idx = i
                        exit find_var
                    endif
                endif
                exit ! Inner loop only
            else
                start = nextpos
                k = index( declarations(i)(start:), ' ' // trim(varname) )
            endif
        enddo
    enddo &
find_var

    ! Is there a dimension for this variable?
    !
    if ( idx == -1 ) then
        number_dimensions = 0
    else
        if ( c == '(' ) then
            k = index( declarations(idx)(nextpos:), ')' )
            dim_string = declarations(idx)(nextpos:nextpos+k-1)
        else
            k1 = index( declarations(idx), 'dimension' )
            k2 = index( declarations(idx)(k1:), ')' )
            dim_string = declarations(idx)(k1+9:k1+k2-1)
        endif

        number_dimensions = 1
        do i = 1,len_trim(dim_string)
            if ( dim_string(i:i) == ',' ) then
                number_dimensions = number_dimensions + 1
            endif
        enddo
    endif
end function number_dimensions


! get_varnames --
!     Extract the variable names from a declaration
!
! Arguments:
!     line              Line containing subroutine/function argument list
!     name              Array of argument names
!
! Returns:
!     (Via the "name" array) list of individual names
!
! Note:
!     It is up to the caller to free the list of arguments, when done
!
subroutine get_varnames( line, name )

    character(len=*)                        :: line
    character(len=*), dimension(:), pointer :: name
    integer :: idx
    integer :: i

    idx = index( line, '::' ) ! Guaranteed to exist!

    call get_arguments( 'subroutine (' // line(idx+2:) // ')', name )

    do i = 1,size(name)
        idx = index( name(i), '*' )
        if ( idx > 0 ) then
            name(i)(idx:) = ' '
        endif
        idx = index( name(i), '(' )
        if ( idx > 0 ) then
            name(i)(idx:) = ' '
        endif
    enddo
end subroutine get_varnames


! get_arguments --
!     Extract the arguments from a subroutine's or function's argument list
!
! Arguments:
!     line              Line containing subroutine/function argument list
!     name              Array of argument names
!
! Returns:
!     (Via the "name" array) list of individual arguments
!
! Note:
!     It is up to the caller to free the list of arguments, when done
!
subroutine get_arguments( line, name )
    character(len=*)                        :: line
    character(len=*), dimension(:), pointer :: name

    integer          :: i
    integer          :: beginc
    integer          :: endc
    integer          :: count
    integer          :: ierr
    integer          :: keywordc
    character(len=1) :: c

    !
    ! Count the arguments first
    !
    keywordc = index( line, 'subroutine' )
    if ( keywordc <= 0 ) then
        keywordc = index( line, 'function' )
    endif
    if ( keywordc > 0 ) then
        beginc = index( line(keywordc:), '(' )
        endc   = index( line(keywordc:), ')', .true. )
    else
        beginc = -1
        endc   = -1
    endif

    if ( beginc < 1 .or. endc < beginc ) then
        allocate( name(0) )
    else
        beginc = keywordc + beginc - 1
        endc   = keywordc + endc   - 1
        count = 1
        do
            read( line(beginc+1:endc-1), *, iostat = ierr ) ( c ,i = 1,count )
            if ( ierr /= 0 ) then
                count = count - 1
                exit
            else
                count = count + 1
            endif
        enddo

        allocate( name(count) )

        read( line(beginc+1:endc-1), *, iostat = ierr ) ( name(i) ,i = 1,count )
    endif

end subroutine get_arguments


end module variables


! test program
!     Test the routines
!
program test_vars
    use variables

    character(len=20), dimension(:), pointer :: name
    character(len=40), dimension(7) :: declarations = &
           !1234567890123456789012345678901234567890
        (/ 'integer,dimension(:)::iarray,ivars      ', &
           'dimension::x1(10),x(20)                 ', &
           'integer,pointer::w                      ', &
           'character(len=5)::y                     ', &
           'character*100::z,y2*20                  ', &
           'character(len=*),intent(in)::abc        ', &
           'dimension::abc(5|4|3)                   '  /)

    type(var_info), dimension(:), pointer :: vars

    integer :: i
    integer :: n

    call init_variables

    write(*,*) 'Declarations:'
    write(*,'(1x,a)') declarations

    call convert_declarations( declarations, vars )

    do i = 1,size(vars)
        write(*,'(4a20)') vars(i)%name, vars(i)%type_name, vars(i)%dimensions
    enddo
!   do i = 1,size(varname)
!       write(*,'(1x,5a)') 'Type of "', varname(i),'": ', &
!           fortran_type( declarations, varname(i) )
!   enddo
!   do i = 1,size(varname)
!       n = number_dimensions( declarations, varname(i) )
!       write(*,'(1x,3a,i3)') 'Dimension of "', varname(i),'": ', n
!   enddo

    !
    ! Watch it: type(x) function f(...)
    ! Watch it: function f(...) result(r)
    !
!   call get_arguments( 'subroutine subr(x,y,array,z)', name )
!
!   write(*,*) 'Arguments:'
!   do i = 1,size(name)
!       write(*,'(1x,a)' ) name(i)
!   enddo
!
!   deallocate( name )
!   call get_arguments( 'type(xx) function func(x,y,array,z)', name )
!
!   write(*,*) 'Arguments:'
!   do i = 1,size(name)
!       write(*,'(1x,a)' ) name(i)
!   enddo
!   write(*,*) 'Pointers:'
!   write(*,* ) 'w: ', is_pointer( declarations, 'w' )
!   write(*,* ) 'x: ', is_pointer( declarations, 'x' )
!   write(*,*) 'Character lengths:'
!   write(*,* ) 'y: ', char_length( declarations, 'y' )
!   write(*,* ) 'z: ', char_length( declarations, 'z' )
end program
