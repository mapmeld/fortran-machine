! tuplespace.f90 --
!     Implementation of tuplespaces
!
!      *** UNDER CONSTRUCTION ***
!
module tuplespaces

    use ipc_file ! Or one of the others!

    implicit none

    type tuple_connection
        private
        type(ipc_comm)    :: comm
        character(len=40) :: space
    end type tuple_connection

    type tuple_elem
        private
        integer                                       :: elem_type = 0
        integer                                       :: ivalue    = 0
        logical                                       :: lvalue    = .false.
        real                                          :: rvalue    = 0.0
        real(kind=kind(1.0d0))                        :: dvalue    = 0.0d0
        character(len=80)                             :: svalue    = ''
        integer,                dimension(:), pointer :: iarray    => null()
        logical,                dimension(:), pointer :: larray    => null()
        real,                   dimension(:), pointer :: rarray    => null()
        real(kind=kind(1.0d0)), dimension(:), pointer :: darray    => null()
    end type tuple_elem

    type tuple_data
        type(tuple_elem), dimension(:), pointer :: elem
    end type tuple_data

    type(tuple_elem), parameter :: elem_any = tuple_elem(0,0,.false.,0.0,0.0d0,'',null(),null(),null(),null())
    type(tuple_elem), parameter :: elem_integer = tuple_elem(-1,0,.false.,0.0,0.0d0,'',null(),null(),null(),null())
    type(tuple_elem), parameter :: elem_logical = tuple_elem(-2,0,.false.,0.0,0.0d0,'',null(),null(),null(),null())
    type(tuple_elem), parameter :: elem_real = tuple_elem(-3,0,.false.,0.0,0.0d0,'',null(),null(),null(),null())
    type(tuple_elem), parameter :: elem_double = tuple_elem(-4,0,.false.,0.0,0.0d0,'',null(),null(),null(),null())
    type(tuple_elem), parameter :: elem_string = tuple_elem(-5,0,.false.,0.0,0.0d0,'',null(),null(),null(),null())
    type(tuple_elem), parameter :: elem_intarray = tuple_elem(-6,0,.false.,0.0,0.0d0,'',null(),null(),null(),null())
    type(tuple_elem), parameter :: elem_realarray = tuple_elem(-7,0,.false.,0.0,0.0d0,'',null(),null(),null(),null())
    type(tuple_elem), parameter :: elem_dblarray = tuple_elem(-8,0,.false.,0.0,0.0d0,'',null(),null(),null(),null())

    interface elem
        module procedure elem_create_integer
        module procedure elem_create_logical
        module procedure elem_create_real
        module procedure elem_create_double
        module procedure elem_create_string
    end interface

    interface tuple_out
        module procedure tuple_out_array
        module procedure tuple_out_tuple
    end interface

    interface assignment(=)
        module procedure tuple_assign
    end interface

    private :: tuple_read_in_tuple
    private :: tuple_read_elem
    private :: tuple_write_elem

contains

!!subroutine tuple_open(...)

! tuple_assign --
!     Short-hand for creating a tuple from an array of elements
!
! Arguments:
!     tuple      Tuple to be created
!     array      Array of tuple elements
!
! NOTE:
!     We should deep-copy the tuple elements as well!
!
subroutine tuple_assign( tuple, array )
    type(tuple_data), intent(inout)            :: tuple
    type(tuple_elem), dimension(:), intent(in) :: array

    if ( associated(tuple%elem) ) then
        deallocate( tuple%elem )
    endif
    allocate( tuple%elem(size(array)) )

    tuple%elem = array

end subroutine tuple_assign

! elem --
!     Short-hand constructor for tuple elements
!
type(tuple_elem) function elem_create_integer( value )
    integer, intent(in) :: value
    elem_create_integer%elem_type = 1
    elem_create_integer%ivalue    = value
end function elem_create_integer

type(tuple_elem) function elem_create_logical( value )
    logical, intent(in) :: value
    elem_create_logical%elem_type = 2
    elem_create_logical%lvalue    = value
end function elem_create_logical

type(tuple_elem) function elem_create_real( value )
    real, intent(in) :: value
    elem_create_real%elem_type = 3
    elem_create_real%rvalue    = value
end function elem_create_real

type(tuple_elem) function elem_create_double( value )
    real(kind=kind(1.0d0)), intent(in) :: value
    elem_create_double%elem_type = 4
    elem_create_double%dvalue    = value
end function elem_create_double

type(tuple_elem) function elem_create_string( value )
    character(len=*), intent(in) :: value
    elem_create_string%elem_type = 5
    elem_create_string%svalue    = value
end function elem_create_string

type(tuple_elem) function elem_create_integer_array( array )
    integer, intent(in), dimension(:), target :: array
    elem_create_integer_array%elem_type = 6
    !allocate( elem_integer_array%iarray(1:size(array)) )
    elem_create_integer_array%iarray => array
end function elem_create_integer_array

type(tuple_elem) function elem_create_logical_array( array )
    logical, intent(in), dimension(:), target :: array
    elem_create_logical_array%elem_type = 6
    !allocate( elem_logical_array%larray(1:size(array)) )
    elem_create_logical_array%larray  => array
end function elem_create_logical_array

type(tuple_elem) function elem_create_real_array( array )
    real, intent(in), dimension(:), target :: array
    elem_create_real_array%elem_type = 6
    !allocate( elem_real_array%larray(1:size(array)) )
    elem_create_real_array%rarray => array
end function elem_create_real_array

type(tuple_elem) function elem_create_double_array( array )
    real(kind=kind(1.0d0)), intent(in), dimension(:), target :: array
    elem_create_double_array%elem_type = 8
    !allocate( elem_double_array%darray(1:size(array)) )
    elem_create_double_array%darray => array
end function elem_create_double_array


! TODO: array of strings


! tuple_match --
!     See if a tuple matches a pattern
!
logical function tuple_match( pattern, tuple )
    type(tuple_data), intent(in) :: pattern
    type(tuple_data), intent(in) :: tuple

    integer :: i

    if ( size(pattern%elem) /= size(tuple%elem) ) then
        tuple_match = .false.
    else
        tuple_match = .true.
        do i = 1,size(pattern%elem)
            if ( pattern%elem(i)%elem_type == 0 ) then
                cycle
            endif
            if ( pattern%elem(i)%elem_type <= 0 ) then
                if ( abs(pattern%elem(i)%elem_type) == abs(tuple%elem(i)%elem_type) ) then
                    cycle
                else
                    tuple_match = .false.
                    exit
                endif
            else
                if ( pattern%elem(i)%elem_type /= tuple%elem(i)%elem_type ) then
                    tuple_match = .false.
                    exit
                endif
            endif

            !
            ! Match the actual values
            !
            select case ( pattern%elem(i)%elem_type )
                case( 1 )
                    tuple_match = pattern%elem(i)%ivalue == tuple%elem(i)%ivalue
                case( 2 )
                    tuple_match = pattern%elem(i)%lvalue .eqv. tuple%elem(i)%lvalue
                case( 3 )
                    tuple_match = pattern%elem(i)%rvalue == tuple%elem(i)%rvalue
                case( 4 )
                    tuple_match = pattern%elem(i)%dvalue == tuple%elem(i)%dvalue
                case( 5 )
                    tuple_match = pattern%elem(i)%svalue == tuple%elem(i)%svalue
                case( 6 )
                    if ( size(pattern%elem(i)%iarray) /= size(tuple%elem(i)%iarray) ) then
                        tuple_match = .false.
                        exit
                    endif
                    tuple_match = all( pattern%elem(i)%iarray == tuple%elem(i)%iarray )
                case( 7 )
                    if ( size(pattern%elem(i)%larray) /= size(tuple%elem(i)%larray) ) then
                        tuple_match = .false.
                        exit
                    endif
                    tuple_match = all( pattern%elem(i)%larray .eqv. tuple%elem(i)%larray )
                case( 8 )
                    if ( size(pattern%elem(i)%rarray) /= size(tuple%elem(i)%rarray) ) then
                        tuple_match = .false.
                        exit
                    endif
                    tuple_match = all( pattern%elem(i)%rarray == tuple%elem(i)%rarray )
                case( 9 )
                    if ( size(pattern%elem(i)%darray) /= size(tuple%elem(i)%darray) ) then
                        tuple_match = .false.
                        exit
                    endif
                    tuple_match = all( pattern%elem(i)%darray == tuple%elem(i)%darray )
            end select
        enddo
    endif

end function tuple_match

! tuple_empty --
!     Is the tuple empty or not?
!
! Arguments:
!     tuple            Tuple to be examined
!
logical function tuple_empty( tuple )
    type(tuple_data), intent(in) :: tuple

    tuple_empty = .not. associated(tuple%elem)
end function tuple_empty

! tuple_out --
!     Write a tuple into the tupleserver's database
!
! Arguments:
!     server           Connection information for the server
!     tuple            Tuple to be written (or array of tuple elements)
!
subroutine tuple_out_array( server, array )
    type(tuple_connection), intent(in)                 :: server
    type(tuple_elem), dimension(:), target, intent(in) :: array

    type(tuple_data)                                   :: tuple

    tuple%elem => array
    call tuple_out_tuple( server, tuple )
end subroutine tuple_out_array

subroutine tuple_out_tuple( server, tuple )
    type(tuple_connection), intent(in)     :: server
    type(tuple_data), intent(in)           :: tuple

    integer                                :: i

    call ipc_send_start( server%comm, server%space, "OUT", 0 )

    call ipc_send_data( server%comm, size(tuple%elem) )
    do i = 1,size(tuple%elem)
        call tuple_write_elem( server%comm, tuple%elem(i) )
    enddo

    call ipc_send_finish( server%comm )
end subroutine

! tuple_in --
!     Read a tuple from the tupleserver's database and leave it in
!
! Arguments:
!     server           Connection information for the server
!     pattern          Pattern that the tuple should match
!     tuple            Tuple returned by the server
!     wait             Wait for the right tuple or not
!
subroutine tuple_in_array( server, pattern_array, tuple, wait )
    type(tuple_connection), intent(in)                 :: server
    type(tuple_elem), dimension(:), target, intent(in) :: pattern_array
    type(tuple_data), intent(out)                      :: tuple
    logical, intent(in)                                :: wait

    type(tuple_data)                                   :: pattern

    pattern%elem => pattern_array
    call tuple_read_in_tuple( server, pattern, tuple, 'IN', wait )
end subroutine tuple_in_array

subroutine tuple_in_tuple( server, pattern, tuple, wait )
    type(tuple_connection), intent(in)     :: server
    type(tuple_data), intent(in)           :: pattern
    type(tuple_data), intent(out)          :: tuple
    logical, intent(in)                    :: wait

    call tuple_read_in_tuple( server, pattern, tuple, 'IN', wait )

end subroutine tuple_in_tuple

! tuple_read --
!     Read a tuple from the tupleserver's database and remove it from
!     the database
!
! Arguments:
!     server           Connection information for the server
!     pattern          Pattern that the tuple should match
!     tuple            Tuple returned by the server (may be empty)
!     wait             Wait for the right tuple or not
!
subroutine tuple_read_array( server, pattern_array, tuple, wait )
    type(tuple_connection), intent(in)                 :: server
    type(tuple_elem), dimension(:), intent(in), target :: pattern_array
    type(tuple_data), intent(out)                      :: tuple
    logical, intent(in)                                :: wait

    type(tuple_data)                       :: pattern

    pattern%elem => pattern_array
    call tuple_read_in_tuple( server, pattern, tuple, 'READ', wait )
end subroutine tuple_read_array

subroutine tuple_read_tuple( server, pattern, tuple, wait )
    type(tuple_connection), intent(in)     :: server
    type(tuple_data), intent(in)           :: pattern
    type(tuple_data), intent(out)          :: tuple
    logical, intent(in)                    :: wait

    call tuple_read_in_tuple( server, pattern, tuple, 'READ', wait )

end subroutine tuple_read_tuple

! tuple_read_in_tuple --
!     Read or input a tuple from the tupleserver's database
!
! Arguments:
!     server           Connection information for the server
!     pattern          Pattern that the tuple should match
!     tuple            Tuple returned by the server
!     operation        Keyword defining the operation
!     wait             Wait for the right tuple or not
!
subroutine tuple_read_in_tuple( server, pattern_array, tuple, operation, wait )
    type(tuple_connection), intent(in)     :: server
    type(tuple_data), intent(in)           :: pattern
    type(tuple_data), intent(out)          :: tuple
    character(len=*), intent(in)           :: operation
    logical, intent(in)                    :: wait

    character(len=40)                      :: tag
    integer                                :: id
    integer                                :: noelem

    call tuple_free( tuple )

    if ( wait ) then
        call ipc_send_start( server%comm, server%space, 'A' // operation, 0 )
    else
        call ipc_send_start( server%comm, server%space, operation, 0 )
    endif

    call ipc_send_data( server%comm, size(pattern%elem) )

    do i = 1,size(tuple%elem)
        call tuple_write_elem( server%comm, tuple%elem(i) )
    enddo

    call ipc_send_finish( server%comm )

    tag = ipc_any_tag
    id  = ipc_any_id

    do
        call ipc_receive_start( server%comm, server%space, tag, id )

        if ( tag /= 'READY' ) then
            if ( .not. wait ) then
                exit
            else
                cycle
            endif
        else
            call ipc_receive_data( server%comm, noelem )

            allocate( tuple%elem(noelem) )

            do i = 1,noelem
                call tuple_read_elem( server%comm, tuple%elem(i) )
            enddo

            call ipc_receive_finish( server%comm )

        endif
    enddo

end subroutine tuple_read_in_tuple

! tuple_write_elem --
!     Write a single element
!
! Arguments:
!     comm             Connection information
!     elem             Element to be written
!
subroutine tuple_write_elem( comm, elem )
    type(ipc_comm), intent(in)             :: comm
    type(tuple_elem), intent(in)           :: elem

    call ipc_send_data( comm, elem%elem_type )
    select case ( elem%elem_type )
        case( 1 )
            call ipc_send_data( comm, elem%ivalue )
        case( 2 )
            call ipc_send_data( comm, elem%lvalue )
        case( 3 )
            call ipc_send_data( comm, elem%rvalue )
        case( 4 )
            call ipc_send_data( comm, elem%dvalue )
        case( 5 )
            call ipc_send_data( comm, elem%svalue )
        case( 6 )
            call ipc_send_data( comm, size(elem%iarray) )
            call ipc_send_data( comm, elem%iarray )
        case( 7 )
            call ipc_send_data( comm, size(elem%larray) )
            call ipc_send_data( comm, elem%larray )
        case( 8 )
            call ipc_send_data( comm, size(elem%rarray) )
            call ipc_send_data( comm, elem%rarray )
        case( 9 )
            call ipc_send_data( comm, size(elem%darray) )
            call ipc_send_data( comm, elem%darray )
        case default
            ! Ignore!
    end select

end subroutine tuple_write_elem

! tuple_read_elem --
!     Read a single element
!
! Arguments:
!     comm             Connection information
!     elem             Element to be read
!
subroutine tuple_write_elem( comm, elem )
    type(ipc_comm), intent(in)             :: comm
    type(tuple_elem), intent(out)          :: elem

    integer                                :: nodata

    call ipc_receive_data( comm, elem%elem_type )
    select case ( elem%elem_type )
        case( 1 )
            call ipc_receive_data( comm, elem%ivalue )
        case( 2 )
            call ipc_receive_data( comm, elem%lvalue )
        case( 3 )
            call ipc_receive_data( comm, elem%rvalue )
        case( 4 )
            call ipc_receive_data( comm, elem%dvalue )
        case( 5 )
            call ipc_receive_data( comm, elem%svalue )
        case( 6 )
            call ipc_receive_data( comm, nodata )
            allocate( elem%iarray(nodata) )
            call ipc_receive_data( comm, elem%iarray )
        case( 7 )
            call ipc_receive_data( comm, nodata )
            allocate( elem%larray(nodata) )
            call ipc_receive_data( comm, elem%larray )
        case( 8 )
            call ipc_receive_data( comm, nodata )
            allocate( elem%rarray(nodata) )
            call ipc_receive_data( comm, elem%rarray )
        case( 9 )
            call ipc_receive_data( comm, nodata )
            allocate( elem%darray(nodata) )
            call ipc_receive_data( comm, elem%darray )
        case default
            ! Ignore!
    end select

end subroutine tuple_read_elem

end module

! test program
!     - Test the matching routine
!
program test_tuple
    use tuplespaces

    type(tuple_data)  :: pattern
    type(tuple_data)  :: tuple

    pattern = (/ elem_any, elem_integer, elem(1) /)
    tuple   = (/ elem(1.0), elem(1), elem(1) /)

    write(*,*) 'Tuple should match: ', tuple_match( pattern, tuple )


    tuple   = (/ elem(1.0), elem(1.0), elem(1) /)
                            ! Real!
    write(*,*) 'Tuple should NOT match: ', tuple_match( pattern, tuple )


    tuple   = (/ elem(1.0), elem(1), elem(1), elem(2) /)
                                               ! Too long
    write(*,*) 'Tuple should NOT match: ', tuple_match( pattern, tuple )


    pattern = (/ elem(1.0), elem_string, elem('abc') /)
    tuple   = (/ elem(1.0), elem('abc'), elem('abc') /)

    write(*,*) 'Tuple should match: ', tuple_match( pattern, tuple )

    deallocate( pattern%elem, tuple%elem )
end program test_tuple
