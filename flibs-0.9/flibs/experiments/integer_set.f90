! integer_set.f90
!     Experiment with an unusual programming style
!
module integer_sets
    implicit none

    type INTEGER_OPERAND
        integer                        :: operation
        integer                        :: value
        type(INTEGER_OPERAND), pointer :: first
        type(INTEGER_OPERAND), pointer :: second
    end type INTEGER_OPERAND

    type INTEGER_RELATION
        integer                        :: relation
        logical                        :: value
        type(INTEGER_OPERAND), pointer :: first
        type(INTEGER_OPERAND), pointer :: second
    end type INTEGER_RELATION

    type INTEGER_SET
        type(INTEGER_RELATION)         :: relation
    end type INTEGER_SET

    type(INTEGER_OPERAND)              :: x
    type(INTEGER_OPERAND)              :: y

    interface assignment(=)
        module procedure integer_set_value
    end interface

    interface operator(+)
        module procedure integer_add
    end interface

    interface operator(.eq.)
        module procedure integer_equal
        module procedure integer_equal_v
    end interface

contains

subroutine integer_set_value( x, value )
    type(INTEGER_OPERAND), intent(out) :: x
    integer, intent(in)                :: value

    x%operation = 0
    x%value     = value
    x%first     => null()
    x%second    => null()
end subroutine integer_set_value

function integer_add( x, y ) result(add)
    type(INTEGER_OPERAND), intent(in), target  :: x
    type(INTEGER_OPERAND), intent(in), target  :: y
    type(INTEGER_OPERAND), pointer             :: add

    allocate( add )

    add%operation =  1
    add%first     => x
    add%second    => y
end function integer_add

function integer_equal( x, y ) result(equal)
    type(INTEGER_OPERAND), intent(in), target   :: x
    type(INTEGER_OPERAND), intent(in), target   :: y
    type(INTEGER_RELATION), pointer             :: equal

    allocate( equal )

    equal%relation =  1
    equal%first    => x
    equal%second   => y
end function integer_equal

function integer_equal_v( x, y ) result(equal)
    type(INTEGER_OPERAND), intent(in), target   :: x
    integer, intent(in)                         :: y
    type(INTEGER_RELATION), pointer             :: equal

    type(INTEGER_OPERAND), pointer  :: yy

    allocate( equal )
    allocate( yy    )

    yy = y

    equal%relation =  1
    equal%first    => x
    equal%second   => yy
end function integer_equal_v

recursive subroutine integer_eval( x )
    type(INTEGER_OPERAND)   :: x

    if ( associated( x%first  ) ) call integer_eval( x%first  )
    if ( associated( x%second ) ) call integer_eval( x%second )

    select case( x%operation )
    case ( 0 )
        ! Nothing to be done
    case ( 1 )
        x%value = x%first%value + x%second%value
    case default
    end select

end subroutine integer_eval

function integer_relation_eval( relation ) result(value)
    type(INTEGER_RELATION)  :: relation
    logical                 :: value

    call integer_eval( relation%first  )
    call integer_eval( relation%second )

    select case( relation%relation )
    case ( 1 )
        value = relation%first%value == relation%second%value
    case default
        ! TODO
    end select

end function integer_relation_eval

end module integer_sets

program test_sets
    use integer_sets

    type(INTEGER_RELATION), pointer :: relation

    relation => x + y == 0
!   relation => integer_equal_v( integer_add(x,y), 0 )

    x = 1
    y = -1
    write(*,*) 'x, y: 1, -1 ', integer_relation_eval( relation )

    x = 2
    y = -1
    write(*,*) 'x, y: 2, -1 ', integer_relation_eval( relation )

end program
