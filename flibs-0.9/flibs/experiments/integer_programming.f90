! integer_programming.f90
!     Use a simple integer programming problem as an
!     illustration of automated memory management
!
!     Note:
!     It should be not too difficult to generalise
!     this to N dimensions.
!
module integer_collections
    implicit none

    type INTEGER_SET
        logical                        :: from_function
        integer, dimension(:), pointer :: elements
    end type INTEGER_SET

    type INTEGER_PAIR_SET
        logical                        :: from_function
        integer, dimension(:), pointer :: x_elements
        integer, dimension(:), pointer :: y_elements
    end type INTEGER_PAIR_SET

    interface assignment(=)
        module procedure assign_set
        module procedure assign_pair_set
    end interface

contains

integer function elements(c)
    type(INTEGER_PAIR_SET)         :: c

    elements = size(c%x_elements)
end function elements

function x(c) result(xx)
    type(INTEGER_PAIR_SET)         :: c
    integer, dimension(:), pointer :: xx

    xx => c%x_elements
end function x

function y(c) result(yy)
    type(INTEGER_PAIR_SET)         :: c
    integer, dimension(:), pointer :: yy

    yy => c%y_elements
end function y

function getelem(c,idx) result(pair)
    type(INTEGER_PAIR_SET), intent(in) :: c
    integer, intent(in)                :: idx
    integer, dimension(2)              :: pair

    pair(1) = c%x_elements(idx)
    pair(2) = c%y_elements(idx)
end function getelem

function range(min,max) result(list)
    integer, intent(in)                :: min
    integer, intent(in)                :: max
    type(INTEGER_SET)                  :: list

    integer                            :: i

    allocate( list%elements(max-min+1) )

    list%from_function = .true.
    list%elements = (/ (i, i=min,max) /)
end function range

function select(c,mask) result(cnew)
    type(INTEGER_PAIR_SET), intent(in) :: c
    logical, dimension(:)              :: mask
    type(INTEGER_PAIR_SET)             :: cnew

    integer                            :: noelems

    noelems = count(mask)
    allocate( cnew%x_elements(noelems), cnew%y_elements(noelems) )

    cnew%from_function = .true.
    cnew%x_elements = pack( c%x_elements, mask )
    cnew%y_elements = pack( c%y_elements, mask )
end function select

function cartesian_product(s1,s2) result(cnew)
    type(INTEGER_SET), intent(in)      :: s1, s2
    type(INTEGER_PAIR_SET)             :: cnew

    integer                            :: noelems
    integer                            :: i
    integer                            :: j

    noelems = size(s1%elements) * size(s2%elements)
    allocate( cnew%x_elements(noelems), cnew%y_elements(noelems) )

    cnew%from_function = .true.

    cnew%x_elements    = (/ (s1%elements, i = 1,size(s2%elements) ) /)
    cnew%y_elements    = (/ ((s2%elements(j), i = 1,size(s1%elements) ), j=1,size(s2%elements) ) /)

end function cartesian_product

subroutine assign_set( setleft, setright )
    type(INTEGER_SET), intent(inout) :: setleft
    type(INTEGER_SET), intent(in)    :: setright

    if ( associated(setleft%elements) ) then
        deallocate( setleft%elements )
    endif
    if ( .not. setright%from_function ) then
        allocate( setleft%elements(size(setright%elements)) )
        setleft%elements = setright%elements
    else
        setleft%elements => setright%elements
    endif

end subroutine assign_set

subroutine assign_pair_set( pairleft, pairright )
    type(INTEGER_PAIR_SET), intent(inout) :: pairleft
    type(INTEGER_PAIR_SET), intent(in)    :: pairright


    if ( associated(pairleft%x_elements) ) then
        deallocate( pairleft%x_elements )
        deallocate( pairleft%y_elements )
    endif

    if ( .not. pairright%from_function ) then
        allocate( pairleft%x_elements(size(pairright%x_elements)) )
        allocate( pairleft%y_elements(size(pairright%y_elements)) )
        pairleft%x_elements = pairright%x_elements
        pairleft%y_elements = pairright%y_elements
    else
        pairleft%x_elements => pairright%x_elements
        pairleft%y_elements => pairright%y_elements
    endif
end subroutine assign_pair_set

end module integer_collections

program test_program
    use integer_collections

    type(INTEGER_SET)      :: xset
    type(INTEGER_SET)      :: yset
    type(INTEGER_PAIR_SET) :: c

    integer, dimension(1)  :: idx   ! Slightly nasty

    integer                :: i, j
    integer, dimension(3)  :: solution

!
! The problem:
! Find integer values for (x,y) such that 3x+4y is maximal
! subject to:
!   10x+3y <= 200
!    3x+7y <= 121
!    x, y >= 0
!
! Steps:
! First assume that 0 <= x <= 20 and 0 <= y <= 41
! (rough bounds derived from the conditions)
! Then construct a set of pairs (x,y) that fulfill these
! conditions.
!
    xset = range(0,20)                ! x values in the required range
    yset = range(0,41)                ! ditto for y

    c = cartesian_product(xset,yset)  ! create all pairs

!
! Select those pairs that fulfill the first two conditions
!
    write(*,*) 'Number of pairs: ', elements(c)

    c = select( c, 10*x(c)+3*y(c) <= 200 ) ; write(*,*) 'Number of pairs: ', elements(c)
    c = select( c,  3*x(c)+7*y(c) <= 121 ) ; write(*,*) 'Number of pairs: ', elements(c)

!
! So, now we have all (x,y) pairs that could possibly
! be candidates. The last step is to pick out the
! one that maximizes 3x-4y.
!
    idx = maxloc( 3*x(c) + 4*y(c) )

    write(*,*) 'Solution: ', getelem(c,idx(1))

!
! Here is a more classical, brute-force, solution to the problem to
!
    solution(1) = 0
    solution(2) = 0
    solution(3) = 0
    do j = 0,41
        do i = 0,20
            if ( 10*i + 3*j <= 200 .and. 3*i + 7*j <= 121 ) then
                if ( solution(3) < 3*i + 4*j ) then
                    solution(1) = i
                    solution(2) = j
                    solution(3) = 3*i + 4*j
                endif
            endif
        enddo
    enddo

    write(*,*) 'Solution: ',solution

end program
