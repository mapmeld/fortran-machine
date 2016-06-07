! decimals.f90 --
!     Module for decimal arithmetic:
!     - the result of something like 0.1+0.2 is
!       exactly 0.3 and not 0.299999998
!     - the numbers as implemented are fixed-point,
!       not floating-point, so the range is
!       somewhat limited.
!     - the precision of the numbers is
!       around 9 digits (exactly)
!
!     NOTE:
!     - No provision yet to deal with overflows!
!     - No rounding modes taken care of yet
!
!     TODO:
!     - Rounding modes
!     - string to decimal
!
!     $Id: decimal.f90,v 1.2 2006/03/26 19:03:53 arjenmarkus Exp $
!
module decimal_arithmetic
    implicit none

    !
    ! Use large integers ...
    ! (Perhaps double-precision reals would make it
    ! faster and simpler to implement?)
    !
    integer, parameter, private :: dec_kind = selected_int_kind( 10 )
    type DECIMAL
        integer(kind=dec_kind) :: int
        integer(kind=dec_kind) :: frac
    end type DECIMAL

    integer, parameter, private :: nodecimals = range( 1_dec_kind ) / 2
    integer, parameter, private :: supfrac = 10 ** nodecimals

    interface operator(-)
        module procedure dec_sub
        module procedure dec_umin
    end interface

    interface operator(+)
        module procedure dec_add
    end interface

    interface operator(*)
        module procedure dec_mult
    end interface

    interface operator(/)
        module procedure dec_div
    end interface

contains
subroutine print_decs
    write(*,*) 'Type:      ', dec_kind
    write(*,*) 'Precision: ', nodecimals
end subroutine

! real_to_dec
!     Convert a real to a decimal value
!
! Arguments:
!     value         Decimal number
!
type(DECIMAL) function real_to_dec( value )
    real, intent(in)          :: value

    real_to_dec%int  = int(value)
    real_to_dec%frac = int( (value-aint(value))*supfrac )
    if ( real_to_dec%frac >= supfrac ) then
        real_to_dec%frac = real_to_dec%frac - supfrac
        real_to_dec%int  = real_to_dec%int  + 1
    endif
    if ( real_to_dec%frac <= -supfrac ) then
        real_to_dec%frac = real_to_dec%frac + supfrac
        real_to_dec%int  = real_to_dec%int  - 1
    endif
end function real_to_dec

! dec_to_string
!     Convert a decimal value to a string
!
! Arguments:
!     value         Decimal number
!     decimals      Number of decimals (optional)
!
function dec_to_string( value, decimals ) result(string)
    type(DECIMAL), intent(in)     :: value
    integer, optional             :: decimals
    character(len=2*nodecimals+1) :: string

    integer                       :: dec
    character(len=20)             :: form

    if ( present(decimals) ) then
        dec = decimals
    else
        dec = nodecimals
    endif

    write( form, '(a,i2,a,i2,a,i2,a)' ) '(i', nodecimals, ',''.'',i', &
        nodecimals, '.', dec, ')'

    write( string, form ) value%int, value%frac
end function dec_to_string

! dec_abs
!     Return the absolute value of a decimal number
!
! Arguments:
!     value         Decimal number
!
type(DECIMAL) function dec_abs( value)
    type(DECIMAL), intent(in) :: value

    dec_abs%int  = abs(value%int)
    dec_abs%frac = abs(value%frac)
end function dec_abs

! dec_umin
!     Return the negative value of a decimal number
!     (unary minus)
!
! Arguments:
!     value         Decimal number
!
type(DECIMAL) function dec_umin( value)
    type(DECIMAL), intent(in) :: value

    dec_umin%int  = -value%int
    dec_umin%frac = -value%frac
end function dec_umin

! dec_add
!     Return the sum of two decimal numbers
!
! Arguments:
!     x             First decimal number
!     y             Second decimal number
!
type(DECIMAL) function dec_add( x, y )
    type(DECIMAL), intent(in) :: x
    type(DECIMAL), intent(in) :: y

    dec_add%int  = x%int  + y%int
    dec_add%frac = x%frac + y%frac

    !
    ! Normalise the number
    !
    if ( dec_add%frac > supfrac ) then
        dec_add%frac = dec_add%frac - supfrac
        dec_add%int  = dec_add%int  + 1
    elseif ( dec_add%frac < -supfrac ) then
        dec_add%frac = dec_add%frac + supfrac
        dec_add%int  = dec_add%int  - 1
    endif

    if ( dec_add%int > 0 .and. dec_add%frac < 0 ) then
        dec_add%frac = dec_add%frac + supfrac
        dec_add%int  = dec_add%int  + 1
    elseif ( dec_add%int < 0 .and. dec_add%frac > 0 ) then
        dec_add%frac = dec_add%frac - supfrac
        dec_add%int  = dec_add%int  - 1
    endif

end function dec_add

! dec_sub
!     Return the difference of two decimal numbers
!
! Arguments:
!     x             First decimal number
!     y             Second decimal number
!
type(DECIMAL) function dec_sub( x, y )
    type(DECIMAL), intent(in) :: x
    type(DECIMAL), intent(in) :: y

    dec_sub =  x + (-y)
end function dec_sub

! dec_mult
!     Return the product of two decimal numbers
!
! Arguments:
!     x             First decimal number
!     y             Second decimal number
!
type(DECIMAL) function dec_mult( x, y )
    type(DECIMAL), intent(in) :: x
    type(DECIMAL), intent(in) :: y

    integer(kind=dec_kind) :: i1, i2, f1, f2
    integer(kind=dec_kind) :: if1, if2, if3

    i1 = x%int
    f1 = x%frac
    i2 = y%int
    f2 = y%frac

    if1 = i1 * f2
    if2 = i2 * f1
    if1 = if1 + if2
    if3 = (if1 + if2) / supfrac
    if1 =  if1 - if3  * supfrac

    dec_mult%int  = i1*i2 + if3
    dec_mult%frac = if1   + (f1*f2)/supfrac

end function dec_mult

! dec_div
!     Return the quotient of two decimal numbers
!
! Arguments:
!     x             First decimal number
!     y             Second decimal number
!
type(DECIMAL) function dec_div( x, y )
    type(DECIMAL), intent(in) :: x
    type(DECIMAL), intent(in) :: y

    integer(kind=dec_kind) :: x2, y2
    integer(kind=dec_kind) :: i1, f1, f2, r
    integer                :: k

    x2 = x%int * supfrac + x%frac
    y2 = y%int * supfrac + y%frac

    i1 = x2 / y2
    r  = x2 - y2 * i1
    f2 = 0

    do k = 1,nodecimals
        f1 = r / y2
        f2 = f2 * 10 + f1
        r  = ( r - f1 * y2 ) * 10
    enddo

    dec_div%int  = i1
    dec_div%frac = f2

end function dec_div

end module decimal_arithmetic
