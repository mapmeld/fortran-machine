! interval_simple.f90 --
!     Straightforward implementation of interval arithmetic:
!     no dependency analysis
!
!     TODO:
!     Make sure rounding has the right effect!
!
!     TODO:
!     Degenerate intervals, arising from division by zero
!
!     TODO:
!     Make find_root more robust!
!
!     $Id: interval_simple.f90,v 1.3 2008/10/26 12:16:43 arjenmarkus Exp $
!
module interval_arithmetic
    use select_precision
    implicit none

    private
    public   :: interval, find_root
    public   :: intval, mid, operator(+), operator(-), operator(*), operator(/)
    public   :: operator(.pgt.), operator(.cgt.), operator(.pge.), operator(.cge.)
    public   :: operator(.plt.), operator(.clt.), operator(.ple.), operator(.cle.)
    public   :: abs, exp, log, log10, cos, sin, tan, acos, asin, atan, sinh, cosh, tanh, sqrt
   !public   :: min, max

    real(kind=wp), save :: pi = 0.0

    type INTERVAL
        real(kind=wp) :: lower
        real(kind=wp) :: upper
    end type INTERVAL

    interface operator(+)
        module procedure add
        module procedure add_r_i
        module procedure add_i_r
    end interface

    interface operator(-)
        module procedure usub
        module procedure sub
        module procedure sub_r_i
        module procedure sub_i_r
    end interface

    interface operator(*)
        module procedure mult
        module procedure mult_r_i
        module procedure mult_i_r
    end interface

    interface operator(/)
        module procedure div
        module procedure div_r_i
        module procedure div_i_r
    end interface

    interface abs
        module procedure abs_i
    end interface

    interface exp
        module procedure exp_i
    end interface

    interface log
        module procedure log_i
    end interface

    interface log10
        module procedure log10_i
    end interface

    interface sqrt
        module procedure sqrt_i
    end interface

    interface sin
        module procedure sin_i
    end interface

    interface cos
        module procedure cos_i
    end interface

    interface tan
        module procedure tan_i
    end interface

    interface asin
        module procedure asin_i
    end interface

    interface acos
        module procedure acos_i
    end interface

    interface atan
        module procedure atan_i
    end interface

    interface sinh
        module procedure sinh_i
    end interface

    interface cosh
        module procedure cosh_i
    end interface

    interface tanh
        module procedure tanh_i
    end interface

   ! interface min
   !     module procedure min_i
   !     module procedure min_r_i
   !     module procedure min_i_r
   ! end interface

   ! interface max
   !     module procedure max_i
   !     module procedure max_r_i
   !     module procedure max_i_r
   ! end interface

    !
    ! Relational operations:
    ! - probably and certainly greater than etc.
    !
    interface operator(.pgt.)
        module procedure pgt
    end interface
    interface operator(.cgt.)
        module procedure cgt
    end interface
    interface operator(.pge.)
        module procedure pge
    end interface
    interface operator(.cge.)
        module procedure cge
    end interface
    interface operator(.plt.)
        module procedure plt
    end interface
    interface operator(.clt.)
        module procedure clt
    end interface
    interface operator(.ple.)
        module procedure ple
    end interface
    interface operator(.cle.)
        module procedure cle
    end interface


contains

! intval --
!     Create an interval number
! Arguments:
!     xmin     Lower bound of the interval
!     xmax     Upper bound of the interval
! Result:
!     New interval number
!
function intval( xmin, xmax ) result(r)
    real(kind=wp), intent(in) :: xmin, xmax
    type(INTERVAL)            :: r

    r%lower = min( xmin, xmax )
    r%upper = max( xmin, xmax )
end function intval

! mid --
!     Return the midpoint of the interval
! Arguments:
!     x       Interval number
! Result:
!     Midpoint of the interval
!
function mid( x ) result(r)
    type(INTERVAL)            :: x
    real(kind=wp)             :: r

    r = 0.5 * (x%lower + x%upper)
end function mid

! add --
!     Add two interval numbers
! Arguments:
!     x        First interval number
!     y        Second interval number
! Result:
!     x+y
!
function add( x, y ) result(r)
    type(INTERVAL), intent(in) :: x, y
    type(INTERVAL)             :: r

    r%lower = x%lower + y%lower
    r%upper = x%upper + y%upper
end function add

function add_r_i( x, y ) result(r)
    real(kind=wp), intent(in)  :: x
    type(INTERVAL), intent(in) :: y
    type(INTERVAL)             :: r

    r%lower = x + y%lower
    r%upper = x + y%upper
end function add_r_i

function add_i_r( x, y ) result(r)
    type(INTERVAL), intent(in) :: x
    real(kind=wp), intent(in)  :: y
    type(INTERVAL)             :: r

    r%lower = x%lower  + y
    r%upper = x%upper  + y
end function add_i_r

! usub --
!     Unary minus
! Arguments:
!     x        Interval number
! Result:
!     -x
!
function usub( x ) result(r)
    type(INTERVAL), intent(in) :: x
    type(INTERVAL)             :: r

    r%lower = -x%upper
    r%upper = -x%lower
end function usub

! sub --
!     Subtract two interval numbers
! Arguments:
!     x        First interval number
!     y        Second interval number
! Result:
!     x-y
!
function sub( x, y ) result(r)
    type(INTERVAL), intent(in) :: x, y
    type(INTERVAL)             :: r

    r%lower = x%lower - y%upper
    r%upper = x%upper - y%lower
end function sub

function sub_r_i( x, y ) result(r)
    real(kind=wp), intent(in)  :: x
    type(INTERVAL), intent(in) :: y
    type(INTERVAL)             :: r

    r%lower = x - y%upper
    r%upper = x - y%lower
end function sub_r_i

function sub_i_r( x, y ) result(r)
    type(INTERVAL), intent(in) :: x
    real(kind=wp), intent(in)  :: y
    type(INTERVAL)             :: r

    r%lower = x%lower - y
    r%upper = x%upper - y
end function sub_i_r

! mult --
!     Multiply two interval numbers
! Arguments:
!     x        First interval number
!     y        Second interval number
! Result:
!     x*y
!
function mult( x, y ) result(r)
    type(INTERVAL), intent(in) :: x, y
    type(INTERVAL)             :: r

    real(kind=wp)              :: r1, r2, r3, r4

    r1      = x%lower * y%lower
    r2      = x%lower * y%upper
    r3      = x%upper * y%lower
    r4      = x%upper * y%upper
    r%lower = min( r1, r2, r3, r4 )
    r%upper = max( r1, r2, r3, r4 )
end function mult

function mult_r_i( x, y ) result(r)
    real(kind=wp), intent(in)  :: x
    type(INTERVAL), intent(in) :: y
    type(INTERVAL)             :: r

    r%lower = x * y%lower
    r%upper = x * y%upper
end function mult_r_i

function mult_i_r( x, y ) result(r)
    type(INTERVAL), intent(in) :: x
    real(kind=wp), intent(in)  :: y
    type(INTERVAL)             :: r

    r%lower = x%lower  * y
    r%upper = x%upper  * y
end function mult_i_r

! div --
!     Divide two interval numbers
! Arguments:
!     x        First interval number
!     y        Second interval number
! Result:
!     x/y
! Note:
!     If the y interval contains zero, this is a
!     division by zero.
!
function div( x, y ) result(r)
    type(INTERVAL), intent(in) :: x, y
    type(INTERVAL)             :: r

    real(kind=wp)              :: r1, r2, r3, r4

    if ( y%lower < 0.0 .and. y%upper > 0.0 ) then
        stop 'Division by zero'
    endif

    r1      = x%lower / y%lower
    r2      = x%lower / y%upper
    r3      = x%upper / y%lower
    r4      = x%upper / y%upper
    r%lower = min( r1, r2, r3, r4 )
    r%upper = max( r1, r2, r3, r4 )
end function div

function div_r_i( x, y ) result(r)
    real(kind=wp), intent(in)  :: x
    type(INTERVAL), intent(in) :: y
    type(INTERVAL)             :: r, xi

    xi = intval(x,x)
    r = div( xi, y )
end function div_r_i

function div_i_r( x, y ) result(r)
    type(INTERVAL), intent(in) :: x
    real(kind=wp), intent(in)  :: y
    type(INTERVAL)             :: r

    r%lower = x%lower / y
    r%upper = x%upper / y
end function div_i_r

! pgt and others --
!     Determine relation between two interval numbers
! Arguments:
!     x        First interval number
!     y        Second interval number
! Result:
!     x/y
! Note:
!     Probably greater: the interval of x extends further to the
!     positive side than that of y
!     Certainly greater: the complete interval of x lies right from that of y
!     Other relations are similar
!
logical function pgt( x, y )
    type(INTERVAL), intent(in) :: x, y

    pgt = x%upper > y%upper
end function pgt

logical function cgt( x, y )
    type(INTERVAL), intent(in) :: x, y

    cgt = x%lower > y%upper
end function cgt

logical function pge( x, y )
    type(INTERVAL), intent(in) :: x, y

    pge = x%upper >= y%upper
end function pge

logical function cge( x, y )
    type(INTERVAL), intent(in) :: x, y

    cge = x%lower >= y%upper
end function cge

logical function plt( x, y )
    type(INTERVAL), intent(in) :: x, y

    plt = x%lower < y%lower
end function plt

logical function clt( x, y )
    type(INTERVAL), intent(in) :: x, y

    clt = x%upper < y%lower
end function clt

logical function ple( x, y )
    type(INTERVAL), intent(in) :: x, y

    ple = x%lower <= y%lower
end function ple

logical function cle( x, y )
    type(INTERVAL), intent(in) :: x, y

    cle = x%upper <= y%lower
end function cle

! exp, log, ...
!     Interval versions of elementary mathematical functions
!     (monotone functions are easy)
! Arguments:
!     x        First interval number
! Result:
!     Interval containing the result
!
function abs_i( x ) result( r )
    type(INTERVAL), intent(in) :: x
    type(INTERVAL)             :: r

    if ( x%lower < 0.0 .and. x%upper > 0.0 ) then
        r%lower = 0.0
        r%upper = max( abs(x%lower), abs(x%upper) )
    else
        r%lower = min( abs(x%lower), abs(x%upper) )
        r%upper = max( abs(x%lower), abs(x%upper) )
    endif
end function abs_i

function exp_i( x ) result( r )
    type(INTERVAL), intent(in) :: x
    type(INTERVAL)             :: r

    r%lower = exp( x%lower )
    r%upper = exp( x%upper )
end function exp_i

function log_i( x ) result( r )
    type(INTERVAL), intent(in) :: x
    type(INTERVAL)             :: r

    r%lower = log( x%lower )
    r%upper = log( x%upper )
end function log_i

function log10_i( x ) result( r )
    type(INTERVAL), intent(in) :: x
    type(INTERVAL)             :: r

    r%lower = log10( x%lower )
    r%upper = log10( x%upper )
end function log10_i

function sinh_i( x ) result( r )
    type(INTERVAL), intent(in) :: x
    type(INTERVAL)             :: r

    r%lower = sinh( x%lower )
    r%upper = sinh( x%upper )
end function sinh_i

function cosh_i( x ) result( r )
    type(INTERVAL), intent(in) :: x
    type(INTERVAL)             :: r

    if ( x%lower < 0.0 .and. x%upper > 0.0 ) then
        r%lower = 1.0_wp
        r%upper = max( cosh(x%lower), cosh(x%upper) )
    else
        r%lower = cosh( x%lower )
        r%upper = cosh( x%upper )
    endif
end function cosh_i

function tanh_i( x ) result( r )
    type(INTERVAL), intent(in) :: x
    type(INTERVAL)             :: r

    r%lower = tanh( x%lower )
    r%upper = tanh( x%upper )
end function tanh_i

function sqrt_i( x ) result( r )
    type(INTERVAL), intent(in) :: x
    type(INTERVAL)             :: r

    r%lower = sqrt( x%lower )
    r%upper = sqrt( x%upper )
end function sqrt_i

function asin_i( x ) result( r )
    type(INTERVAL), intent(in) :: x
    type(INTERVAL)             :: r

    r%lower = asin( x%lower )
    r%upper = asin( x%upper )
end function asin_i

function acos_i( x ) result( r )
    type(INTERVAL), intent(in) :: x
    type(INTERVAL)             :: r

    r%lower = acos( x%upper ) ! acos() is monotone decreasing
    r%upper = acos( x%lower )
end function acos_i

function atan_i( x ) result( r )
    type(INTERVAL), intent(in) :: x
    type(INTERVAL)             :: r

    r%lower = atan( x%lower ) ! acos() is monotone decreasing
    r%upper = atan( x%upper )
end function atan_i

! sine, cosine and tangent are more involved
!
function sin_i( x ) result( r )
    type(INTERVAL), intent(in) :: x
    type(INTERVAL)             :: r

    integer                    :: k1
    integer                    :: k2

    if ( pi == 0.0 ) then
        pi = 4.0 * atan(1.0_wp)
    endif

    k1 = floor( (x%upper - 0.5*pi) / pi )
    k2 = floor( (x%lower - 0.5*pi) / pi )

    if ( k1 == k2 ) then
        r%lower = sin( x%lower )
        r%upper = sin( x%upper )
    else
        if ( abs(k1-k2) == 1 ) then
            if ( mod(k1,2) == 0 ) then
                r%lower = min( sin(x%lower), sin(x%upper) )
                r%upper = 1.0_wp
            else
                r%lower = -1.0_wp
                r%upper = max( sin(x%lower), sin(x%upper) )
            endif
        else
            r%lower = -1.0_wp
            r%upper =  1.0_wp
        endif
    endif
end function sin_i

function cos_i( x ) result( r )
    type(INTERVAL), intent(in) :: x
    type(INTERVAL)             :: r

    integer                    :: k1
    integer                    :: k2

    if ( pi == 0.0 ) then
        pi = 4.0 * atan(1.0_wp)
    endif

    k1 = floor( x%upper / pi )
    k2 = floor( x%lower / pi )

    if ( k1 == k2 ) then
        r%lower = cos( x%lower )
        r%upper = cos( x%upper )
    else
        if ( abs(k1-k2) == 1 ) then
            if ( mod(k1,2) == 0 ) then
                r%lower = min( cos(x%lower), cos(x%upper) )
                r%upper = 1.0_wp
            else
                r%lower = -1.0_wp
                r%upper = max( cos(x%lower), cos(x%upper) )
            endif
        else
            r%lower = -1.0_wp
            r%upper =  1.0_wp
        endif
    endif
end function cos_i

function tan_i( x ) result( r )
    type(INTERVAL), intent(in) :: x
    type(INTERVAL)             :: r

    integer                    :: k1
    integer                    :: k2

    if ( pi == 0.0 ) then
        pi = 4.0 * atan(1.0_wp)
    endif

    k1 = floor( (x%upper - 0.5*pi) / pi )
    k2 = floor( (x%lower - 0.5*pi) / pi )

    if ( k1 == k2 ) then
        r%lower = tan( x%lower )
        r%upper = tan( x%upper )
    else
        r%lower = -huge(1.0_wp)
        r%upper =  huge(1.0_wp)
    endif
end function tan_i

! min, max are involved as well
!
! TODO: min, max
!

! find_root
!     Find a root using Newton-Raphson
! Arguments:
!     f          Function whose root we want to find
!     fprime     First derivative of this function
!     xinit      Initial estimate (interval valued)
!     tolerance  Tolerance in finding root
!     root       Final estimate (interval valued)
!     found      Whether it was found or not
! Note:
!     If the iteration does not converge, we assume
!     that the iterate will grow indefinitely.
!
!     If you need a more general interface to the function,
!     consider using the implementation as a template
!
subroutine find_root( f, fprime, xinit, tolerance, root, found )
    interface
        function f(x)
            use select_precision
            real(kind=wp), intent(in) :: x
            real(kind=wp)             :: f
        end function
    end interface
    interface
        function fprime(x)
            use select_precision
            real(kind=wp), intent(in) :: x
            real(kind=wp)             :: fprime
        end function
    end interface

    type(INTERVAL), intent(in)  :: xinit
    type(INTERVAL), intent(out) :: root
    real(kind=wp)               :: tolerance
    logical                     :: found

    integer                     :: iter
    integer, parameter          :: maxiter = 1000
    real(kind=wp)               :: fvalue   ! Real valued!
    type(INTERVAL)              :: fpvalue
    real(kind=wp)               :: fleft
    real(kind=wp)               :: fright
    real(kind=wp)               :: fpleft
    real(kind=wp)               :: fpright

    found = .false.
    root  = xinit

    do iter = 1,maxiter

        fpleft  = fprime(root%lower)
        fpright = fprime(root%upper)
        fvalue  = f(mid(root))
        fpvalue = intval( fpleft, fpright )

        call find_root_iter( fvalue, fpvalue, root, tolerance, found )
        if ( found .or. abs(mid(root)) > huge(1.0_wp)/10.0 ) exit
    enddo

end subroutine find_root

! find_root_iter
!     Do one iteration in the Newton-Raphson method
! Arguments:
!     fvalue     Function value - real valued!
!     fpvalue    Function derivative - interval valued!
!     root       Current iterate
!     tolerance  Tolerance in finding root
!     found      Whether it was found or not
!
subroutine find_root_iter( fvalue, fpvalue, root, tolerance, found )
    real(wp), intent(in)           :: fvalue
    type(INTERVAL), intent(in)     :: fpvalue
    type(INTERVAL), intent(inout)  :: root
    real(wp), intent(in)           :: tolerance
    logical, intent(out)           :: found

    type(INTERVAL)                 :: newroot

    newroot = root - fvalue / fpvalue
    found   = abs( mid(root) - mid(root) ) < tolerance
    root    = newroot

end subroutine find_root_iter

end module interval_arithmetic
