# chktype.tcl --
#     An attempt at literate programming
#     See the generated file for the motivation
#
proc comment {text} {
    global outfile
    puts $outfile "! [join [split $text \n] "\n !"]"
}
proc code {text} {
    global outfile
    puts $outfile $text
}
# Hm, gencode is superfluous probably
proc gencode {name text} {
    global outfile
    puts $outfile "[string map [list NAME $name] $text]"
}

# main --
#
set outfile [open "chktype.f90" w]

comment {\
chktype.f90 --

    The module chktype is meant to have the compiler check
    as much as possible if the terms in an expression all
    have the same precision. This can be used to find out
    if there are expressions like "2.1*x" with x a double
    precision value or "2.0*i/9" with i an integer.

    Such statements cause serious problems:
    - the constant 2.1 in the first example is used as
      single precision whereas the expression as a whole
      will be of double precision. The value is, however,
      _not_ the same as 2.1d0*x.
    - Evaluating 2.0*i/9 may result in a wrong answer:
      set i to 1. If i/9 is computed first, then the result
      is 0, not 2.0/9= 0.2222...

    The module is not intended for use in an actual program,
    it is merely meant for checking at run time
}
code {
module chktype
    type real_
        private
        real :: v
    endtype
    type double_
        private
        real(kind=kind(1.0d0)) :: v
    endtype
    type integer_
        private
        integer :: v
    endtype
}

foreach {op name} {+ add - sub * mult / div ** expon > gt < lt >= ge <= le
                   == eq /= ne} {
    gencode $op "
    interface operator(NAME)
        module procedure ${name}_real
        module procedure ${name}_double
        module procedure ${name}_integer
        module procedure ${name}_real_tc
        module procedure ${name}_double_tc
        module procedure ${name}_integer_tc
        module procedure ${name}_real_ct
        module procedure ${name}_double_ct
        module procedure ${name}_integer_ct
    end interface"
}
code {
    interface assignment(=)
        module procedure assign_real
        module procedure assign_double
        module procedure assign_integer
        module procedure assign_real_const
        module procedure assign_double_const
        module procedure assign_integer_const
    end interface
}
code {
contains
}

array set T {real real double "real(kind=kind(1.0d0))" integer integer}

foreach type {real double integer} {
    code "
    elemental subroutine assign_${type}(x,y)
        type(${type}_), intent(in)  :: y
        type(${type}_), intent(out) :: x
        x%v = y%v
    end subroutine"
    code "
    elemental subroutine assign_${type}_const(x,y)
        $T($type), intent(in)       :: y
        type(${type}_), intent(out) :: x
        x%v = y
    end subroutine"
}
foreach {op name} {+ add - sub * mult / div ** expon } {
    foreach type {real double integer} {
        code "
    elemental type(${type}_) function ${name}_${type}(x,y)
        type(${type}_), intent(in) :: x,y
        ${name}_${type}%v = x%v $op y%v
    end function
    elemental type(${type}_) function ${name}_${type}_tc(x,y)
        type(${type}_), intent(in) :: x
        $T($type), intent(in)      :: y
        ${name}_${type}_tc%v = x%v $op y
    end function
    elemental type(${type}_) function ${name}_${type}_ct(x,y)
        $T($type), intent(in)      :: x
        type(${type}_), intent(in) :: y
        ${name}_${type}_ct%v = x $op y%v
    end function"
    }
}
foreach {op name} {> gt < lt >= ge <= le == eq /= ne} {
    foreach type {real double integer} {
        code "
    elemental logical function ${name}_${type}(x,y)
        type(${type}_), intent(in) :: x,y
        ${name}_${type} = x%v $op y%v
    end function
    elemental logical function ${name}_${type}_tc(x,y)
        type(${type}_), intent(in) :: x
        $T($type), intent(in)      :: y
        ${name}_${type}_tc = x%v $op y
    end function
    elemental logical function ${name}_${type}_ct(x,y)
        $T($type), intent(in)      :: x
        type(${type}_), intent(in) :: y
        ${name}_${type}_ct = x $op y%v
    end function"
    }
}

# TODO: sin, cos, ...
# TODO: max, min
# Do we want minval and friends as well?
# Also: huge(), tiny() ...?


code {
end module chktype}

# Add a small test program
code {
program aha
    use chktype

    type(real_) :: x
    type(real_) :: y

    y = 1.0d0
    x = 11 * y
end program
}
close $outfile
