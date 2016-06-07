! iso_c_wrap.f90 --
!     Mimick the IOS_C_BINDING module that is standard with Fortran 2003
!     This way we can easily prepare for Fortran 2003
!
module iso_c_binding

    implicit none

    integer, parameter :: c_int    = kind(1)
    integer, parameter :: c_long   = kind(1)    ! Requires adjustment for 64-bits?
    integer, parameter :: c_float  = kind(1.0)
    integer, parameter :: c_double = kind(1.0d0)
    type c_ptr
        integer, dimension(2) :: ptr  ! Two integers to cater for 64-bits platforms
    end type c_ptr

end module
