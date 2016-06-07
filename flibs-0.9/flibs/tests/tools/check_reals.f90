! check_reals.f90 --
!     Module to check that reals are initialised
!
!     Note:
!     The routines can not be elemental, due to the pointer to "check"
!     This means that we need to add separate versions for arrays
!
module check_reals
    type checked_real
        integer, pointer :: check => null()
        real             :: value
    endtype

    integer, target, private :: check

    interface assignment(=)
        module procedure set_value_checked
        module procedure set_value_real
        module procedure set_real_checked
    end interface

    interface operator(+)
        module procedure add_checked
    end interface
contains

subroutine set_real_checked( x, y )
    real, intent(out)               :: x
    type(checked_real), intent(in)  :: y

    ! Note: because of "elemental" we can not check the state of y

    x = y%value
end subroutine set_real_checked

subroutine set_value_checked( x, y )
    type(checked_real), intent(out) :: x
    type(checked_real), intent(in)  :: y

    if ( .not. associated(x%check) ) then
        x%check => check
    endif
    if ( .not. associated(y%check) ) then
        x%check = x%check + 1
    endif

    x%value = y%value
end subroutine set_value_checked

subroutine set_value_real( x, y )
    type(checked_real), intent(out) :: x
    real, intent(in)                :: y

    if ( .not. associated(x%check) ) then
        x%check => check
    endif

    x%value = y ! No check possible
end subroutine set_value_real

type(checked_real) function add_checked( x, y )
    type(checked_real), intent(in) :: x, y

    add_checked%check => check
    add_checked%value =  x%value + y%value

    if ( .not. associated(x%check) ) then
        add_checked%check = add_checked%check + 1
    endif

    if ( .not. associated(y%check) ) then
        add_checked%check = add_checked%check + 1
    endif

end function add_checked

subroutine check_assignment( lineno, filename )
    integer :: lineno
    character(len=*) :: filename

    if ( check /= 0 ) then
        write(*,*) 'One or more uninitialised variables used at line ', &
            lineno, ' in file ', trim(filename)
    endif
    check = 0
end subroutine check_assignment

end module check_reals
