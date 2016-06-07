! Extra checks on Fortran 95 compiler:
!
! character(len=lenarg) - in subroutines, lenarg is an argument
!
! allocate( array(0) ) - is it allocated or not?
!
! allocatable components in derived type
!
program chkf95
    integer, dimension(:), allocatable, target :: array
    integer, dimension(:), pointer             :: parray
    type pointer
        integer, dimension(:), pointer :: parray
    end type

    type(pointer) :: p

    allocate( array(0) )

    parray => array
    p%parray => array

    if ( allocated(array) ) then
        write(*,*) 'Array is allocated - size: ', size(array)
    else
        write(*,*) 'Array is reported as NOT allocated'
    endif

    if ( associated(parray) ) then
        write(*,*) 'Pointer to array is associated - size: ', size(parray)
    else
        write(*,*) 'Pointer to array is reported as NOT associated'
    endif

    if ( associated(p%parray) ) then
        write(*,*) '(Component) Pointer to array is associated - size: ', size(p%parray)
    else
        write(*,*) '(Component) Pointer to array is reported as NOT associated'
    endif
end program
