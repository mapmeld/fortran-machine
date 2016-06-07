! test_sets.f90 --
!     Test program for (unordered) sets
!
!     Note:
!     The module "MYDATA_MODULE" defines
!     the appropriate data type for the
!     data in the sets, the module
!     MYDATA_SET_STRUCTS is meant to prepare
!     the underlying data structure for the
!     sets.
!
!     $Id: test_sets.f90,v 1.2 2006/03/26 19:03:53 arjenmarkus Exp $
!
module MYDATA_MODULE

!
! The data that will be stored in the sets
!
type MYDATA
    integer :: value
end type MYDATA

!
! As derived types are compared, we need to define
! how to compare them
!
interface operator(.eq.)
    module procedure mydata_isequal
end interface

contains
logical function mydata_isequal( v1, v2 )
    type(MYDATA), intent(in) :: v1
    type(MYDATA), intent(in) :: v2

    mydata_isequal = v1%value .eq. v2%value
end function mydata_isequal

end module MYDATA_MODULE

module MYDATA_SET_STRUCTS
    use MYDATA_MODULE, SET_DATA => MYDATA

    include "data_for_sets.f90"
end module MYDATA_SET_STRUCTS

module MYDATA_SETS
    use MYDATA_SET_STRUCTS

    include "sets.f90"
end module MYDATA_SETS

program test_sets
    use MYDATA_SETS

    implicit none

    type(SET)      :: dataset
    type(SET)      :: dataset2
    type(SET)      :: result
    type(SET_DATA) :: data
    integer        :: i
    integer        :: size


    !
    ! Create a small set with the numbers 1 to 10
    !
    call set_create( dataset )

    do i = 1,10
        data%value = i
        call set_add( dataset, data )
    enddo

    !
    ! Add a redundant element
    !
    data%value = 5
    call set_add( dataset, data )

    !
    ! Check the size (must be 10)
    !
    size = set_size( dataset )
    if ( size /= 10 ) then
        write(*,*) 'Error: Size should be 10, is:', size
    endif

    !
    ! Check that the elements 1, 2, 3 are there,
    ! that 11, 12 and 13 are not
    !
    do i = 1,3
        data%value = i
        if ( .not. (data .elementof. dataset) ) then
            write(*,*) 'Error: Element ', i, 'is missing'
        endif
    enddo
    do i = 11,13
        data%value = i
        if ( data .elementof. dataset ) then
            write(*,*) 'Error: Element ', i, 'is present'
        endif
    enddo

    !
    ! Create another set that partly overlaps
    !
    call set_create( dataset2 )

    do i = 5,25
        data%value = i
        call set_add( dataset2, data )
    enddo

    !
    ! Now some simple set operations ...
    !
    result = dataset .union. dataset2

    do i = 1,25
        data%value = i
        if ( .not. (data .elementof. result) ) then
            write(*,*) 'Error: Element ', i, 'is missing from union'
        endif
    enddo

    call set_destroy( result )

    !
    ! Intersection ...
    !
    result = dataset .intersect. dataset2

    do i = 1,4
        data%value = i
        if ( data .elementof. result ) then
            write(*,*) 'Error: Element ', i, 'is present in intersection'
        endif
    enddo

    do i = 5,10
        data%value = i
        if ( .not. (data .elementof. result) ) then
            write(*,*) 'Error: Element ', i, 'is missing from intersection'
        endif
    enddo

    call set_destroy( result )

    !
    ! Exclusion ...
    !
    result = dataset .exclude. dataset2

    do i = 1,4
        data%value = i
        if ( .not. (data .elementof. result) ) then
            write(*,*) 'Error: Element ', i, 'is missing from exclusion'
        endif
    enddo

    do i = 5,10
        data%value = i
        if ( data .elementof. result ) then
            write(*,*) 'Error: Element ', i, 'is present in exclusion'
        endif
    enddo

    !
    ! Deletion ...
    !
    data%value = 3
    call set_delete_element( dataset, data )
    if ( data .elementof. dataset ) then
        write(*,*) 'Error: Element ', i, 'is still present after deletion'
    endif
    if ( set_size(dataset) /= 9 ) then
        write(*,*) 'Error: Size should 9, is ', set_size(dataset)
    endif

    write(*,*) 'If all is well, no error messages printed'
end program
