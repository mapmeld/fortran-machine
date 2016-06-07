! test_vectors.f90 --
!     Program illustrating the use of "vectors"
!
!     $Id: test_vectors.f90,v 1.2 2006/03/26 19:03:53 arjenmarkus Exp $
!
! Convenience module, holding all the data types
!
module DATA_MODULE

type REAL_DATA
    real :: value
end type REAL_DATA

type(REAL_DATA), parameter :: empty_data = REAL_DATA(-999.0)

end module DATA_MODULE

!
! Module defining the vector type
!
module REAL_VECTORS
    use DATA_MODULE, VECTOR_DATA => REAL_DATA, &
                     empty_vector_data => empty_data

    include "vectors.f90"
end module REAL_VECTORS

program test_vectors
    use REAL_VECTORS

    implicit none

    type(VECTOR)      :: vector1
    type(VECTOR_DATA) :: data
    integer           :: i

    !
    ! Create a vector and set some elements
    !
    call vector_create( vector1, 2 )

    do i = 1,10
       data%value = i
       call vector_put( vector1, i, data )
    enddo

    write(*,*) 'Contents of the vector: numbers 1 to 10'

    do i = 1,vector_size(vector1)
        write(*,*) i, vector_at(vector1,i)
    enddo

    !
    ! Insert an element in the middle
    !
    call vector_insert_empty( vector1, 5, 3 )
    data%value = 3.1415926
    call vector_put( vector1, 5, data )

    write(*,*) 'Contents of the vector: 1 to 4, pi, two empty, 5 to 10'

    do i = 1,vector_size(vector1)
        write(*,*) i, vector_at(vector1,i)
    enddo

    !
    ! Append an element at the end
    !
    data%value = exp(1.0)
    call vector_append( vector1, data )

    write(*,*) 'Contents of the vector: plus e'

    do i = 1,vector_size(vector1)
        write(*,*) i, vector_at(vector1,i)
    enddo

    !
    ! Delete the two empty entries
    !
    call vector_delete_elements( vector1, 6, 2 )

    write(*,*) 'Contents of the vector: minus the empty ones'

    do i = 1,vector_size(vector1)
        write(*,*) i, vector_at(vector1,i)
    enddo

end program
