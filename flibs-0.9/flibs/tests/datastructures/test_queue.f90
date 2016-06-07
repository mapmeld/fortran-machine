! test_queue.f90 --
!     Test program for queues
!
!     $Id: test_queue.f90,v 1.2 2006/03/26 19:03:53 arjenmarkus Exp $
!
module MYDATA_MODULE

type MYDATA
    character(len=20) :: string
end type MYDATA

end module

module MYDATA_QUEUES
    use MYDATA_MODULE, QUEUE_DATA => MYDATA

    include "queues.f90"
end module MYDATA_QUEUES

program test_queue
    use MYDATA_QUEUES

    implicit none

    type(QUEUE_STRUCT), pointer :: queue
    type(QUEUE_DATA)            :: data
    logical                     :: success

    !
    ! Create a small queue
    !
    call queue_create( queue, 3 )

    !
    ! It should be empty now
    !
    write(*,*) 'Queue empty?', queue_empty(queue)
    !
    ! Fill it up
    !
    data%string = 'A'
    call queue_append_data( queue, data, success )
    data%string = 'B'
    call queue_append_data( queue, data, success )
    data%string = 'C'
    call queue_append_data( queue, data, success )

    !
    ! It should be full now
    !
    write(*,*) 'Queue full?', queue_full(queue)

    !
    ! Check the start
    !
    write(*,*) 'Start of queue:', queue_start_data(queue)
    !
    ! Retrieve the first element
    !
    write(*,*) 'Start of queue retrieved:', queue_retrieve_data(queue)
    write(*,*) 'Queue full? (should not be)', queue_full(queue)

    data%string = 'D'
    call queue_append_data( queue, data, success )
    write(*,*) 'Retrieve second element:', queue_retrieve_data(queue)

    data%string = 'E'
    call queue_append_data( queue, data, success )
    write(*,*) 'Retrieve third element:', queue_retrieve_data(queue)
    write(*,*) 'Retrieve fourth element:', queue_retrieve_data(queue)
    write(*,*) 'Retrieve fifth element:', queue_retrieve_data(queue)
    write(*,*) 'Now the queue is empty:', queue_empty(queue)

    !
    ! Destroy the queue
    !
    call queue_destroy( queue )

end program
