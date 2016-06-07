! queues.f90 --
!     Include file for defining queues with a fixed capacity
!
!     See the example/test program for the way to use this
!
!     Queues as implemented here are simply arrays where
!     data are inserted at the end and retrieved from the
!     top.
!
!     $Id: queues.f90,v 1.2 2006/03/26 19:03:53 arjenmarkus Exp $
!
type QUEUE_STRUCT
    logical                                 :: full
    integer                                 :: start
    integer                                 :: end
    type(QUEUE_DATA), dimension(:), pointer :: data
end type QUEUE_STRUCT

!
! Define the subroutines and functions
!
contains

! queue_create --
!     Create and initialise a queue
! Arguments:
!     queue      Pointer to new queue
!     capacity   The number of data that can be stored
! Note:
!     This version assumes a shallow copy is enough
!     (that is, there are no pointers within the data
!     to be stored)
!     It also assumes the argument queue does not already
!     refer to a queue. Use queue_destroy first to
!     destroy an old queue.
!
!     There is no check that the capacity is positive!
!
subroutine queue_create( queue, capacity )
    type(QUEUE_STRUCT), pointer        :: queue
    integer                            :: capacity

    allocate( queue )
    allocate( queue%data(1:capacity) )

    queue%full  = .false.
    queue%start = 1
    queue%end   = 0
end subroutine queue_create

! queue_destroy --
!     Destroy a queue
! Arguments:
!     queue       Pointer to the queue to be destroyed
! Note:
!     This version assumes that there are no
!     pointers within the data that need deallocation
!
subroutine queue_destroy( queue )
    type(QUEUE_STRUCT), pointer  :: queue

    deallocate( queue%data )
    deallocate( queue )
end subroutine queue_destroy

! queue_empty --
!     Check if the queue is empty
! Arguments:
!     queue       Pointer to the queue
! Result:
!     logical indicating if the queue is
!     empty or not
!
logical function queue_empty( queue )
    type(QUEUE_STRUCT), intent(in)  :: queue

    queue_empty = .not. queue%full .and. &
        queue%end .eq. queue%start - 1

end function queue_empty

! queue_full --
!     Check if the queue is full
! Arguments:
!     queue       Pointer to the queue
! Result:
!     logical indicating if the queue is
!     full or not
!
logical function queue_full( queue )
    type(QUEUE_STRUCT), intent(in)  :: queue

    queue_full = queue%full

end function queue_full

! queue_start_data
!     Return the data stored at the start,
!     but leave them in
! Arguments:
!     queue      Queue to be examined
! Result:
!     Data stored at the start
! Note:
!     With an empty queue, random data
!     are returned!
!
function queue_start_data( queue ) result(data)
    type(QUEUE_STRUCT), intent(in) :: queue
    type(QUEUE_DATA)               :: data

    data = queue%data(queue%start)

end function queue_start_data

! queue_retrieve_data
!     Return the data stored at the top,
!     and remove them from the queue
! Arguments:
!     queue      Queue to be examined
! Result:
!     Data stored at the top, afterwards
!     removed
! Note:
!     With an empty queue, random data
!     are returned!
!
function queue_retrieve_data( queue ) result(data)
    type(QUEUE_STRUCT)             :: queue
    type(QUEUE_DATA)               :: data

    data = queue%data(queue%start)

    if ( .not. queue_empty(queue) ) then
        queue%start = queue%start + 1
        if ( queue%start .gt. size(queue%data) ) then
            queue%start = 1
        endif
        queue%full = .false.
    endif

end function queue_retrieve_data

! queue_append_data
!     Append data to the end of the queue
! Arguments:
!     queue      Queueu to which to add the data
!     data       The data to be added
!     success    Indicates success or not
!
subroutine queue_append_data( queue, data, success )
    type(QUEUE_STRUCT)           :: queue
    type(QUEUE_DATA), intent(in) :: data
    logical, intent(out)         :: success

    success = .not. queue_full( queue )
    if ( success ) then
        queue%end = queue%end + 1
        if ( queue%end .gt. size(queue%data) ) then
            queue%end = 1
        endif
        if ( queue%start .eq. queue%end+1 ) then
            queue%full = .true.
        endif
        if ( queue%end   .eq. size(queue%data) .and. &
             queue%start .eq. 1 ) then
            queue%full = .true.
        endif
        queue%data(queue%end) = data
    endif
end subroutine queue_append_data
