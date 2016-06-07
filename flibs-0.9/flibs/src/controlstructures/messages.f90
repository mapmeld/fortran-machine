! messages.f90 --
!     Module (main part of a module) to handle messages
!     passed between steps in a computation
!     The idea is that this makes certain types of programming
!     easier, as the steps can be implemented independent
!     of the logic that selects them.
!
!     Requirements:
!     - This file is included in a module - see example/test program
!     - A module "msg_data_definitions" that defines:
!       - Type STATE_DATA
!       - Type MSG_DATA
!       - Integer parameter INIT
!
implicit none

!
! Standard message types
!
!! integer, parameter :: INIT = 0 ! Tricky bit!

!
! Private definitions
!
type MESSAGE
    real              :: time
    integer           :: type
    type(MSG_DATA)    :: data
end type

private :: MESSAGE
type(MESSAGE), dimension(:), allocatable, private :: queue
integer, save, private                            :: max_queue = 1000

real, private          :: system_time
integer, private, save :: last_msg  = 0
logical, private, save :: stop_loop = .false.
logical, private       :: error
logical, private       :: debug = .true.

public  :: msg_loop, msg_put
private :: msg_loop_print
private :: msg_loop_default
private :: msg_print_default

interface msg_loop
    module procedure msg_loop_print
    module procedure msg_loop_default
end interface

contains

! msg_exit --
!     Sets the system time to signal the end of the computation
!
! Arguments:
!     None
!
subroutine msg_exit
    stop_loop = .true.
end subroutine msg_exit

! msg_error --
!     Registers an error
!
! Arguments:
!     text             Error text
!
subroutine msg_error( text )
    character(len=*), intent(in) :: text
    write(*,*) 'ERROR: ', trim(text)
    error = .true.
end subroutine msg_error

! msg_print_default --
!     Default print routine
!
! Arguments:
!     lurep         LU-number for report file
!     state         State of the computation
!     time          System time
!     type          Type of the current message
!     msg           Message data
!
subroutine msg_print_default( lurep, state, time, type, msg )
    integer, intent(in)             :: lurep
    type(STATE_DATA), intent(inout) :: state
    real, intent(in)                :: time
    integer, intent(in)             :: type
    type(MSG_DATA), intent(in)      :: msg

    ! TODO
    write(*,'(a,f12.4,a,i5)') 'Time: ', time, ' - type: ', type

end subroutine msg_print_default

! msg_loop_default --
!     Dispatch the messages to the routine that actually handles them.
!     Default debug printing facilities used.
!
! Arguments:
!     state         The state of the computation
!     machine       The routine that handles the messages
!
subroutine msg_loop_default( state, machine )
    type(STATE_DATA) :: state
    interface
        subroutine machine( state, time, type, data )
            use msg_data_definitions
            implicit none
            type(STATE_DATA), intent(inout) :: state
            real, intent(in)                :: time
            integer, intent(in)             :: type
            type(MSG_DATA), intent(in)      :: data
        end subroutine
    end interface

    call msg_loop_print( state, machine, msg_print_default )

end subroutine msg_loop_default

! msg_loop_print --
!     Dispatch the messages to the routine that actually handles them.
!     Use specific debug printing facilities.
!
! Arguments:
!     state         The state of the computation
!     machine       The routine that handles the messages
!     print_debug   The routine that prints debug information
!
subroutine msg_loop_print( state, machine, print_debug )
    type(STATE_DATA) :: state
    interface
        subroutine machine( state, time, type, data )
            use msg_data_definitions
            implicit none
            type(STATE_DATA), intent(inout) :: state
            real, intent(in)                :: time
            integer, intent(in)             :: type
            type(MSG_DATA), intent(in)      :: data
        end subroutine
    end interface
    interface
        subroutine print_debug( lurep, state, time, type, data )
            use msg_data_definitions
            implicit none
            integer, intent(in)             :: lurep
            type(STATE_DATA), intent(inout) :: state
            real, intent(in)                :: time
            integer, intent(in)             :: type
            type(MSG_DATA), intent(in)      :: data
        end subroutine
    end interface

    integer :: lurep = 10
    real    :: time
    integer :: count = 0
    integer :: i

!
! This _uninitialised_ variable is used for special messages
!
    type(MSG_DATA) :: neutral_data

!
! Initialise the queue
!
    error       = .false.
    system_time = 0.0
    stop_loop   = .false.

    call msg_put( system_time, INIT, neutral_data )

!
! Enter the message loop
!
    do while( .not. stop_loop )
        !count = count + 1; if ( count > 40 ) stop
        !write(*,*) 'last_msg: ', last_msg
        !write(*,'(f12.4,i5)') (queue(i)%time, queue(i)%type ,i=1,last_msg )

        !
        ! Any messages in the queue? If not, we are done
        !
        if ( last_msg <= 0 ) then
            exit
        endif

        !
        ! Pass the message to the handler,
        ! print debug information, if requested
        !
        system_time = queue(1)%time
        time        = system_time

        if ( debug ) then
            call print_debug( lurep, state, time, &
                     queue(1)%type, queue(1)%data )
        endif

        time = system_time
        call machine( state, time, queue(1)%type, queue(1)%data )

        !
        ! Remove the first message
        !
        call deallocate_msg_data( queue(1)%data )
        queue(1:last_msg-1) = queue(2:last_msg)
        last_msg = last_msg - 1
    enddo

    !
    ! Be prepared for the next loop
    !
    last_msg    = 0

end subroutine msg_loop_print

! msg_put --
!     Put a message in the (sorted) message queue
!
! Arguments:
!     time          Time at which the message is posted
!     type          Type of the message
!     data          Data associated with the message
!
subroutine msg_put( time, type, data )
    real, intent(in)             :: time
    integer, intent(in)          :: type
    type(MSG_DATA), intent(in)   :: data

    integer                      :: i
    integer                      :: insert

    !
    ! Initialise the queue if needed
    !
    if ( .not. allocated(queue) ) then
        allocate( queue(1:max_queue) )
    endif

    !
    ! Accept only messages with a time equal/later than the system time
    !
    if ( time < system_time ) then
        call msg_error( 'Time of message before system time!' )
        return
    endif

    !
    ! Do we have enough space for the message in the queue"?
    !
    if ( last_msg >= max_queue ) then
        error = .true.
        call msg_error( "Queue full" )
        return
    endif

    !
    ! Insert the message in the queue
    !
    insert = last_msg+1
    do i = 1,last_msg
        if ( queue(i)%time > time ) then
            insert = i ! Insert before this message
            exit
        endif
    enddo

    queue(insert+1:last_msg+1) = queue(insert:last_msg)
    queue(insert)%time         = time
    queue(insert)%type         = type
    call copy_msg_data( data, queue(insert)%data )
    last_msg                   = last_msg + 1

    !write(*,*) 'msg_put:',last_msg, time, type, data
    !write(*,*) '    first:',queue(1)%time, queue(1)%type, queue(1)%data
end subroutine msg_put

! End of general module text
