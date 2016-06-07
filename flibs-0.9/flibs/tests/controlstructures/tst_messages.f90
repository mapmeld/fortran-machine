! tst_messages.f90 --
!     Test/demonstration program for the messages module
!
!     The program simulates a simplified elevator system:
!     - The elevator passes six floors
!     - If someone presses the up or down button, this
!       event is registered as a message that includes the
!       floor the person wants to go to.
!     - The elevator moves up and down according to the
!       messages received
!     - In an array information is kept if the elevator
!       should stop on the way up or down
!
! elevator_simulation --
!     Module to define the elevator system we are trying
!     to simulate
!
module msg_data_definitions
    type MSG_DATA
        integer     :: start          ! The floor the person is on now
        logical     :: up             ! Going upward?
        integer     :: destination    ! The floor he/she wants to go to
    end type MSG_DATA

    type STATE_DATA
        logical, dimension(2,6) :: stopping  ! Stop at this floor?
        integer                 :: position  ! Position of the elevator
        integer                 :: direction ! Direction of motion (-1, 0 or 1)
    end type STATE_DATA
    !
    ! Messages defined in the elevator system
    !
    integer, parameter :: INIT  = 0  ! Tricky: has to be defined in messages module!
    integer, parameter :: CHECK = 1
    integer, parameter :: PUSH  = 2
    integer, parameter :: FLOOR = 3

contains

! copy_msg_data --
!     Copy the message data
!
! Arguments:
!     src           Original data
!     dest          Copy to be stored in the message queue
!
subroutine copy_msg_data( src, dest )
    type(MSG_DATA), intent(in)  :: src
    type(MSG_DATA), intent(out) :: dest

    !
    ! In this case: trivial
    !
    dest = src

end subroutine copy_msg_data

! deallocate_msg_data --
!     Deallocate any allocated memory in the message data
!
! Arguments:
!     msg           Message possibly holding allocated memory
!
subroutine deallocate_msg_data( msg )
    type(MSG_DATA), intent(inout) :: msg

    !
    ! In this case: there is no allocated memory, so nothing to do
    !

end subroutine deallocate_msg_data

end module msg_data_definitions

! Define the messages module
!
module messages
    use msg_data_definitions
    include 'messages.f90'
end module messages

! Define the specific simulation routines
!
module elevator_simulation
    use msg_data_definitions
    use messages
    implicit none

contains


! elevator_system --
!     Handle the events in the elevator system
! Arguments:
!     data          State information of the system
!     time          System time
!     type          Type of message
!     msg           Contents of the message
!
subroutine elevator_system( data, time, type, msg )
    type(STATE_DATA), intent(inout)  :: data
    real, intent(in)                 :: time
    integer                          :: type
    type(MSG_DATA),  intent(inout)   :: msg

    integer                          :: new_floor
    integer                          :: this_floor
    integer                          :: this_dir

    character(len=10), dimension(3)  :: dir = &
        (/ 'Going DOWN', 'Stationary', 'Going UP  ' /)

    select case( type )
        !
        ! The initial event allows us to get started:
        ! Send a "check" event to see if anything needs to be done
        !
        case( INIT )
            call msg_put( time, CHECK, msg_data( 0, .false., 0 ) )

        !
        ! Someone has pushed the up or down button
        !
        case( PUSH )
            if ( msg%up ) then
                data%stopping(1,msg%start)       = .true.
                data%stopping(1,msg%destination) = .true.
            else
                data%stopping(2,msg%start)       = .true.
                data%stopping(2,msg%destination) = .true.
            endif

            write(*,*) 'Time: ', time
            write(*,*) '    From floor ', msg%start, ' to ', msg%destination
        !
        ! See if any of the buttons have been pressed
        ! - but keep the current motion in mind
        !
        case( CHECK )
            if ( time > 100.0 ) then
                call msg_exit ! We have seen enough
            endif

            if ( any(data%stopping) ) then
                call decide_direction( data )
                new_floor = data%position + data%direction
                call msg_put( time+3.0, FLOOR, msg_data( new_floor, .false., 0 ) )

                write(*,*) 'Time: ', time
                write(*,*) '     ', dir(data%direction+2)
            else
                call msg_put( time+0.5, CHECK, msg_data( 0, .false., 0 ) )
            endif

        !
        ! We have arrived on a new floor:
        ! Clear the flag and let's see what to do next
        !
        case( FLOOR )
            data%position = msg%start
            this_floor    = data%position
            this_dir      = merge( 1, 2, data%direction == 1 )
            if ( data%stopping(this_dir,this_floor) ) then
                if ( this_floor == 0 ) then
                    write(*,'(2l5)') data%stopping
                endif
                data%stopping(this_dir,this_floor) = .false.
                write(*,*) 'Time: ', time
                write(*,*) '     Arrived at floor ', data%position
            else
                write(*,*) 'Time: ', time
                write(*,*) '     Passing floor ', data%position
            endif
            call msg_put( time, CHECK, msg_data( 0, .false., 0 ) )

        case default
            ! Ignore any other messages
    end select

end subroutine elevator_system


! decide_direction --
!     Decide what direction to move
! Arguments:
!     data          State information of the system
!
subroutine decide_direction( data )
    type(STATE_DATA), intent(inout)  :: data

    integer                          :: pos

    pos = data%position

    !
    ! No motion: take the upward direction first
    !
    if ( data%direction == 0 ) then
        if ( any(data%stopping(:,pos+1:)) ) then
            data%direction = 1
        else
            data%direction = -1
        endif
    else
        !
        ! Continue in the same direction if possible
        !
        if ( data%direction == 1 ) then
            if ( any(data%stopping(:,pos+1:)) ) then
                data%direction = 1
            else if ( any(data%stopping(:,1:pos-1)) ) then
                data%direction = -1
            else
                data%direction = 0
            endif
        else
            if ( any(data%stopping(:,1:pos-1)) ) then
                data%direction = -1
            else if ( any(data%stopping(:,pos+1:)) ) then
                data%direction = 1
            else
                data%direction = 0
            endif
        endif
    endif

end subroutine decide_direction

end module elevator_simulation


! Actual program:
! There are two flaws:
! - The initial PUSH message leaves a person at floor 1
! - At the end of the simulation this is an excuse to move to floor 0
!
! Not bothering to solve this: the program seems to be doing its job
! otherwise, thus showing the messages module works.
!
program tst_messages
    use elevator_simulation

    type(STATE_DATA) :: data

    !
    ! Initialise the system
    ! - The floors are numbered from 1 to 6
    ! - The elevator starts at 1
    ! - There is no one about at the start
    !
    data%stopping  = .false.
    data%position  = 1
    data%direction = 0

    !
    ! Insert the initial messages to get the thing going
    !
    call msg_put( 3.0, PUSH, msg_data(3, .true.,  5) )
    call msg_put( 3.5, PUSH, msg_data(3, .false., 2) )
    call msg_put( 2.5, PUSH, msg_data(1, .true.,  6) )

    !
    ! Start the simulation
    !
    call msg_loop( data, elevator_system )

    !
    ! Done
    !
end program tst_messages
