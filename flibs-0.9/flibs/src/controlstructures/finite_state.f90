! finite_state.f90 --
!     Module (main part of a module) to help implement a finite
!     state machine. This is actually an almost trivial module
!
!     Requirements:
!     - This file is included in a module - see example/test program
!     - A module "fsm_data_definitions" that defines:
!       -  Type STATE_DATA
!       -  Type FSM_STATE - via including "fsm_state.f90"
!

!Definition of FSM_STATE moved to "fsm_state.f90"
!type FSM_STATE
!    private
!    character(len=40) :: state_name
!    integer           :: state_no
!    integer           :: lurep
!    logical           :: finished
!end type FSM_STATE

interface fsm_set_state
    module procedure fsm_set_state_int
    module procedure fsm_set_state_char
end interface

interface fsm_get_state
    module procedure fsm_get_state_int
    module procedure fsm_get_state_char
end interface

interface fsm_loop_int
    module procedure fsm_loop_default_int
end interface

interface fsm_loop
    module procedure fsm_loop_default_char
end interface

interface fsm_loop_print_int
    module procedure fsm_loop_print_int
end interface
interface fsm_loop_print
    module procedure fsm_loop_print_char
end interface

integer, parameter :: FSM_INIT = 0
character(len=10), parameter:: FSM_INIT_CHAR = 'INIT'

contains

! fsm_finish --
!     Sets a flag to indicate that the finite state machine has finished
!
! Arguments:
!     fsm           The FSM_STATE structure
!
subroutine fsm_finish( fsm )
    type(FSM_STATE), intent(inout) :: fsm

    fsm%finished = .true.
end subroutine fsm_finish

! fsm_set_state --
!     Sets the state (either as a string or as an integer)
!
! Arguments:
!     fsm          The FSM_STATE structure
!     new_state    The ID of the new state (if string: at most 40 characters)
!
subroutine fsm_set_state_int( fsm, new_state )
    type(FSM_STATE), intent(inout) :: fsm
    integer,         intent(in)    :: new_state

    fsm%state_no = new_state
end subroutine fsm_set_state_int

subroutine fsm_set_state_char( fsm, new_state )
    type(FSM_STATE), intent(inout)  :: fsm
    character(len=*), intent(in)    :: new_state

    fsm%state_name = new_state
end subroutine fsm_set_state_char

! fsm_get_state --
!     Gets the state (either as a string or as an integer)
!
! Arguments:
!     fsm          The FSM_STATE structure
!     state        The ID of the state (if string: at most 40 characters)
!
subroutine fsm_get_state_int( fsm, state )
    type(FSM_STATE), intent(in)    :: fsm
    integer,         intent(out)   :: state

    state = fsm%state_no
end subroutine fsm_get_state_int

subroutine fsm_get_state_char( fsm, state )
    type(FSM_STATE),  intent(in)    :: fsm
    character(len=*), intent(out)   :: state

    state = fsm%state_name
end subroutine fsm_get_state_char

! fsm_set_lurep --
!     Set the LU number for debugging
!
! Arguments:
!     fsm           The FSM_STATE structure
!     lurep         The LU number to be used
!
subroutine fsm_set_lurep( fsm, lurep )
    type(FSM_STATE)      :: fsm
    integer, intent(in) :: lurep

    fsm%lurep = lurep
end subroutine fsm_set_lurep

! fsm_loop_default --
!     Dispatch the messages to the routine that actually handles them.
!     Default debug printing facilities used.
!
! Arguments:
!     data          The state of the computation
!     machine       The routine that handles according to the present state
!
subroutine fsm_loop_default_int( data, machine )
    type(STATE_DATA) :: data

    interface
        subroutine machine( fsm, data, curstate )
            use fsm_data_definitions
            implicit none
            type(FSM_STATE),   intent(inout) :: fsm
            type(STATE_DATA), intent(inout) :: data
            integer,          intent(in)    :: curstate
        end subroutine
    end interface

    call fsm_loop_print_int( data, machine, fsm_print_default_int )

end subroutine fsm_loop_default_int

subroutine fsm_loop_default_char( data, machine )
    type(STATE_DATA) :: data

    interface
        subroutine machine( fsm, data, curstate )
            use fsm_data_definitions
            implicit none
            type(FSM_STATE),   intent(inout) :: fsm
            type(STATE_DATA), intent(inout) :: data
            character(len=*), intent(in)    :: curstate
        end subroutine
    end interface

    call fsm_loop_print_char( data, machine, fsm_print_default_char )

end subroutine fsm_loop_default_char

! fsm_loop_print --
!     Dispatch the messages to the routine that actually handles them.
!     Use specific debug printing facilities.
!
! Arguments:
!     state         The state of the computation
!     machine       The routine that handles the messages
!     print_debug   The routine that prints debug information
!
subroutine fsm_loop_print_int( state, machine, print_debug )
    type(STATE_DATA) :: state
    interface
        subroutine machine( fsm, data, curstate )
            use fsm_data_definitions
            implicit none
            type(FSM_STATE),   intent(inout) :: fsm
            type(STATE_DATA), intent(inout) :: data
            integer,          intent(in)    :: curstate
        end subroutine
    end interface
    interface
        subroutine print_debug( lurep, data, oldstate, curstate )
            use fsm_data_definitions
            implicit none
            integer, intent(in)             :: lurep
            type(STATE_DATA), intent(inout) :: data
            integer, intent(in)             :: oldstate
            integer, intent(in)             :: curstate
        end subroutine
    end interface

    type(FSM_STATE) :: fsm
    integer        :: oldstate
!
! Initialise the machine
!
    call fsm_set_state( fsm, FSM_INIT )
    fsm%finished = .false.

!
! Enter the loop
!
    do while( .not. fsm%finished )
        oldstate = fsm%state_no
        call machine( fsm, state, fsm%state_no )
        call print_debug( fsm%lurep, state, oldstate, fsm%state_no )
    enddo
end subroutine fsm_loop_print_int

subroutine fsm_loop_print_char( state, machine, print_debug )
    type(STATE_DATA) :: state
    interface
        subroutine machine( fsm, data, curstate )
            use fsm_data_definitions
            implicit none
            type(FSM_STATE),   intent(inout) :: fsm
            type(STATE_DATA), intent(inout) :: data
            character(len=*), intent(in)    :: curstate
        end subroutine
    end interface
    interface
        subroutine print_debug( lurep, data, oldstate, curstate )
            use fsm_data_definitions
            implicit none
            integer, intent(in)             :: lurep
            type(STATE_DATA), intent(inout) :: data
            character(len=*), intent(in)    :: oldstate
            character(len=*), intent(in)    :: curstate
        end subroutine
    end interface

    type(FSM_STATE)    :: fsm
    character(len=40)  :: oldstate
!
! Initialise the machine
!
    call fsm_set_state( fsm, FSM_INIT_CHAR )
    fsm%finished = .false.

!
! Enter the loop
!
    do while( .not. fsm%finished )
        oldstate = fsm%state_name
        call machine( fsm, state, fsm%state_name )
        call print_debug( fsm%lurep, state, oldstate, fsm%state_name )
    enddo
end subroutine fsm_loop_print_char

! fsm_print_default --
!     Default printing routine. Does nothing
!
! Arguments:
!     lurep         LU-number for reporting
!     state         The state of the computation
!     oldstate      Number of old state
!     newstate      Number of new state
!
subroutine fsm_print_default_int( lurep, state, oldstate, newstate )
    integer, intent(in)             :: lurep
    type(STATE_DATA), intent(inout) :: state
    integer, intent(in)             :: oldstate
    integer, intent(in)             :: newstate

    return
end subroutine fsm_print_default_int

subroutine fsm_print_default_char( lurep, state, oldstate, newstate )
    integer, intent(in)             :: lurep
    type(STATE_DATA), intent(inout) :: state
    character(len=*), intent(in)    :: oldstate
    character(len=*), intent(in)    :: newstate

    return
end subroutine fsm_print_default_char
! End of general module text
