! fsm_state.f90 --
!     Definition of the FSM_STATE derived type
!     To be included in the module "fsm_data_definitions"
!
!     Note:
!     This is due to a limitation in the way interface blocks
!     work in Fortran 90.
!
type FSM_STATE
    !private - can not use this in the current implementation
    character(len=40) :: state_name
    integer           :: state_no
    integer           :: lurep
    logical           :: finished
end type FSM_STATE
