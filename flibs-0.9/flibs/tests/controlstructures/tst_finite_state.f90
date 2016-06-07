! tst_finite_state.f90 --
!     Test/demonstration program for the finite state machine
!     module
!
!     The program takes a string containing an arithmetic
!     expression with integers only and analyses it.
!     The state machine works as follows:
!     - On each entry it takes the next character
!     - If it is in the state "number", then that character
!       is either a digit, an operator or a closing parenthesis
!     - If it is in the state "operator", then the character
!       can be a digit or an open parenthesis
!     - Similar for a parenthesis
!     - It expect NO spaces to be present - a space ends the expression -
!       to keep it simple)
!     Here is a schematic overview:
!     State         Character        New state
!     INIT          digit            DIGIT
!                   -,+              UNARY
!                   (                OPEN
!                   anything else    ERROR
!     DIGIT         digit            DIGIT
!                   +,-,*,/          OPERATION
!                   )                CLOSE
!                   anything else    ERROR
!     OPERATION     digit            DIGIT
!                   (                OPEN
!                   anything else    ERROR
!     OPEN          digit            DIGIT
!                   (                OPEN
!                   +,-              UNARY
!                   anything else    ERROR
!     CLOSE         +,-,*,/          OPERATION
!                   )                CLOSE
!                   anything else    ERROR
!     any           space            DONE
!

! fsm_data_definitions --
!     Module to define the data structure that represents
!     data that are manipulated by the FSM.
!
module fsm_data_definitions
    implicit none

    include 'fsm_state.f90'

    type STATE_DATA
        integer :: position           ! Current position in the string
        integer :: open_parens        ! Number of open parentheses
        character(len=80)              :: string ! String holding the expression
    end type STATE_DATA

end module fsm_data_definitions

module analyse_string
    use fsm_data_definitions
    include 'finite_state.f90'

!
! Here is the actual routine that implements the finite state machine
!
! analyse_expression --
!     Analyse an arithmetic expression
! Arguments:
!     fsm           Private data structure for the FSM machinery
!     data          Evaluation data structure
!     state_name    Current state of the machine
!
subroutine analyse_expression( fsm, data, state_name )
    type(FSM_STATE),  intent(inout)  :: fsm
    type(STATE_DATA), intent(inout)  :: data
    character(len=*), intent(in)     :: state_name

    character(len=1)                 :: curr_char

    select case( state_name )
        case( FSM_INIT_CHAR )
            data%position    = 1
            data%open_parens = 0

            curr_char = data%string(data%position:data%position)

            call fsm_set_state( fsm, 'ERROR' )
            if ( index( '0123456789', curr_char ) > 0 ) call fsm_set_state( fsm, 'DIGIT' )
            if ( index( '-+', curr_char ) > 0 )         call fsm_set_state( fsm, 'UNARY' )
            if ( index( '(', curr_char ) > 0 )          call fsm_set_state( fsm, 'OPEN' )

        case( 'DIGIT' )
            data%position = data%position + 1
            curr_char = data%string(data%position:data%position)
            call fsm_set_state( fsm, 'ERROR' )
            if ( index( ' ', curr_char ) > 0 )          call fsm_set_state( fsm, 'DONE' )
            if ( index( ')', curr_char ) > 0 )          call fsm_set_state( fsm, 'CLOSE' )
            if ( index( '0123456789', curr_char ) > 0 ) call fsm_set_state( fsm, 'DIGIT' )
            if ( index( '-+*/', curr_char ) > 0 )       call fsm_set_state( fsm, 'OPERATION' )

        case( 'OPERATION' )
            data%position = data%position + 1
            curr_char = data%string(data%position:data%position)
            call fsm_set_state( fsm, 'ERROR' )
            if ( index( ' ', curr_char ) > 0 )          call fsm_set_state( fsm, 'ERROR' )
            if ( index( '0123456789', curr_char ) > 0 ) call fsm_set_state( fsm, 'DIGIT' )
            if ( index( '(', curr_char ) > 0 )          call fsm_set_state( fsm, 'OPEN'  )

        case( 'UNARY' )
            data%position = data%position + 1
            curr_char = data%string(data%position:data%position)
            call fsm_set_state( fsm, 'ERROR' )
            if ( index( ' ', curr_char ) > 0 )          call fsm_set_state( fsm, 'ERROR' )
            if ( index( '0123456789', curr_char ) > 0 ) call fsm_set_state( fsm, 'DIGIT' )
            if ( index( '(', curr_char ) > 0 )          call fsm_set_state( fsm, 'OPEN'  )

        case( 'OPEN' )
            data%open_parens = data%open_parens + 1
            data%position = data%position + 1
            curr_char = data%string(data%position:data%position)
            call fsm_set_state( fsm, 'ERROR' )
            if ( index( ' ', curr_char ) > 0 )          call fsm_set_state( fsm, 'ERROR' )
            if ( index( '0123456789', curr_char ) > 0 ) call fsm_set_state( fsm, 'DIGIT' )
            if ( index( '-+', curr_char ) > 0 )         call fsm_set_state( fsm, 'UNARY' )
            if ( index( '(', curr_char ) > 0 )          call fsm_set_state( fsm, 'OPEN'  )

        case( 'CLOSE' )
            data%open_parens = data%open_parens - 1
            data%position = data%position + 1
            curr_char = data%string(data%position:data%position)
            call fsm_set_state( fsm, 'ERROR' )
            if ( index( ' ', curr_char ) > 0 )          call fsm_set_state( fsm, 'DONE' )
            if ( index( '0123456789', curr_char ) > 0 ) call fsm_set_state( fsm, 'DIGIT' )
            if ( index( '-+*/', curr_char ) > 0 )       call fsm_set_state( fsm, 'OPERATION' )
            if ( index( '(', curr_char ) > 0 )          call fsm_set_state( fsm, 'CLOSE' )

        case( 'DONE' )
            if ( data%open_parens > 0 ) then
                write(*,*) 'Error: unbalanced parentheses in expression: '
                write(*,*) '       ', trim(data%string)
            else
                write(*,*) 'Expression is syntactically correct: '
                write(*,*) '       ', trim(data%string)
            endif
            call fsm_finish(fsm)

        case( 'ERROR' )
            write(*,*) 'Error: syntax error in expression: '
            write(*,*) '       ', trim(data%string)
            write(*,*) '       ', repeat(' ',data%position-1),'^'
            call fsm_finish(fsm)

        case default
            write(*,*) 'Programming error: unhandled state - ', state_name
    end select

end subroutine analyse_expression

end module analyse_string



program tst_finite_state
    use analyse_string

    type(STATE_DATA) :: data

    !
    ! Try several expressions:
    !
    data%string = '1+2*(3+4)/2'
    call fsm_loop( data, analyse_expression )

    data%string = '1+2*(3+4/2'
    call fsm_loop( data, analyse_expression )

    data%string = '1+2(3+4)/2'
    call fsm_loop( data, analyse_expression )

end program tst_finite_state
