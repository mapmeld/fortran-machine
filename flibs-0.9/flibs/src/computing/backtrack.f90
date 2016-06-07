! backtrack.f90 --
!    Module implementing a simple backtracking mechanism
!
!    The prime routine defined here requires four
!    user-defined routines and a suitable data structure
!    The data structure (derived type) is to be defined via the
!    module "backtracking_data" and is to be called
!    "SOLUTION_DATA".
!    This structure contains all there is to know about
!    the (partial) solutions that will be expanded as
!    the algorithm proceeds
!
!    The subroutines and functions are:
!
!        generate    Subroutine to generate the solutions
!        acceptable  Function to check a solution
!        free        Subroutine to free any memory
!                    associated with solutions
!        collect     Subroutine to collect solutions
!
!    Note:
!    The data need to be defined in the parate module,
!    because of the interface definitions
!
!    For example:
!    For the eight queens problem, the solution consists
!    of the position of the queens on at most 8 columns
!        module backtracking_data
!            implicit none
!            type SOLUTION_DATA
!                integer, dimension(8) :: row
!             end type SOLUTION_DATA
!        end module backtracking_data
!
module backtracking
    use backtracking_data
    implicit none
contains

! bt_solve --
!     Subroutine to run the backtracking algorithm
! Arguments:
!     base        Base solution or solutions
!     nostage     Number of stages
!     generate    Subroutine to generate the solutions
!     acceptable  Function to check a solution
!     free        Subroutine to free any memory
!                 associated with solutions
!     collect     Subroutine to collect solutions
!     stage_priv  Current stage (hidden)
!
recursive subroutine bt_solve( base, nostage, generate, acceptable, free, collect, stage_priv )
    type(SOLUTION_DATA), dimension(:) :: base
    integer, intent(in)               :: nostage
    integer, intent(in), optional     :: stage_priv

    interface
        subroutine generate( base, stage, solutions )
            use backtracking_data
            type(SOLUTION_DATA), intent(in)             :: base
            integer, intent(in)                         :: stage
            type(SOLUTION_DATA), dimension(:), pointer  :: solutions
        end subroutine generate
    end interface
    interface
        subroutine free( solutions )
            use backtracking_data
            type(SOLUTION_DATA), dimension(:), pointer  :: solutions
        end subroutine free
    end interface
    interface
        logical function acceptable( solution, stage )
            use backtracking_data
            type(SOLUTION_DATA), intent(in) :: solution
            integer, intent(in)             :: stage
        end function acceptable
    end interface
    interface
        subroutine collect( solution, stop )
            use backtracking_data
            type(SOLUTION_DATA), intent(in) :: solution
            logical, intent(out)            :: stop
        end subroutine collect
    end interface

    logical :: accept
    logical :: stop
    integer :: i
    integer :: stage
    type(SOLUTION_DATA), dimension(:), pointer  :: new_solutions

    if ( present(stage_priv) ) then
        stage = stage_priv
    else
        stage = 0
    endif

    !
    ! For each base solution:
    ! 1. If we are starting the search (stage = 0), then
    !    accept all incoming base solutions
    ! 2. If we have reached the last stage, the acceptable base
    !    solutions are real solutions
    ! 3. Otherwise, they can be used to generate new solutions
    !

    do i = 1,size(base)
        accept = (stage == 0)
        if ( .not. accept) accept = acceptable( base(i), stage )
        if ( accept ) then
            if ( stage == nostage ) then
                call collect( base(i), stop )
                if ( stop ) then
                    return
                endif
            else
                call generate( base(i), stage+1, new_solutions )
                call bt_solve( new_solutions, nostage, generate, acceptable, free, collect, stage+1 )
                call free( new_solutions )
            endif
        endif
    enddo
end subroutine bt_solve

end module backtracking
