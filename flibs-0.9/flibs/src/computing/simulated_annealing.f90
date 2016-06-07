! simulated_annealing.f90 --
!    Module for optimisation via simulated annealing
!
!    The prime routine defined here requires two or three
!    user-defined routines and a suitable data structure
!    The data structure (derived type) is to be defined via the
!    module "annealing_data" and is to be called
!    "SOLUTION_DATA".
!    This structure contains all there is to know about
!    the intermediate solutions that will be generated as
!    the algorithm proceeds. At the end it contains the
!    solution that was found to be optimal.
!
!    The subroutines and functions are:
!
!        nextsolution  Subroutine to generate the next solution
!        costfunc      Function to evaluate the cost of a solution
!        reporting     Subroutine to report the progress (optional)
!
!    Note:
!    The data need to be defined in the separate module,
!    because of the interface definitions. This module must contain
!    a subroutine to copy a solution (see the example). Most
!    solution data structures will have pointer fields, so as to
!    allow different problem sizes.
!
!    A trivial example:
!    To sort integers in ascending order one can use
!    a module like this:
!        module annealing_data
!            implicit none
!            type SOLUTION_DATA
!                integer, dimension(:), pointer :: int
!             end type SOLUTION_DATA
!        contains
!        subroutine copy_solution( in, out )
!            ... Required
!        end subroutine
!        end module annealing_data
!
module simulated_annealing
    use annealing_data
    implicit none

    type annealing_parameters
        integer      :: max_trials      = 2000       ! Maximun solutions per step
        integer      :: max_steps       =   10       ! Maximun number of steps
        real         :: accept_fraction =    0.1     ! Fraction to accept
        real         :: temp_reduction  =    0.5     ! Reduction factor for temperature
    end type annealing_parameters

    interface anneal
        module procedure anneal_with_report
        module procedure anneal_no_report
    end interface
contains

! set_annealing_parameters --
!
! TODO
!

! dummy_reporting --
!     Dummy reporting subroutine
!
subroutine dummy_reporting( temp, solution, currcost, nsuccess, idx )
    use annealing_data
    real, intent(in)                            :: temp
    type(SOLUTION_DATA), intent(in)             :: solution
    real, intent(in)                            :: currcost
    integer, intent(in)                         :: nsuccess
    integer, intent(in)                         :: idx

    return
end subroutine reporting

! anneal_with_report --
!     Subroutine to run the annealing algorithm (with reporting)
! Arguments:
!     params        Annealing parameters
!     solution      Initial and final solution
!     nextsolution  Function to generate the next solution
!     costfunc      Function to evaluate the cost of the solution
!     reporting     Subroutine to report the progress
!
subroutine anneal_with_report( params, solution, nextsolution, costfunc, reporting )
    type(ANNEALING_PARAMETERS), intent(in) :: params
    type(SOLUTION_DATA), intent(inout)     :: solution

    interface
        subroutine reporting( temp, solution, currcost, nsuccess, idx )
            use annealing_data
            real, intent(in)                            :: temp
            type(SOLUTION_DATA), intent(in)             :: solution
            real, intent(in)                            :: currcost
            integer, intent(in)                         :: nsuccess
            integer, intent(in)                         :: idx
        end subroutine reporting
    end interface
    interface
        function nextsolution( solution ) result( next )
            use annealing_data
            type(SOLUTION_DATA) :: solution, next
        end function nextsolution
    end interface
    interface
        real function costfunc( solution )
            use annealing_data
            type(SOLUTION_DATA) :: solution
        end function costfunc
    end interface

    type(SOLUTION_DATA) :: next
    real                :: currcost
    real                :: cost
    real                :: temp
    integer             :: istep
    integer             :: idx
    integer             :: no_accepted
    integer             :: max_accept

    temp       = 1.0
    currcost   = costfunc( solution )
    max_accept = params%accept_fraction * params%max_trials

    do istep = 1,params%max_steps
        no_accepted = 0
        do idx = 1,params%max_trials
            if ( no_accepted > max_accept ) then
                exit
            endif

            ! Ai, memory leak!
            next = nextsolution(solution)
            cost = costfunc(solution)

            call random_number( r )
            if ( r < exp(-(cost-currcost)/temp) ) then
                call copy_solution( nextsolution(solution), solution )
                currcost    = cost
                no_accepted = no_accepted + 1
            endif
        enddo

        call reporting( temp, solution, currcost, no_accepted, idx-1 )
    enddo

end subroutine anneal_with_report

! anneal_no_report
!     Subroutine to run the annealing algorithm (without reporting)
! Arguments:
!     params        Annealing parameters
!     solution      Initial and final solution
!     nextsolution  Function to generate the next solution
!     costfunc      Function to evaluate the cost of the solution
!
subroutine anneal_no_report( params, solution, nextsolution, constfunc )
    type(ANNEALING_PARAMETERS), intent(in) :: params
    type(SOLUTION_DATA), intent(inout)     :: solution

    interface
        function nextsolution( solution ) result( next )
            use annealing_data
            type(SOLUTION_DATA) :: solution, next
        end function nextsolution
    end interface
    interface
        real function costfunc( solution )
            use annealing_data
            type(SOLUTION_DATA) :: solution
        end function costfunc
    end interface

    call anneal_with_report( params, solution, nextsolution, costfunc, dummy_reporting )
end subroutine anneal_no_report

end module simulated_annealing
