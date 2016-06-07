! annealing.f90 --
!     Module for a simple implementation of
!     simulated annealing
!
!     $Id: annealing.f90,v 1.3 2008/09/22 18:41:28 arjenmarkus Exp $
!

module simulated_annealing
    use select_precision

    integer, parameter :: annealing_init   = 0
    integer, parameter :: annealing_value  = 1
    integer, parameter :: annealing_report = 2
    integer, parameter :: annealing_done   = 3

    type ANNEALING_PARAMETERS
        real    :: initial_temp
        real    :: temperature
        real    :: temp_reduction
        real    :: scale_factor
        real    :: old_value
        real    :: old_x
        integer :: number_iterations
        integer :: iteration_count
        integer :: state               = 0 ! Initialisation
        integer :: accepted
        integer :: old_idx
        logical :: verbose
        logical :: automatic_scaling
        logical :: changes
    endtype ANNEALING_PARAMETERS

    private :: determine_new_vector

contains

! set_parameters --
!     Initialise or update the annealing parameters
!
! Arguments:
!     params              The structure holding the parameters
!     update              If true, only update the parameters in the argument list
!                         otherwise re-initialise
!     initial_temp        Initial temperature
!     temp_reduction      Factor by which to reduce the temperature
!     number_iterations   Number of iterations before reducing the temperature
!     scale_factor        Factor by which to scale the values
!     automatic_scaling   If true, first determine a useful scale factor
!     verbose             If true, print
!
subroutine set_parameters( params, update, initial_temp, temp_reduction, &
                           number_iterations, scale_factor, automatic_scaling, verbose )
    type(ANNEALING_PARAMETERS), intent(inout) :: params
    logical, intent(in)                       :: update
    real, intent(in), optional                :: initial_temp
    real, intent(in), optional                :: temp_reduction
    integer, intent(in), optional             :: number_iterations
    real, intent(in), optional                :: scale_factor
    logical, intent(in), optional             :: automatic_scaling
    logical, intent(in), optional             :: verbose

    params%state = 0 ! Initialisation

    if ( .not. update ) then
        params%initial_temp       = 1.0
        params%temp_reduction     = 0.95
        params%scale_factor       = 1.0
        params%number_iterations  = 100
        params%verbose            = .false.
        params%automatic_scaling  = .false.
        params%old_value          = sqrt(huge(params%old_value))
    endif

    if ( present(initial_temp) ) then
        params%initial_temp       = initial_temp
    endif

    if ( present(temp_reduction) ) then
        params%temp_reduction     = temp_reduction
    endif

    if ( present(scale_factor) ) then
        params%scale_factor       = scale_factor
    endif

    if ( present(number_iterations) ) then
        params%number_iterations  = number_iterations
    endif

    if ( present(automatic_scaling) ) then
        params%automatic_scaling  = automatic_scaling
    endif

    if ( present(verbose) ) then
        params%verbose            = verbose
    endif
end subroutine set_parameters

! determine_new_vector --
!     Determine a new random vector
!
! Arguments:
!     range               Range for the solution parameters
!     x                   New solution parameters
!
subroutine determine_new_vector( range, x, idx, oldx )
    real(wp), dimension(:,:), intent(in)      :: range
    real(wp), dimension(:), intent(inout)     :: x
    integer, intent(out)                      :: idx
    real(wp), intent(out)                     :: oldx

    real(wp)                                  :: y

    call random_number( y )
    idx = min( 1 + size(x) * y, size(x) )
    oldx = x(idx)

    call random_number( y )
    x(idx) = range(1,idx) + (range(2,idx)-range(1,idx)) * y

end subroutine determine_new_vector

! get_next_step --
!     Set the next step in the simulated annealing process
!
! Arguments:
!     params              The structure holding the process parameters
!     range               Range for the solution parameters
!     x                   Present solution parameters
!     value               Function value at present solution
!     task                Task to be run
!
subroutine get_next_step( params, range, x, value, task )
    type(ANNEALING_PARAMETERS), intent(inout) :: params
    real(wp), dimension(:,:), intent(in)      :: range
    real(wp), dimension(:), intent(inout)     :: x
    real(wp), intent(inout)                   :: value
    integer, intent(inout)                    :: task

    !
    ! Initial stage: automatic scaling required?
    ! Then simply accumulate the function values
    !
    if ( task == annealing_init ) then
        task = annealing_value
        params%state = 0
        return
    endif

    if ( task == annealing_report ) then
        call determine_new_vector( range, x, idx, oldx )
        params%old_idx  = idx
        params%old_x    = oldx
        params%accepted = 0
        task            = annealing_value
        return
    endif

    if ( params%state == 0 ) then
        x                  = 0.5 * ( range(1,:) + range(2,:) )
        params%temperature = params%initial_temp
        if ( params%automatic_scaling ) then

            if ( params%iteration_count < params%number_iterations / 3 ) then
                params%scale_factor    = params%scale_factor    + value
                params%iteration_count = params%iteration_count + 1

                call determine_new_vector( range, x, idx, oldx )
                task = annealing_value
                return
            else
                params%scale_factor    = params%scale_factor    / &
                                         params%iteration_count
            endif
        endif
        params%state           = 1
        params%iteration_count = 0
        params%changes         = .true.
        params%accepted        = 0
    endif

    !
    ! Evaluate the function value and decide what to do now
    !
    call random_number( threshold )
    if ( (params%old_value - value)/(params%scale_factor *params%temperature) > &
            log(threshold) ) then
        params%old_value       = value
        params%changes         = .true.
        if ( params%iteration_count == 0 ) then
            params%accepted = 1
        else
            params%accepted = params%accepted + 1
        endif
    else
        value             = params%old_value
        x(params%old_idx) = params%old_x
    endif

    params%iteration_count = params%iteration_count + 1

    if ( params%iteration_count >= params%number_iterations ) then
        params%iteration_count = 0
        if ( params%changes ) then
            params%changes = .false.
            if ( params%verbose ) then
                task = annealing_report
            else
                task = annealing_value
            endif
            params%temperature = params%temperature * params%temp_reduction
        else
            task = annealing_done
        endif
    endif

    if ( task == annealing_value ) then
        call determine_new_vector( range, x, idx, oldx )
        params%old_idx = idx
        params%old_x   = oldx
    endif

end subroutine get_next_step

! find_minimum --
!     Simple routine implementing the simulated annealing procedure
!
! Arguments:
!     params              The structure holding the process parameters
!     range               Range for the solution parameters
!     x                   Initial and final solution parameters
!     func                Function to be minimised
!     value               Function value at present solution
!
! Note:
!     This assumes that the parameters have already been set!
!
subroutine find_minimum( params, range, x, func, value )
    type(ANNEALING_PARAMETERS), intent(inout) :: params
    real(wp), dimension(:,:), intent(in)      :: range
    real(wp), dimension(:), intent(inout)     :: x
    real(wp), intent(out)                     :: value

    interface
        function func( x )
            use select_precision
            real(wp), dimension(:), intent(in) :: x
            real(wp)                           :: func
        end function
    end interface

    integer :: task
    !integer :: count = 0

    task = annealing_init

    do
        call get_next_step( params, range, x, value, task )
        !write(*,*) task, x, value

        select case ( task )
            case ( annealing_value )
                !
                ! Fill in the evaluation of the function
                !
                value = func(x)

            case ( annealing_report )
                !
                ! Fill in the reporting code
                !
                write(*,'(a,e12.4)')      'Value so far: ', value
                write(*,'(a,(5e12.4),/)') '    Vector:   ', x
                write(*,'(2(a,i5))')     '    Accepted: ', &
                    params%accepted, ' from ', params%number_iterations

            case ( annealing_done )
                exit
        end select
        !count = count + 1; if ( count > 10 ) exit
    enddo

end subroutine find_minimum

end module simulated_annealing
