! genetic_algorithms.f90 --
!     Basic framework for genetic algorithms
!
!     $Id: genetic_algorithms.f90,v 1.2 2008/10/02 09:02:33 arjenmarkus Exp $
!
module genetic_algorithms

    use select_precision

    implicit none

    integer, parameter :: ga_init     = 0
    integer, parameter :: ga_value    = 1
    integer, parameter :: ga_report   = 2
    integer, parameter :: ga_generate = 3
    integer, parameter :: ga_done     = 4

    type GA_PARAMETERS
        real(wp) :: mutation_chance
        real(wp) :: crossover_chance
        real(wp) :: optimum
        integer  :: number_iterations
        integer  :: number_descendants
        integer  :: number_parents
        integer  :: iteration_count
        integer  :: descendant
        logical  :: verbose
        integer, dimension(:), pointer   :: idx   => null()
        integer, dimension(:,:), pointer :: gene  => null()
        real(wp), dimension(:), pointer  :: value => null()
    endtype GA_PARAMETERS

contains

! set_parameters --
!     Initialise or update the genetic algorithm parameters
!
! Arguments:
!     params              The structure holding the parameters
!     update              If true, only update the parameters in the argument list
!                         otherwise re-initialise
!     number_descendants  Number of descendants
!     number_parents      Number of parents
!     number_iterations   (Maximum) number of iterations
!     mutation_chance     Chance for an individual gene to mutate
!     crossover_chance    Chance for a gene to split and rejoin
!     verbose             If true, print
!
subroutine set_parameters( params, update, number_descendants, number_parents, &
                           number_iterations, mutation_chance, crossover_chance, verbose )
    type(GA_PARAMETERS), intent(inout) :: params
    logical, intent(in)                :: update
    integer, intent(in), optional      :: number_descendants
    integer, intent(in), optional      :: number_parents
    integer, intent(in), optional      :: number_iterations
    real, intent(in), optional         :: mutation_chance
    real, intent(in), optional         :: crossover_chance
    logical, intent(in), optional      :: verbose

    if ( .not. update ) then
        params%number_descendants = 100
        params%number_parents     = 10
        params%number_iterations  = 100
        params%mutation_chance    = 0.1
        params%crossover_chance   = 0.1
        params%verbose            = .false.
    endif

    if ( present(number_descendants) ) then
        params%number_descendants = number_descendants
    endif

    if ( present(number_parents) ) then
        params%number_parents = number_parents
    endif

    if ( present(number_iterations) ) then
        params%number_iterations  = number_iterations
    endif

    if ( present(mutation_chance) ) then
        params%mutation_chance  = mutation_chance
    endif

    if ( present(crossover_chance) ) then
        params%crossover_chance  = crossover_chance
    endif

    if ( present(verbose) ) then
        params%verbose            = verbose
    endif
end subroutine set_parameters

! sort_by_value --
!     Sort the index of the population members by their index
!
! Arguments:
!     params              GA parameters
!
subroutine sort_by_value( params )
    type(GA_PARAMETERS), intent(inout)     :: params

    logical                                :: exchanged
    integer                                :: tmp
    integer                                :: i
    integer                                :: j

    !
    ! Sort using a simple bubblesort
    !
    do j = 1,params%number_descendants
        do i = j+1,params%number_descendants
            if ( params%value(params%idx(j)) > params%value(params%idx(i)) ) then
                tmp = params%idx(j)
                params%idx(j) = params%idx(i)
                params%idx(i) = tmp
            endif
        enddo
    enddo

end subroutine sort_by_value

! retrieve_member --
!     Retrieve the gene values of a particular member of the (sorted) population
!
! Arguments:
!     params              GA parameters
!     range               Range for the solution parameters
!     idx                 Index of the member
!     x                   Gene values
!     value               Value of the function for that member
!
subroutine retrieve_member( params, range, idx, x, value )
    type(GA_PARAMETERS), intent(in)        :: params
    real(wp), dimension(:,:), intent(in)   :: range
    integer, intent(in)                    :: idx
    real(wp), dimension(:), intent(out)    :: x
    real(wp), intent(out)                  :: value

    integer                                :: idm

    idm   = params%idx(idx)

    x     = range(1,:) + params%gene(:,idm) * (range(2,:)-range(1,:)) / huge(1)
    value = params%value(idm)

end subroutine retrieve_member

! random_index --
!     Generate a random index
!
! Arguments:
!     maxidx              Maximum index
!     idx                 New index
!
subroutine random_index( maxidx, idx )
    integer, intent(in)                       :: maxidx
    integer, intent(out)                      :: idx

    real(wp)                                  :: y

    call random_number( y )
    idx = min( 1 + maxidx * y, maxidx )

end subroutine random_index

! create_descendant --
!     Create a new descendant by selecting two parents and combining
!     their genes
!
! Arguments:
!     params              GA parameters
!     idx                 Index for the new descendant
!
subroutine create_descendant( params, idx )
    type(GA_PARAMETERS), intent(inout)        :: params
    integer, intent(in)                       :: idx

    integer                                   :: i
    integer                                   :: idm
    integer                                   :: idbit
    integer                                   :: p1
    integer                                   :: p2
    real(wp)                                  :: y
    integer                                   :: new_gene

    idm = params%idx(idx) ! No need to reorder the genes this way

    !
    ! Select the two parents
    !
    call random_index( params%number_parents, p1 )
    call random_index( params%number_parents, p2 )

    p1 = params%idx(p1)
    p2 = params%idx(p2)

    !
    ! Combine the genes (also implement mutation and cross-over here)
    !
    do i = 1,size(params%gene,1)
        call random_number( y )
        if ( y >= 0.5 ) then
            new_gene = params%gene(i,p1)
        else
            new_gene = params%gene(i,p2)
        endif

        call random_number( y )
        if ( y <= params%mutation_chance ) then
            call random_number( y )
            idbit = 31 + log(y)/log(2.0) ! Select the more important bits most of the time
            if ( btest( new_gene, idbit ) ) then
                new_gene = ibclr( new_gene, idbit )
            else
                new_gene = ibset( new_gene, idbit )
            endif
        endif

        call random_number( y )
        if ( y <= params%crossover_chance ) then
            call random_index( 31, idbit )
            idbit = idbit - 1
            new_gene = params%gene(i,p2)
            call mvbits( params%gene(i,p1), idbit, 32-idbit-1, new_gene, idbit )
        endif

        params%gene(i,idm) = new_gene
    enddo
end subroutine create_descendant

! get_next_step --
!     Set the next step in the simulated ga process
!
! Arguments:
!     params              The structure holding the process parameters
!     range               Range for the solution parameters
!     x                   Present solution parameters
!     value               Function value at present solution
!     task                Task to be run
!
subroutine get_next_step( params, range, x, value, task )
    type(GA_PARAMETERS), intent(inout)        :: params
    real(wp), dimension(:,:), intent(in)      :: range
    real(wp), dimension(:), intent(inout)     :: x
    real(wp), intent(inout)                   :: value
    integer, intent(inout)                    :: task

    real(dp)                                  :: y    ! Double precision required
    integer                                   :: i
    integer                                   :: j

    !
    ! Initial stage: automatic scaling required?
    ! Then simply accumulate the function values
    !
    if ( task == ga_init ) then
        if ( associated(params%idx) ) then
            deallocate( params%idx )
        endif
        if ( associated(params%value) ) then
            deallocate( params%value )
        endif
        if ( associated(params%gene) ) then
            deallocate( params%gene )
        endif

        allocate( params%idx(params%number_descendants) )
        allocate( params%value(params%number_descendants) )
        allocate( params%gene(size(range,2),params%number_descendants) )

        do i = 1,params%number_descendants
            do j = 1,size(range,2)
                call random_number( y )
                params%gene(j,i) = huge(1) * y
            enddo
            params%value(i) = huge(1.0)
            params%idx(i)   = i

            call retrieve_member( params, range, i, x, value )
        enddo

        task = ga_value
        params%descendant = 1
        params%optimum    = huge(1.0)
        call retrieve_member( params, range, params%descendant, x, value )
       !write(*,*) 'Compute value for: ', params%descendant
        return
    endif

    if ( task == ga_report ) then
        task = ga_generate
    endif

    if ( task == ga_value ) then
        !
        ! First step: determine the function value for each member
        !
       !write(*,'(a,2i12,f10.2)') 'Descendant: ', params%descendant, &
       !    params%gene(1,params%idx(params%descendant)), value
        params%value(params%idx(params%descendant)) = value

        params%descendant = params%descendant + 1
        if ( params%descendant <= params%number_descendants ) then
            call retrieve_member( params, range, params%descendant, x, value )
            return
        else
           !write(*,*) 'Reporting'
            if ( params%verbose ) then
                call retrieve_member( params, range, 1, x, value )
                task = ga_report
                return
            else
                task = ga_generate
            endif
        endif
    endif

    if ( task == ga_generate ) then
       !write(*,*) 'Generating'
        !
        ! Second step: sort by value to determine the new parents.
        ! Then check whether continuing makes sense
        !
        params%descendant      = params%number_parents + 1
        params%iteration_count = params%iteration_count + 1

        call sort_by_value( params )

        if ( params%iteration_count >= params%number_iterations ) then
           !write(*,*) 'Done - max iterations'
            call retrieve_member( params, range, 1, x, value )
            task = ga_done
            return
        endif

       !write(*,*) 'Optimum', params%iteration_count, params%optimum
       !do i = 1,5
       !    write(*,'(3i12,f10.2)') &
       !        (params%gene(j,params%idx(i)), j=1,3), params%value(params%idx(i))
       !enddo

        if ( params%value(params%idx(1)) >= params%optimum ) then
           !write(*,*) 'Done - optimum reached'
            call retrieve_member( params, range, 1, x, value )
            task = ga_done
            return
        else
            params%optimum = params%value(params%idx(1))
           !write(*,*) 'Optimum set:', params%optimum
        endif

       ! if ( params%verbose ) then
       !     write(*,*) 'Report task'
       !     task = ga_report
       !     return
       ! endif

        !
        ! Third step: generate the descendants (and keep the parents)
        !
        do i = params%number_parents+1, params%number_descendants
            call create_descendant( params, i )
        enddo
       !write(*,*) 'Next descendant:', params%descendant
        call retrieve_member( params, range, params%descendant, x, value )
        task = ga_value
    endif

end subroutine get_next_step

! find_minimum --
!     Simple routine implementing the simulated ga procedure
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
    type(GA_PARAMETERS), intent(inout)        :: params
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

    task = ga_init

    do
        call get_next_step( params, range, x, value, task )
        !write(*,*) task, x, value

        select case ( task )
            case ( ga_value )
                !
                ! Fill in the evaluation of the function
                !
                value = func(x)

            case ( ga_report )
                !
                ! Fill in the reporting code
                !
                write(*,'(a,e12.4)')      'Value so far: ', value
                write(*,'(a,(5e12.4),/)') '    Vector:   ', x

            case ( ga_done )
                exit
        end select
        !count = count + 1; if ( count > 10 ) exit
    enddo

end subroutine find_minimum

end module genetic_algorithms
