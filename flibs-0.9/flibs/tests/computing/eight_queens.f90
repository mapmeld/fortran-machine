! eight_queens.f90 --
!    Program to solve the classical problem of positioning
!    eight queens on a chessboard so that none can reach
!    another in one move.
!
!    This illustrates the use of the backtracking module.
!
module backtracking_data
    implicit none
    type SOLUTION_DATA
        integer, dimension(8) :: row
    end type SOLUTION_DATA

end module backtracking_data

module eight_queens
    use backtracking_data
    implicit none

    type(SOLUTION_DATA), dimension(:), pointer  :: all_solutions
contains

! generate --
!     Subroutine to generate the possible solutions at each stage
! Arguments:
!     base        The basic solution that new solutions will be
!                 derived from
!     stage       Index of the current stage in the construction
!     solutions   Array of new solutions that need to be examined
! Note:
!     The solutions that are returned in the (allocated)
!     array are simply feasible solutions that need to
!     be inspected further.
!     To keep the memory management clear, the subroutine
!     "free" cleans up the array of feasible solutions.
!
subroutine generate( base, stage, solutions )
    type(SOLUTION_DATA), intent(in)             :: base
    integer, intent(in)                         :: stage
    type(SOLUTION_DATA), dimension(:), pointer  :: solutions

    integer :: i

    !
    ! If stage <= 8, there are 8 possible solutions
    !
    if ( stage <= 8 ) then
        allocate( solutions(8) )
    else
        nullify( solutions )
        return
    endif

    !
    ! Construct them (be sure to copy the base
    ! solution first!)
    !
    solutions(:)            = base
    solutions(:)%row(stage) = (/ (i, i=1,8 ) /)
end subroutine generate

! free --
!     Subroutine to clean up the memory used by the intermediate
!     solutions
! Arguments:
!     solutions   Array of solutions that need to be cleaned up
!
subroutine free( solutions )
    type(SOLUTION_DATA), dimension(:), pointer  :: solutions

    deallocate( solutions )

end subroutine free

! acceptable --
!     Function to check if a possible solution is acceptable
! Arguments:
!     solution    The solution to be examined
!     stage       The current stage
! Returns:
!     .true. if the solution is acceptable, .false. otherwise
!
logical function acceptable( solution, stage )
    type(SOLUTION_DATA), intent(in) :: solution
    integer, intent(in)             :: stage

    logical :: clash
    integer  :: i
    integer  :: diff

    !
    ! We need to check the current queen only - all others
    ! have already been checked.
    !
    ! So:
    ! The new queen should not be on the same row as any others
    !
    clash = any( solution%row(1:stage-1) .eq. solution%row(stage) )

    !
    ! Check the diagonals
    !
    do i = 1,stage-1
        diff = abs( solution%row(i)-solution%row(stage) )
        clash = clash .or. ( diff .eq. (stage-i) )
    enddo

    acceptable = .not. clash

end function acceptable

! collect --
!     Subroutine to collect all solutions (or do something else)
! Arguments:
!     solution    Solution that was found
!     stop        Flag that indicates whether to stop or not
!
subroutine collect( solution, stop )
    type(SOLUTION_DATA), intent(in) :: solution
    logical, intent(out)            :: stop

    type(SOLUTION_DATA), dimension(:), pointer  :: new

    stop = .false. ! We want to collect all solutions

    allocate( new(size(all_solutions)+1) )

    new(1:size(all_solutions)) = all_solutions
    new(size(new))             = solution

    deallocate( all_solutions )
    all_solutions => new

end subroutine collect

end module eight_queens

program eightq

    use eight_queens
    use backtracking
    implicit none

    integer                           :: i
    integer                           :: nostage
    type(SOLUTION_DATA), dimension(1) :: base

    !
    ! There is no need to initialise the base solution in this example
    ! so just move on!
    !
    allocate( all_solutions(0) ) ! To be on the safe side

    nostage = 8

    call bt_solve( base, nostage, generate, acceptable, free, collect )

    !
    ! Now print the solutions ...
    !
    write(*,*) 'Number of solutions found: ', size(all_solutions)
    do i = 1,size(all_solutions)
        write(*,'(8i3)' ) all_solutions(i)%row
    enddo
end program
