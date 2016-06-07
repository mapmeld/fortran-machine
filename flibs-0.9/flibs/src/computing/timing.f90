! timing.f90 --
!     Module for timing operations:
!     - create a timer
!     - start the timer, run some operation
!     - stop the timer
!     This will automatically register how much
!     "real" time was spent and how much CPU time
!     Notes:
!     - Routines are available for a simple report
!     - To keep the code simple, there is a fixed
!       number of timers (MAX_TIMERS). This way
!       writing the report is very simple
!     - The following items are recorded:
!       - total "real" time
!       - total CPU time
!       - total number of iterations
!       - minimum and maximum "real" time intervals
!         for a single iteration
!
!     $Id: timing.f90,v 1.3 2007/01/26 09:56:43 arjenmarkus Exp $
!
module timing
    implicit none

    type TIMER
        integer           :: start_clock
        real              :: start_cpu
        real              :: total_sys_time
        real              :: total_cpu_time
        real              :: min_sys_time
        real              :: max_sys_time
        integer           :: number_iterations
        character(len=40) :: name
    end type

    private :: TIMER
    integer, parameter, private                       :: max_timers = 100
    logical, private, save                            :: init_timers = .true.
    type(TIMER), dimension(max_timers), save, private :: timers

contains

! timer_create
!     Create a new timer
!
! Arguments:
!     name          Name of the timer (for the report)
!     timerid       ID of the timer (-1: no more timers)
!
subroutine timer_create( name, timerid )
    character(len=*), intent(in)    :: name
    integer, intent(out)            :: timerid

    integer                         :: i

    if ( init_timers ) then
        init_timers = .false.
        timers(1:max_timers)%number_iterations = -1
    endif

    timerid = -1
    do i = 1,max_timers
        if ( timers(i)%number_iterations .eq. -1 ) then
            timerid = i
            timers(i)%name = name
            call timer_reset( timerid )

            exit
        endif
    enddo
end subroutine timer_create

! timer_destroy
!     Destroy a timer (make the entry available for a new name)
!
! Arguments:
!     timerid       ID of the timer
!
subroutine timer_destroy( timerid )
    integer, intent(in)             :: timerid

    if ( timerid .ge. 1 .and. timerid .le. max_timers ) then
        timers(timerid)%number_iterations = -1
    endif
end subroutine timer_destroy

! timer_reset
!     Reset a timer
!
! Arguments:
!     timerid       ID of the timer
!
subroutine timer_reset( timerid )
    integer, intent(in)             :: timerid

    if ( timerid .ge. 1 .and. timerid .le. max_timers ) then
        timers(timerid)%number_iterations = 0
        timers(timerid)%start_clock       = 0
        timers(timerid)%start_cpu         = 0
        timers(timerid)%total_sys_time    = 0.0
        timers(timerid)%total_cpu_time    = 0.0
        timers(timerid)%min_sys_time      = huge(1.0)
        timers(timerid)%max_sys_time      = 0.0
    endif
end subroutine timer_reset

! timer_report
!     Write a report on the timers
!
! Arguments:
!     lun           LU-number to write to (-1: to screen)
!
subroutine timer_report( lun )
    integer, intent(in)             :: lun

    integer                         :: i

    if ( lun .gt. 0 ) then
        write( lun, '(1x,a)' ) 'Timing report:'
        write( lun, '(1x,a)' ) &
        'Timer                                    Mean real time  Mean CPU time        Minimum        Maximum',&
        '----------------------------------------            (s)            (s)            (s)            (s)'
        do i = 1,max_timers
            if ( timers(i)%number_iterations .gt. 0 ) then
                write( lun, '(1x,a,4e15.4)' ) &
                    timers(i)%name, &
                    timers(i)%total_sys_time / timers(i)%number_iterations, &
                    timers(i)%total_cpu_time / timers(i)%number_iterations, &
                    timers(i)%min_sys_time, timers(i)%max_sys_time
            endif
        enddo
        write( lun, '(/)' )
    else
        write( *, '(1x,a)' ) 'Timing report:'
        write( *, '(1x,a)' ) &
        'Timer                                    Mean real time  Mean CPU time        Minimum        Maximum',&
        '----------------------------------------            (s)            (s)            (s)            (s)'
        do i = 1,max_timers
            if ( timers(i)%number_iterations .gt. 0 ) then
                write( *, '(1x,a,4e15.4)' ) &
                    timers(i)%name, &
                    timers(i)%total_sys_time / timers(i)%number_iterations, &
                    timers(i)%total_cpu_time / timers(i)%number_iterations, &
                    timers(i)%min_sys_time, timers(i)%max_sys_time
            endif
        enddo
        write( *, '(/)' )
    endif
end subroutine timer_report

! timer_start
!     Register the start time
!
! Arguments:
!     timerid       ID of the timer
!
subroutine timer_start( timerid )
    integer, intent(in)             :: timerid

    integer                         :: count
    integer                         :: count_scale
    integer                         :: count_max

    if ( timerid .ge. 1 .and. timerid .le. max_timers ) then
        call cpu_time( timers(timerid)%start_cpu )
        call system_clock( count, count_scale, count_max )
        timers(timerid)%start_clock = count
    endif
end subroutine timer_start

! timer_stop
!     Register the stop time
!
! Arguments:
!     timerid       ID of the timer
!     noiter        Number of iterations (defaults to 1)
!                   the elapsed period represents
!
subroutine timer_stop( timerid, noiter )
    integer, intent(in)             :: timerid
    integer, intent(in), optional   :: noiter

    integer                         :: noiter_
    integer                         :: count
    integer                         :: count_scale
    integer                         :: count_max
    real                            :: stop_time
    real                            :: period

    if ( timerid .ge. 1 .and. timerid .le. max_timers ) then
        call cpu_time( stop_time )
        call system_clock( count, count_scale, count_max )

        noiter_ = 1
        if ( present(noiter) ) noiter_ = noiter

        period = (real(count-timers(timerid)%start_clock))/real(count_scale)
        timers(timerid)%total_sys_time = timers(timerid)%total_sys_time + period
        timers(timerid)%total_cpu_time = timers(timerid)%total_cpu_time + &
            stop_time - timers(timerid)%start_cpu
        timers(timerid)%number_iterations = timers(timerid)%number_iterations + noiter_

        period = period / noiter_
        timers(timerid)%min_sys_time = min( timers(timerid)%min_sys_time, period )
        timers(timerid)%max_sys_time = max( timers(timerid)%max_sys_time, period )
    endif
end subroutine timer_stop

end module timing
