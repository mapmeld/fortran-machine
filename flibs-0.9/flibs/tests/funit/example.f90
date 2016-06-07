! Test program
program test_table
    implicit none
    integer, parameter :: wp = kind(1.0)
    integer, parameter :: niters_ = 200
    logical :: error_recognised_
    integer :: luout_
    integer :: lutbl_
    integer :: error_
    real(kind=wp) :: expected_oxymin, min_oxymin, max_oxymin
    real(kind=wp) :: expected_time, min_time, max_time

    error_ = 0
    luout_ = 10
    lutbl_ = 11
    open( luout_, file = 'report.out' )
    open( lutbl_, file = 'table.out' )
    write(*,*) 'Running random tests ...'
    call run_ranges
    write(*,*) 'Running specific tests ...'
    call all_tests
    write(luout_,'(a,i5)') 'All tests completed. Number of errors:',error_
    write(*,*) 'Done'
contains

logical function not_equal_abs( x, y, margin )
    real(kind=wp) :: x, y, margin
    not_equal_abs = abs(x-y) > margin
end function not_equal_abs

logical function not_equal_rel( x, y, margin )
    real(kind=wp) :: x, y, margin
    not_equal_rel = abs(x-y) > 0.5 * margin * (abs(x)+abs(y))
end function not_equal_rel

real function random_uniform( xmin, xmax )
    real(kind=wp) :: xmin, xmax
    real(kind=wp) :: r

    call random_number( r )
    random_uniform = xmin + (xmax-xmin) * r
end function random_uniform

real function random_normal( xmean, xstdev )
    real(kind=wp) :: xmean, xstdev
    real(kind=wp) :: r, phi

    call random_number( r )
    call random_number( phi )
    phi = 8.0_wp * atan(1.0_wp) * phi
    r   = sqrt( -2.0_wp * log(r) )
    random_normal = xmean + xstdev * r * cos(phi)
end function random_normal

subroutine recognised_error
    error_recognised_ = .true.
    write(luout_,'(a)') '    Note: error condition correctly recognised'
end subroutine recognised_error

subroutine unexpected_error
    write(luout_,'(a)') '    Note: unexpected error detected'
    error_ = error_ + 1
end subroutine unexpected_error

subroutine all_tests
    use streeter_phelps
    real :: bod, oxy
    real :: k, ka, h, oxysat, dt, oxymin, time

    write(lutbl_,'(100a12)') 'dt','oxy','bod','oxysat','h','k','ka','oxymin','time'

    write(luout_,'(a)') 'Test case: 1'
    dt = 0.1
    oxy = 10
    bod = 1
    oxysat = 10
    h = 10
    k = 0.1
    ka = 1.0
    expected_oxymin = 10.0
    expected_time = 2.0

    call compute_min_oxygen( bod, oxy, k, ka, h, oxysat, dt, oxymin, time )

    write(lutbl_,'(100g12.4)') dt,oxy,bod,oxysat,h,k,ka,oxymin,time
    if ( not_equal_abs(oxymin,expected_oxymin,0.001_wp) ) then
        write(luout_,'(a,g12.4,a,g12.4)') &
            '    Error: oxymin = ',oxymin, ' - expected: ',expected_oxymin
        error_ = error_ + 1
    endif
    if ( not_equal_rel(time,expected_time,0.001_wp) ) then
        write(luout_,'(a,g12.4,a,g12.4)') &
            '    Error: time = ',time, ' - expected: ',expected_time
        error_ = error_ + 1
    endif


    write(luout_,'(a)') 'Test case: 2'
    dt = 0.1
    oxy = 10
    bod = 1
    oxysat = 10
    h = 10
    k = 0.1
    ka = 1.0
    expected_oxymin = 9.630
    expected_time = 9.9

    call compute_min_oxygen( bod, oxy, k, ka, h, oxysat, dt, oxymin, time )

    write(lutbl_,'(100g12.4)') dt,oxy,bod,oxysat,h,k,ka,oxymin,time
    if ( not_equal_abs(oxymin,expected_oxymin,0.001_wp) ) then
        write(luout_,'(a,g12.4,a,g12.4)') &
            '    Error: oxymin = ',oxymin, ' - expected: ',expected_oxymin
        error_ = error_ + 1
    endif
    if ( not_equal_rel(time,expected_time,0.001_wp) ) then
        write(luout_,'(a,g12.4,a,g12.4)') &
            '    Error: time = ',time, ' - expected: ',expected_time
        error_ = error_ + 1
    endif


    write(luout_,'(a)') 'Test case: 3'
    dt = 0.1
    oxy = 10
    bod = 1
    oxysat = 10
    h = 10
    k = 0.1
    ka = 1.0
    expected_oxymin = 9.632
    expected_time = 9.9

    call compute_min_oxygen( bod, oxy, k, ka, h, oxysat, dt, oxymin, time )

    write(lutbl_,'(100g12.4)') dt,oxy,bod,oxysat,h,k,ka,oxymin,time
    if ( not_equal_abs(oxymin,expected_oxymin,0.001_wp) ) then
        write(luout_,'(a,g12.4,a,g12.4)') &
            '    Error: oxymin = ',oxymin, ' - expected: ',expected_oxymin
        error_ = error_ + 1
    endif
    if ( not_equal_rel(time,expected_time,0.001_wp) ) then
        write(luout_,'(a,g12.4,a,g12.4)') &
            '    Error: time = ',time, ' - expected: ',expected_time
        error_ = error_ + 1
    endif


    write(luout_,'(a)') 'Test case: 4'
    dt = 0.1
    oxy = 10
    bod = 1
    oxysat = 10
    h = 10
    k = 0.1
    ka = 1.0
    expected_oxymin = 9.630
    expected_time = 9.8999

    call compute_min_oxygen( bod, oxy, k, ka, h, oxysat, dt, oxymin, time )

    write(lutbl_,'(100g12.4)') dt,oxy,bod,oxysat,h,k,ka,oxymin,time
    if ( not_equal_abs(oxymin,expected_oxymin,0.001_wp) ) then
        write(luout_,'(a,g12.4,a,g12.4)') &
            '    Error: oxymin = ',oxymin, ' - expected: ',expected_oxymin
        error_ = error_ + 1
    endif
    if ( not_equal_rel(time,expected_time,0.001_wp) ) then
        write(luout_,'(a,g12.4,a,g12.4)') &
            '    Error: time = ',time, ' - expected: ',expected_time
        error_ = error_ + 1
    endif


    write(luout_,'(a)') 'Test case: 5'
    dt = 1.0
    oxy = 10
    bod = 1
    oxysat = 10
    h = 10
    k = 0.1
    ka = 1.0
    expected_oxymin = 0.0  ! Undetermined
    expected_time = 0.0  ! Undetermined

    call compute_min_oxygen( bod, oxy, k, ka, h, oxysat, dt, oxymin, time )

    write(lutbl_,'(100g12.4)') dt,oxy,bod,oxysat,h,k,ka,oxymin,time

    write(luout_,'(a)') 'Test case: 6'
    dt = 0.0
    oxy = 0.0  ! Undetermined
    bod = 0.0  ! Undetermined
    oxysat = 0.0  ! Undetermined
    h = 0.0  ! Undetermined
    k = 0.0  ! Undetermined
    ka = 0.0  ! Undetermined
    expected_oxymin = 0.0  ! Undetermined
    expected_time = 0.0  ! Actually ERROR

    call compute_min_oxygen( bod, oxy, k, ka, h, oxysat, dt, oxymin, time )

    write(lutbl_,'(100g12.4)') dt,oxy,bod,oxysat,h,k,ka,oxymin,time
    error_recognised_ = .false.
    if ( time == -999.0 ) then
        call recognised_error
    endif

    if ( .not. error_recognised_ ) then
        write(luout_,'(a)') '    Failure: error not recognised'
    endif

    write(luout_,'(a)') 'Test case: 7'
    dt = 1.0
    oxy = 0.
    bod = 10
    oxysat = 10
    h = 10
    k = 0.1
    ka = 1.0
    expected_oxymin = 0.0  ! Undetermined
    expected_time = 0.0  ! Undetermined

    call compute_min_oxygen( bod, oxy, k, ka, h, oxysat, dt, oxymin, time )

    write(lutbl_,'(100g12.4)') dt,oxy,bod,oxysat,h,k,ka,oxymin,time
end subroutine all_tests

subroutine run_ranges
    use streeter_phelps
    real :: bod, oxy
    real :: k, ka, h, oxysat, dt, oxymin, time

    integer :: i_
    real(wp) :: min_oxymin = huge(1.0_wp), max_oxymin = -huge(1.0_wp)
    real(wp) :: min_time = huge(1.0_wp), max_time = -huge(1.0_wp)
    real(wp) :: min_dt = huge(1.0_wp), max_dt = -huge(1.0_wp)
    real(wp) :: min_oxy = huge(1.0_wp), max_oxy = -huge(1.0_wp)
    real(wp) :: min_bod = huge(1.0_wp), max_bod = -huge(1.0_wp)
    real(wp) :: min_oxysat = huge(1.0_wp), max_oxysat = -huge(1.0_wp)
    real(wp) :: min_h = huge(1.0_wp), max_h = -huge(1.0_wp)
    real(wp) :: min_k = huge(1.0_wp), max_k = -huge(1.0_wp)
    real(wp) :: min_ka = huge(1.0_wp), max_ka = -huge(1.0_wp)

    write(lutbl_,'(100a12)') &
'dt','oxy','bod','oxysat','h','k','ka','oxymin','time'

    do i_ = 1,niters_
        dt = 0.1
        if ( dt < min_dt ) min_dt = dt
        if ( dt > max_dt ) max_dt = dt
        oxy = random_uniform(9.0,11.0)
        if ( oxy < min_oxy ) min_oxy = oxy
        if ( oxy > max_oxy ) max_oxy = oxy
        bod = random_uniform(7.5,12.5)
        if ( bod < min_bod ) min_bod = bod
        if ( bod > max_bod ) max_bod = bod
        oxysat = random_uniform(7.5,12.5)
        if ( oxysat < min_oxysat ) min_oxysat = oxysat
        if ( oxysat > max_oxysat ) max_oxysat = oxysat
        h = 10.0
        if ( h < min_h ) min_h = h
        if ( h > max_h ) max_h = h
        k = random_uniform(0.3,0.7)
        if ( k < min_k ) min_k = k
        if ( k > max_k ) max_k = k
        ka = random_uniform(0.3,0.7)
        if ( ka < min_ka ) min_ka = ka
        if ( ka > max_ka ) max_ka = ka

            call compute_min_oxygen( bod, oxy, k, ka, h, oxysat, dt, oxymin, time )
            if ( time == -999.0 ) then
        call recognised_error
    endif
        if ( oxymin < min_oxymin ) min_oxymin = oxymin
        if ( oxymin > max_oxymin ) max_oxymin = oxymin
        if ( time < min_time ) min_time = time
        if ( time > max_time ) max_time = time
            call compute_min_oxygen( bod, oxy, k, ka, h, oxysat, dt, oxymin, time )

        write(lutbl_,'(100g12.4)') &
dt,oxy,bod,oxysat,h,k,ka,oxymin,time
    enddo
    write(luout_,'(a)') 'Overview of iterations:'
    write(luout_,'(a20,2g12.4)') 'dt', min_dt, max_dt
    write(luout_,'(a20,2g12.4)') 'oxy', min_oxy, max_oxy
    write(luout_,'(a20,2g12.4)') 'bod', min_bod, max_bod
    write(luout_,'(a20,2g12.4)') 'oxysat', min_oxysat, max_oxysat
    write(luout_,'(a20,2g12.4)') 'h', min_h, max_h
    write(luout_,'(a20,2g12.4)') 'k', min_k, max_k
    write(luout_,'(a20,2g12.4)') 'ka', min_ka, max_ka
    write(luout_,'(a20,2g12.4)') 'oxymin', min_oxymin, max_oxymin
    write(luout_,'(a20,2g12.4)') 'time', min_time, max_time

end subroutine run_ranges

end program test_table
