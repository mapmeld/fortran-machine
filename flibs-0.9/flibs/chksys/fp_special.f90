! fp_special.f90 --
!     Investigate the behaviour of the program vis-a-vis special
!     IEEE numbers and floating-point exceptions
!
!     Note:
!     To get full insight, the program may need to run several times
!     successively skipping tests.
!
program fp_special
    implicit none

    integer :: test, success
    logical :: exists

    real    :: y = 0.0
    real    :: z = 1.0e30

    ! Start afresh? If the file "fp_special.done" exists or the
    ! file "fp_special.next' does not.
    !
    test = -1

    inquire( file = "fp_special.done", exist = exists )
    if ( exists ) then
        open( 10, file = "fp_special.done" )
        close( 10, status = 'delete' )
        test = 1
    else
        inquire( file = "fp_special.next", exist = exists )
        if ( .not. exists ) then
            test = 1
        endif
    endif

    open( 10, file = "fp_special.next" )
    if ( test == -1 ) then
        read( 10, * ) test, success
        rewind( 10 )
        if ( success /= 1 ) then
            write(*,*) 'Previous test failed! The program aborted'
        endif
    endif

    !
    ! Select the next test
    !
    if ( test == 1 ) then
        write( 10, * ) test, 0
        rewind( 10 )
        call division_by_zero
        test = 2
    endif

    if ( test == 2 ) then
        write( 10, * ) test, 0
        rewind( 10 )
        call not_a_number
        test = 3
    endif

    if ( test == 3 ) then
        write( 10, * ) test, 0
        rewind( 10 )
        call overflow
        test = 4
    endif

    if ( test == 4 ) then
        write( 10, * ) test, 0
        rewind( 10 )
        call underflow
    endif

    call negative_zero

    close( 10 )
    open( 10, file= "fp_special.done" )
    write( 10, * ) 'Done'
    close( 10 )

contains

! division_by_zero --
!     Does the compiler support division by zero? Or does the program abort?
!
! Arguments:
!     None
!
subroutine division_by_zero

    real :: x

    write( 10, * ) test+1, 0

    write(*,*) 'Test: division by zero'

    x = 1.0 / y

    write(*,*) '      Test succeeded, the program continues'
    write(*,*) '      Result of 1/0: ', x
    if ( x > huge(1.0) ) then
        write(*,*) '      Result is "infinity"'
    else
        write(*,*) '      Result is "huge" - it should be "infinity"'
    endif

    write( 10, * ) test+1, 1

end subroutine

! not_a_number --
!     Does the compiler support not-a-numbers? Or does the program abort?
!
! Arguments:
!     None
!
subroutine not_a_number

    real :: x

    write( 10, * ) test+1, 0

    write(*,*) 'Test: creating Not-a-number'

    x = 0.0 / y

    write(*,*) '      Test succeeded, the program continues'
    write(*,*) '      Result of 0/0: ', x
    if ( x >= 0.0 .or. x <= 0.0 ) then
        write(*,*) '      Result is 0.0 - it should be "NaN"'
    else
        write(*,*) '      Result is "NaN"'
    endif

    write( 10, * ) test+1, 1

end subroutine

! overflow --
!     Does the compiler support overflow? Or does the program abort?
!
! Arguments:
!     None
!
subroutine overflow

    real :: x

    write( 10, * ) test+1, 0

    write(*,*) 'Test: computations with overflow'

    x = 1.0e20 * z

    write(*,*) '      Test succeeded, the program continues'
    write(*,*) '      Result of 1.0e20 * 1.0e30: ', x
    if ( x > huge(1.0) ) then
        write(*,*) '      Result is "Infinity"'
    else
        write(*,*) '      Result is "huge"'
    endif

    write( 10, * ) test+1, 1

end subroutine

! underflow --
!     Does the compiler support underflow (denormalised numbers)?
!
! Arguments:
!     None
!
subroutine underflow

    real :: x
    real :: xprev

    write( 10, * ) test+1, 0

    write(*,*) 'Test: computations with underflow'

    x = 1.0
    do while ( x > tiny(x) )
        xprev = x
        x     = x / 2.0
    enddo

    write(*,*) '      Test succeeded, the program continues'
    write(*,*) '      Result of repeated halving: ', x
    if ( x == 0.0 ) then
        write(*,*) '      Note: no underflow - small values are truncated to zero'
    else
        write(*,*) '      Small values will gradually go to zero'
    endif

    write( 10, * ) test+1, 1

end subroutine

! negative_zero --
!     Does the compiler support negative zeros?
!
! Arguments:
!     None
!
subroutine negative_zero

    logical :: supported
    real :: neg_zero

    supported = .false.

    neg_zero = -0.0
    if ( sign(1.0,neg_zero) < 0.0 ) then
        supported = .true.
    else
        neg_zero = -tiny(1.0)

        do while ( neg_zero /= 0 )
            neg_zero = neg_zero / 2.0
        enddo
        if ( sign(1.0,neg_zero) < 0.0 ) then
            supported = .true.
        endif
    endif

    if ( supported ) then
        write(*,*) 'Negative zeros are supported'
    else
        write(*,*) 'Negative zeros are NOT supported'
    endif

    neg_zero = sqrt( -0.0 )

    if ( sign(1.0,neg_zero) < 0.0 ) then
        write(*,*) 'SQRT(-0) is indeed negative (as required by the IEEE standard)'
    else
        write(*,*) 'SQRT(-0) is NOT negative! Inconsistent with the IEEE standard'
    endif

end subroutine

end program
