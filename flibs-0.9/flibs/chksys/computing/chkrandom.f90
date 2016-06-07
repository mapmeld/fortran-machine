! chkrandom.f90 --
!     Program to check a few properties of the random_number and
!     random_seed subroutines
!
!     Note:
!     The program must be called twice in succession
!

module random_numbers
contains
!
! Derived from the documentation of gfortran:
! A standard-conforming routine to initialise the RNG
!
! init_random_seed --
!     Reset the random number generator via the system clock or via
!     a predefined sequence
!
! Arguments:
!     use_time         (Optional) Use the system time or not
!
subroutine init_random_seed( use_time )
    implicit none
    logical, intent(in), optional :: use_time

    integer                            :: i
    integer                            :: seed_size
    integer, dimension(:), allocatable :: seed
    integer                            :: clock = 305721054

    call random_seed( size = seed_size )
    allocate( seed(seed_size) )

    call random_seed( get = seed )

    if ( use_time ) then
        call system_clock( count = clock )
    endif

    seed = clock + 37 * (/ ( i-1, i = 1,seed_size ) /)

    call random_seed( put = seed )

    deallocate( seed )
end subroutine
end module

program chkrandom
    implicit none
    real                               :: r
    real                               :: rsav
    integer                            :: seed_size
    integer, allocatable, dimension(:) :: seed_data
    integer, allocatable, dimension(:) :: seed_save
    logical                            :: exists

    call system_clock( seed_size )
    write(*,*) 'Clock: ', seed_size

    !
    ! Check:
    ! Is the sequence always the same or not?
    !
    inquire( file = 'chkrandom.sav', exist = exists )
    if ( exists ) then
        open( 10, file = 'chkrandom.sav' )
        read( 10, * ) rsav
        close( 10 )

        call random_number( r )

        if ( abs(r-rsav) < 1.0e-5 ) then
            write(*,*) 'The random number generator is initialised to the same sequence'
        else
            write(*,*) 'The random number generator uses a different sequence at each start-up'
        endif
    else

        write(*,*) 'Simple checks on random number generator'
        write(*,*) '----------------------------------------'

        call random_number( r )
        open( 10, file = 'chkrandom.sav' )
        write( 10, '(f12.8)' ) r
        close( 10 )
    endif

    !
    ! Inspect:
    ! Seed for the random number generator
    !
    call random_seed( size = seed_size )

    allocate( seed_data(seed_size), seed_save(seed_size) )
    call random_seed( get = seed_save )

    !
    ! This call might reset the RNG to a different sequence
    !
    call random_seed

    call random_seed( get = seed_data )

    write(*,*) 'Size of the seed to the random number generator:', seed_size

    write(*,*) 'Effect of call to RANDOM_SEED without arguments:'
    if ( all( seed_data == seed_save ) ) then
        write(*,*) '   RNG is reset to standard sequence'
    else
        write(*,*) '   RNG is reset to a different sequence each time'
    endif

end program
