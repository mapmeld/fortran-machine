! streeter.f90 --
!     Module to compute the minimum oxygen concentration
!     Belongs to the example.tbl file
!
module streeter_phelps
contains

! compute_min_oxygen --
!     Compute the minimum oxygen concentration based on the
!     Streeter-Phelps BOD-DO model
!
! Arguments:
!     bod             Initial BOD concentration (mg O2/l)
!     oxy             Initial oxygen (DO) concentration (mg O2/l)
!     k               Decay coefficient BOD (1/day)
!     ka              Reaeration coefficient oxygen (m/day)
!     h               Depth of the river (m)
!     oxysat          Oxygen saturation concentration (mg O2/l)
!     dt              Timestep (day)
!     oxymin          Minimum oxygen concentration (mg O2/l; output)
!     time            Time at which it occurs (day; output)
!
! Note:
!     Error conditions: k <= 0, ka <= 0, dt <= 0. Then oxymin and time
!     are set to -999.0
!
subroutine compute_min_oxygen( bod, oxy, k, ka, h, oxysat, dt, oxymin, time )
    real, intent(in)  :: bod
    real, intent(in)  :: oxy
    real, intent(in)  :: k
    real, intent(in)  :: ka
    real, intent(in)  :: h
    real, intent(in)  :: oxysat
    real, intent(in)  :: dt
    real, intent(out) :: oxymin
    real, intent(out) :: time

    real              :: timemax
    real              :: t
    real              :: dbod
    real              :: doxy
    real              :: bodn
    real              :: oxyn

    if ( k <= 0.0 .or. ka <= .0 .or. dt <= 0.0 ) then
        time   = -999.0
        oxymin = -999.0
        return
    endif

    oxymin  = oxy
    time    = 0.0

    bodn    = bod
    oxyn    = oxy

    timemax = 10.0 / k ! More than safe time interval

    do while ( t < timemax )
        dbod = -k * bodn
        doxy = -k * bodn + ka * ( oxysat - oxyn ) / h

        t    = t    + dt
        bodn = bodn + dbod *dt
        oxyn = oxyn + doxy *dt

        if ( oxyn < oxymin ) then
            oxymin = oxyn
            time   = t
        endif
    enddo

end subroutine compute_min_oxygen
end module streeter_phelps
