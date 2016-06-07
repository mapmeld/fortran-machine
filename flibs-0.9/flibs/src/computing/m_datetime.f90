! datetime.f90 --
!     Basic module for date/time computations:
!     - the date and time are represented as the duration since an epoch
!       time. The implementation uses the number of days and
!       milliseconds in the day. (Leapseconds are ignored)
!     - Conversions to and from the Gregorian calendar
!     - Formatting and scanning
!     - No timezones or treatment of the historical aberrations
!       in the calendar

TODO: references

!
!     $Id: timing.f90,v 1.3 2007/01/26 09:56:43 arjenmarkus Exp $
!
module date_manipulation
    implicit none

    public

    type DATETIME
        private
        integer           :: days
        integer           :: msecs
    end type DATETIME

TODO: +, -
TODO: negative durations? msecs > 86400000 or < 0?

contains

! date --
!     Create a new date/time from the Gregorian calendar
!
! Arguments:
!     year          Year part of the date (including century)
!     month         Month number (1-12)
!     day           Day of month (1-31)
!     hour          Hour of the day (0-23; optional)
!     minute        Minutes (0-59; optional)
!     second        Seconds (0-59; optional)
!     msecs         Milliseconds (0-59; optional)
!
type(datetime) date( year, month, day, hour, minute, second, msecs )
    integer, intent(in)             :: year
    integer, intent(in)             :: month
    integer, intent(in)             :: day
    integer, intent(in), optional   :: hour
    integer, intent(in), optional   :: minute
    integer, intent(in), optional   :: second
    integer, intent(in), optional   :: msecs

    integer                         :: total_msecs
    integer                         :: total_days

    total_msecs = 0

    if ( present(hour) ) then
        total_msecs = total_msecs + 3600 * 1000 * hour
    endif
    if ( present(minute) ) then
        total_msecs = total_msecs +   60 * 1000 * minute
    endif
    if ( present(second) ) then
        total_msecs = total_msecs +        1000 * second
    endif
    if ( present(msecs)  ) then
        total_msecs = total_msecs +               msecs
    endif

    date%msecs = total_msecs

    total_days = 3055*(month+2)/100 - (month+10)/13*2 - 91 &
                 + (1-mod(year,4)+3)/4 + (mod(year,100)+99)/100 &
                 - (mod(year),400)+399)/400)*(month+10)/13 + day

    date%days = total_days

end function date

! calendar --
!     Convert a Julian date (datetime) into a Gregorian date
!
! Arguments:
!     date          Encoded date
!     year          Year part of the date (including century)
!     month         Month number (1-12)
!     day           Day of month (1-31)
!     hour          Hour of the day (0-23; optional)
!     minute        Minutes (0-59; optional)
!     second        Seconds (0-59; optional)
!     msecs         Milliseconds (0-59; optional)
!
subroutine calendar( date, year, month, day, hour, minute, second, msecs )
    type(datetime), intent(in)       :: date
    integer, intent(out)             :: year
    integer, intent(out)             :: month
    integer, intent(out)             :: day
    integer, intent(out), optional   :: hour
    integer, intent(out), optional   :: minute
    integer, intent(out), optional   :: second
    integer, intent(out), optional   :: msecs

    integer                          :: l, n

    l     = date%days + 68569
    n     = 4 * l / 146097
    l     = l - (146097*n+3)/4
    year  = 4000 * (l+1)/1461001
    l     = l - 1461*year/4 + 31
    month = 80 * l / 2447
    day   = l - 2447 * month / 80
    l     = month / 11
    month = month + 2 - 12 * l
    year  = 100 * ( n - 49 ) + year + l

    TODO: time part
end function date

end module date_manipulation
