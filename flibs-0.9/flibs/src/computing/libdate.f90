MODULE LibDate
!
! Introduces a new variable type: DateType
! and an operator + to add two of such variables.
! This library gives the opportunity to make statements
! of the following kind:
!
! PROGRAM Test
!    USE LibDate
!    TYPE(DateType) :: ThisDate = DateType(2007,1,29,17,0),&
!                    & TimeStep = DateType(   0,0, 1, 0,0),&
!                    & NewDate
!    NewDate = ThisDate + TimeStep ! The only program statement
! END PROGRAM Test
!
! ____________________________________________________
!
! Version: 12 september 2008
!
! Developed by:
!
! Dr. Arjan van Dijk
!
! For:
!
! LSO   Laboratory for Radiation Research
! RIVM - National Institute for Public Health and the Environment
!
! PO Box 1,
! NL - 3720 BA Bilthoven
! The Netherlands
!
! E-mail:  Arjan.van.Dijk@rivm.nl
! Website: http://www.rivm.nl/
!
! Use, copy and modify this source code freely and at your own risk!
! Improvements, suggestions and compliments always welcome!
! ____________________________________________________
!
IMPLICIT NONE

INTEGER,PARAMETER :: Float = kind(1.0)

!
!USE libxmath   ! The only thing USEd from libxmath is:
!
! INTEGER,PARAMETER :: Float = 4
!
! This parameter is used to define all real numbers as REAL(Float) instead of just REAL.
! Include this parameter somewhere in your own code and make it publicly available to this MODULE
! by the proper USE statement, or change all references to REAL(Float) to REAL or to your own type.
! Beware: all REAL(Float) constants in this module also have "_Float" appended to ensure the right type!
!

!
! Principally everything is LOCAL...
!
PRIVATE
!
! ... except for the following:
!
PUBLIC ::         &
& DateType       ,& ! User-defined type for year, month, day, hour and minute
& OPERATOR(.EQ.) ,& ! Comparison of two DateTypes
& OPERATOR(.NE.) ,& ! Comparison of two DateTypes
& OPERATOR(.GT.) ,& ! Comparison of two DateTypes
& OPERATOR(.GE.) ,& ! Comparison of two DateTypes
& OPERATOR(.LT.) ,& ! Comparison of two DateTypes
& OPERATOR(.LE.) ,& ! Comparison of two DateTypes
& LeapYear       ,& ! LOGICAL function of 1 INTEGER arguement that is .TRUE. is its arguement is a leap-year
& DOY            ,& ! INTEGER number, giving the Day Of Year of its DateType arguement
& MinDate        ,& ! Compare two DateTypes and return the one that is earlier in time
& MaxDate        ,& ! Compare two DateTypes and return the one that is later in time
& TimeLag        ,& ! Give difference in time between two DateTypes in days (real number representation)
& OPERATOR(*)    ,& ! Multiplication of DateType with real and integer (for timesteps; only days, hours and minutes are multiplied!)
& OPERATOR(+)    ,& ! Add two DateTypes; the latter is considered a "timestep" of which month and year are neglected!
& OPERATOR(-)    ,& ! As "+" above
& DelaySeconds   ,& ! Real function converting a given DateType timestep to seconds (disregarding year and month!)
& JulianDateType ,& ! Mostly for internal use only; Julian date is represented by 2 numbers since REAL*4 has too little digits...
& Date2Julian    ,& ! Conversion routine DateType --> JulianDateType; for internal use
& Julian2Date    ,& ! Conversion routine JulianDateType --> DateType; for internal use
& format_date       ! Format a date/time structure

   TYPE DateType
     INTEGER :: Year,Month,Day,Hour,Minute
   END TYPE DateType




   INTERFACE OPERATOR (+)
      MODULE PROCEDURE AddDates
   END INTERFACE
   INTERFACE OPERATOR (-)
      MODULE PROCEDURE SubtractDates
   END INTERFACE

   INTERFACE OPERATOR (.EQ.)
      MODULE PROCEDURE EqualDates
   END INTERFACE
   INTERFACE OPERATOR (.NE.)
      MODULE PROCEDURE NotEqualDates
   END INTERFACE
   INTERFACE OPERATOR (.GT.)
      MODULE PROCEDURE LargerDates
   END INTERFACE
   INTERFACE OPERATOR (.LT.)
      MODULE PROCEDURE SmallerDates
   END INTERFACE
   INTERFACE OPERATOR (.GE.)
      MODULE PROCEDURE LargerEqualDates
   END INTERFACE
   INTERFACE OPERATOR (.LE.)
      MODULE PROCEDURE SmallerEqualDates
   END INTERFACE

   INTERFACE OPERATOR (*)
      MODULE PROCEDURE IntTimesDate
   END INTERFACE
   INTERFACE OPERATOR (*)
      MODULE PROCEDURE DateTimesInt
   END INTERFACE
   INTERFACE OPERATOR (*)
      MODULE PROCEDURE FloatTimesDate
   END INTERFACE
   INTERFACE OPERATOR (*)
      MODULE PROCEDURE DateTimesFloat
   END INTERFACE

   TYPE JulianDateType
     REAL(Float) :: Head,Tail
   END TYPE JulianDateType


!
! The following parameter can be set for debugging this MODULE
!
INTEGER, PARAMETER :: DebugLevel = 0
!
! Below this line you'll get the real implementations
!
CONTAINS



FUNCTION Date2Julian(ThisDate)
!
! Compute julian date from a regular date
!
TYPE(DateType), INTENT(IN) :: ThisDate
TYPE(JulianDateType) :: Date2Julian,DumJulian

REAL(Float) :: Dum
REAL(Float), PARAMETER :: Dum0 = 1720994.5
!
INTEGER :: iiYear,iiMonth,iiDay
INTEGER :: Century,E,F,D
!
   iiYear = ThisDate%Year
   iiMonth = ThisDate%Month
   iiDay = ThisDate%Day
!
! Calculation starts 1st of March (after possible leap-day)
!
   IF (iiMonth .LT. 3) THEN
      iiYear = iiYear -1
      iiMonth = iiMonth + 12
   ENDIF
!
! Number of days IN the past years, including the leap-days once every 4 years
!
   E = INT(365.0 * iiYear) + iiYear/4
!
! Average number of days IN a Julian month }
!
   F = INT(30.6001 * (iiMonth + 1))
!
! Correction of leap-days for centuries }
!
   Century = iiYear/100
   D = 2 - Century + Century/4
!
! Add all contributing days
!
   Dum = iiDay + Dum0 + D + E + F
!
   DumJulian%Head = Dum
   DumJulian%Tail = ThisDate%Hour/24._Float + ThisDate%Minute/(24._Float*60._Float)

   IF (DumJulian%Tail.GE.0.5_Float) THEN
     DumJulian%Head = DumJulian%Head + 0.5_Float
     DumJulian%Tail = DumJulian%Tail - 0.5_Float
   ELSE
     DumJulian%Head = DumJulian%Head - 0.5_Float
     DumJulian%Tail = DumJulian%Tail + 0.5_Float
   ENDIF
!
   Date2Julian = DumJulian
!
END FUNCTION Date2Julian




FUNCTION Julian2Date(JulianDate)
!
! Compute regular date from julian date
!
TYPE(JulianDateType), INTENT(IN) :: JulianDate
TYPE(DateType) :: Julian2Date
!
TYPE(JulianDateType) :: DumJulian
INTEGER :: j,g,dg,c,dc,b,db,a,da,y,m,d,Hour,Minute,iDum
REAL(Float) :: Dum
!
   DumJulian = JulianDate

   DumJulian%Tail = NINT(DumJulian%Tail*24._Float*60._Float)/(24._Float*60._Float)

   IF (DumJulian%Tail.GE.0.5_Float) THEN
     DumJulian%Head = DumJulian%Head + 0.5_Float
     DumJulian%Tail = DumJulian%Tail - 0.5_Float
   ELSE
     DumJulian%Head = DumJulian%Head - 0.5_Float
     DumJulian%Tail = DumJulian%Tail + 0.5_Float
   ENDIF

   j = NINT(DumJulian%Head) + 32044
   g = j / 146097
   dg = MOD(j,146097)
   c = (dg / 36524 + 1) * 3 / 4
   dc = dg - c * 36524
   b = dc / 1461
   db = MOD(dc,1461)
   a = (db / 365 + 1) * 3 / 4
   da = db - a * 365
   y = g * 400 + c * 100 + b * 4 + a
   m = (da * 5 + 308) / 153 - 2
   d = da - (m + 4) * 153 / 5 + 122
   Y = y - 4800 + (m + 2) / 12
   M = MOD((m + 2),12) + 1
   D = d + 1

   Dum = 60._Float*24._Float*DumJulian%Tail
    IF (DebugLevel.GT.0) WRITE(*,40) Dum
 40 FORMAT('Julian2Date : Dum = ',F20.10)
   iDum = NINT(Dum) ! Number of minutes in this day
    IF (DebugLevel.GT.0) WRITE(*,30) iDum
 30 FORMAT('Julian2Date : iDum = ',I10)
   Hour = iDum/60 ! integer division gives DIV
    IF (DebugLevel.GT.0) WRITE(*,10) Hour
 10 FORMAT('Julian2Date : Hour = ',I2)
!   Minute = INT(60.*(24.*DumJulian%Tail - Hour))
   Minute = iDum - Hour*60
    IF (DebugLevel.GT.0) WRITE(*,20) Minute
 20 FORMAT('Julian2Date : Minute = ',I2)

   Julian2Date = DateType(Y,M,D,Hour,Minute)
!
END FUNCTION Julian2Date






   FUNCTION AddDates( BaseDate, TimeStep )
!
! Add a timestep [DateType] to a date and make sure that the result exists
! In the timestep, the Months and Years are not considered
!
      TYPE( DateType ) AddDates, DumDate
      TYPE( DateType ), INTENT( IN ) :: BaseDate, TimeStep
      TYPE(JulianDateType) :: JulianDate
      REAL(Float) :: JulianMainShare
!
! Convert to Julian calendar
!
      IF (DebugLevel.GT.0) WRITE(*,10) BaseDate
 10   FORMAT('AddDates : BaseDate = ',5(1X,I4))
      JulianDate = Date2Julian(BaseDate)
      IF (DebugLevel.GT.0) WRITE(*,20) JulianDate
 20   FORMAT('AddDates : JulianDate = ',2(1X,F20.10))
!
! Remove the large number of days in the Julian reference date
!
      JulianMainShare = JulianDate%Head
      IF (DebugLevel.GT.0) WRITE(*,30) JulianMainShare
 30   FORMAT('AddDates : JulianMainShare = ',F20.10)
!
! Consider the bits and pieces behand the comma and account for the step
!
      JulianDate%Head = JulianDate%Tail &
      & + TimeStep%Day + TimeStep%Hour/24.&
      & + TimeStep%Minute/(24.*60.)
      IF (DebugLevel.GT.0) WRITE(*,40) JulianDate%Head
 40   FORMAT('AddDates : JulianDate%Head = ',F20.10)
!
! Give the part behand the comma to the tail
!
      JulianDate%Tail = MOD(JulianDate%Head,1._Float)
      IF (DebugLevel.GT.0) WRITE(*,50) JulianDate%Tail
 50   FORMAT('AddDates : JulianDate%Tail = ',F20.10)
      JulianDate%Head = JulianMainShare + INT(JulianDate%Head)
      IF (DebugLevel.GT.0) WRITE(*,60) JulianDate%Head
 60   FORMAT('AddDates : JulianDate%Head = ',F20.10)

      IF (JulianDate%Tail.LT.0.) THEN
	JulianDate%Tail = JulianDate%Tail + 1._Float
      IF (DebugLevel.GT.0) WRITE(*,70) JulianDate%Tail
 70   FORMAT('AddDates : JulianDate%Tail = ',F20.10)
	JulianDate%Head = JulianDate%Head - 1._Float
      IF (DebugLevel.GT.0) WRITE(*,80) JulianDate%Head
 80   FORMAT('AddDates : JulianDate%Head = ',F20.10)
      ENDIF

      DumDate = Julian2Date(JulianDate)
      IF (DebugLevel.GT.0) WRITE(*,90) DumDate
 90   FORMAT('AddDates : DumDate = ',5(1X,I4))

      AddDates = DumDate
   END FUNCTION AddDates







   FUNCTION SubtractDates( BaseDate, TimeStep )
!
! Subtract a timestep [DateType] from a date and make sure that the result exists
! In the timestep, the Months and Years are not considered
!
   TYPE( DateType ), INTENT( IN ) :: BaseDate, TimeStep
   TYPE(DateType) :: DumDate,SubtractDates
!
      DumDate = DateType(0,0,-TimeStep%Day,-TimeStep%Hour,-TimeStep%Minute)
!
      SubtractDates = AddDates(BaseDate,DumDate)
!
   END FUNCTION SubtractDates







   FUNCTION EqualDates( Date1, Date2 )
!
! Compare 2 dates
!
   TYPE( DateType ), INTENT( IN ) :: Date1,Date2
   LOGICAL :: EqualDates
!
   EqualDates = (Date1%Year  .EQ.Date2%Year  )&
   &       .AND.(Date1%Month .EQ.Date2%Month )&
   &       .AND.(Date1%Day   .EQ.Date2%Day   )&
   &       .AND.(Date1%Hour  .EQ.Date2%Hour  )&
   &       .AND.(Date1%Minute.EQ.Date2%Minute)
!
   END FUNCTION EqualDates



   FUNCTION NotEqualDates( Date1, Date2 )
!
! Compare 2 dates.
!
   TYPE( DateType ), INTENT( IN ) :: Date1,Date2
   LOGICAL :: NotEqualDates
!
   NotEqualDates = (.NOT.EqualDates(Date1,Date2))
!
   END FUNCTION NotEqualDates



   FUNCTION LargerDates( Date1, Date2 )
!
! Compare 2 dates. Seconds are not considered.
!
   TYPE( DateType ), INTENT( IN ) :: Date1,Date2
   LOGICAL :: LargerDates
!
   TYPE(JulianDateType) :: Julian1,Julian2
   LOGICAL :: DumBool
!
   Julian1 = Date2Julian(Date1)
   Julian2 = Date2Julian(Date2)

   DumBool = (Julian1%Head.GT.Julian2%Head)&
   & .OR.((NINT(Julian1%Head-Julian2%Head).EQ.0).AND.(Julian1%Tail.GT.Julian2%Tail))

   LargerDates = DumBool
!
   END FUNCTION LargerDates



   FUNCTION LargerEqualDates( Date1, Date2 )
!
! Compare 2 dates. Seconds are not considered.
!
   TYPE( DateType ), INTENT( IN ) :: Date1,Date2
   LOGICAL :: LargerEqualDates
!
   LargerEqualDates = (EqualDates(Date1,Date2).OR.LargerDates(Date1,Date2))
!
   END FUNCTION LargerEqualDates



   FUNCTION SmallerDates( Date1, Date2 )
!
! Compare 2 dates. Seconds are not considered.
!
   TYPE( DateType ), INTENT( IN ) :: Date1,Date2
   LOGICAL :: SmallerDates
!
   SmallerDates = LargerDates(Date2,Date1)
!
   END FUNCTION SmallerDates



   FUNCTION SmallerEqualDates( Date1, Date2 )
!
! Compare 2 dates. Seconds are not considered.
!
   TYPE( DateType ), INTENT( IN ) :: Date1,Date2
   LOGICAL :: SmallerEqualDates
!
   SmallerEqualDates = (EqualDates(Date1,Date2).OR.SmallerDates(Date1,Date2))
!
   END FUNCTION SmallerEqualDates




   FUNCTION IntTimesDate( i,TimeStep )
!
! Multiply a timestep in date-format with an integer
!
   TYPE( DateType ), INTENT( IN ) :: TimeStep
   INTEGER, INTENT(IN) :: i
   TYPE(DateType) :: IntTimesDate
!
      IntTimesDate = DateType(0,0,&
      &                       i*TimeStep%Day   ,&
      &                       i*TimeStep%Hour  ,&
      &                       i*TimeStep%Minute)
!
   END FUNCTION IntTimesDate


   FUNCTION DateTimesInt( TimeStep,i )
!
! Multiply a timestep in date-format with an integer
!
   TYPE( DateType ), INTENT( IN ) :: TimeStep
   INTEGER, INTENT(IN) :: i
   TYPE(DateType) :: DateTimesInt
!
      DateTimesInt = IntTimesDate(i,TimeStep)
!
   END FUNCTION DateTimesInt



   FUNCTION FloatTimesDate( x,TimeStep )
!
! Multiply a timestep in date-format with a float
!
   TYPE( DateType ), INTENT( IN ) :: TimeStep
   REAL(Float), INTENT(IN) :: x
   TYPE(DateType) :: FloatTimesDate
!
      FloatTimesDate = DateType(0,0,&
      &                       NINT(x*TimeStep%Day   ),&
      &                       NINT(x*TimeStep%Hour  ),&
      &                       NINT(x*TimeStep%Minute))
!
   END FUNCTION FloatTimesDate


   FUNCTION DateTimesFloat( TimeStep,x )
!
! Multiply a timestep in date-format with a float
!
   TYPE( DateType ), INTENT( IN ) :: TimeStep
   REAL(Float), INTENT(IN) :: x
   TYPE(DateType) :: DateTimesFloat
!
      DateTimesFloat = FloatTimesDate(x,TimeStep)
!
   END FUNCTION DateTimesFloat




   FUNCTION TimeLag( Date1, Date2 )
!
! Give difference in time between two dates in days
!
   TYPE( DateType ), INTENT( IN ) :: Date1,Date2
   REAL(Float) :: TimeLag,Dum1,Dum2
!
   TYPE(JulianDateType) :: Julian1,Julian2
!
   Julian1 = Date2Julian(Date1)
   Julian2 = Date2Julian(Date2)

   Dum1 = Julian1%Head - Julian2%Head
   Dum2 = Julian1%Tail - Julian2%Tail

   TimeLag = Dum1 + Dum2
!
   END FUNCTION TimeLag




   FUNCTION DelaySeconds( TimeStep )
!
! Give difference in time in a step
!
   TYPE( DateType ), INTENT( IN ) :: TimeStep
   REAL(Float) :: DelaySeconds
!
   DelaySeconds = TimeStep%Day*24._Float*3600._Float&
   &            + TimeStep%Hour*3600._Float&
   &            + TimeStep%Minute*60._Float
!
   END FUNCTION DelaySeconds



   FUNCTION LeapYear(iYear)
!
! Is this a leapyear?
!
   INTEGER, INTENT(IN) :: iYear
   LOGICAL :: LeapYear
!
   LeapYear = ((MOD(iYear,4).EQ.0) &
   &           .AND. &
   &    ((MOD(iYear,100).NE.0)  .OR.  (MOD(iYear,400).EQ.0)))
!
   END FUNCTION LeapYear


   FUNCTION DOY(t)
!
! Give the day of the year
!
   TYPE(DateType), INTENT(IN) :: t
   INTEGER :: DOY,Dum
   INTEGER, DIMENSION(12), PARAMETER ::     OffsetDays = (/0,31,59,90,120,151,181,212,243,273,304,334/)
   INTEGER, DIMENSION(12), PARAMETER :: LeapOffsetDays = (/0,31,60,91,121,152,182,213,244,274,305,335/)
!
   IF (LeapYear(t%Year)) THEN
     Dum = LeapOffsetDays(t%Month) + t%Day
   ELSE
     Dum = OffsetDays(t%Month) + t%Day
   ENDIF
!
   DOY = Dum
!
   END FUNCTION DOY





   FUNCTION MinDate(Date1,Date2)
!
! Give the earliest of two dates
!
   TYPE(DateType), INTENT(IN) :: Date1,Date2
   TYPE(DateType) :: DumDate,MinDate
!
   IF (Date1.LT.Date2) THEN
     DumDate = Date1
   ELSE
     DumDate = Date2
   ENDIF
!
   MinDate = DumDate
!
   END FUNCTION MinDate





   FUNCTION MaxDate(Date1,Date2)
!
! Give the latest of two dates
!
   TYPE(DateType), INTENT(IN) :: Date1,Date2
   TYPE(DateType) :: DumDate,MaxDate
!
   IF (Date1.GT.Date2) THEN
     DumDate = Date1
   ELSE
     DumDate = Date2
   ENDIF
!
   MaxDate = DumDate
!
   END FUNCTION MaxDate

! format_date --
!     Format a date/time as a string
!
! Arguments:
!     date             Date/time to be formatted
!     pattern          String that serves as the pattern
!     datestring       Resulting string
!
! Note:
!     The pattern can contain any of the following substrings
!     that will be replaced by the corresponding date/time information
!
!     dd        Day of month ("01" for instance)
!     ds        Day of month ("1" for instance, s for space)
!     DDD       Day of the year
!     HH        Hour (00-23)
!     HS        Hour (0-23)
!     mm        Month ("01" for january)
!     ms        Month ("1" for january, s for space)
!     MM        Minutes within the hour (00-59)
!     MS        Minutes within the hour (0-59)
!     YY        Year with the century
!     yyyy      Year with the century
!
!     Each substring is replaced by a string of the same length or
!     shorter.
!
!     The third argument should in general be at least as long
!     as the pattern.
!
! (Added by Arjen Markus)

   SUBROUTINE format_date( date, pattern, datestring )

   TYPE(DateType), INTENT(IN) :: date
   character(len=*)           :: pattern
   character(len=*)           :: datestring

   character(len=4)           :: piece
   integer                    :: k
   logical                    :: found

   datestring = pattern

   found = .true.
   do while ( found )
       found = .false.

       k     = index( datestring, 'dd' )
       if ( k > 0 ) then
           found = .true.
           write( piece, '(i2.2)' ) date%day
           datestring(k:k+1) = piece
       endif

       k     = index( datestring, 'ds' )
       if ( k > 0 ) then
           found = .true.
           write( piece, '(i2)' ) date%day
           datestring(k:) = adjustl( piece(1:2) // datestring(k+2:) )
       endif

       k     = index( datestring, 'DDD' )
       if ( k > 0 ) then
           found = .true.
           write( piece, '(i3)' ) doy(date)
           datestring(k:k+2) = piece
       endif

       k     = index( datestring, 'HH' )
       if ( k > 0 ) then
           found = .true.
           write( piece, '(i2.2)' ) date%hour
           datestring(k:k+1) = piece
       endif

       k     = index( datestring, 'HS' )
       if ( k > 0 ) then
           found = .true.
           write( piece, '(i2)' ) date%hour
           datestring(k:) = adjustl( piece(1:2) // datestring(k+2:) )
       endif

       k     = index( datestring, 'mm' )
       if ( k > 0 ) then
           found = .true.
           write( piece, '(i2.2)' ) date%month
           datestring(k:k+1) = piece
       endif

       k     = index( datestring, 'ms' )
       if ( k > 0 ) then
           found = .true.
           write( piece, '(i2)' ) date%month
           datestring(k:) = adjustl( piece(1:2) // datestring(k+2:) )
       endif

       k     = index( datestring, 'MM' )
       if ( k > 0 ) then
           found = .true.
           write( piece, '(i2.2)' ) date%minute
           datestring(k:k+1) = piece
       endif

       k     = index( datestring, 'MS' )
       if ( k > 0 ) then
           found = .true.
           write( piece, '(i2)' ) date%minute
           datestring(k:) = adjustl( piece(1:2) // datestring(k+2:) )
       endif

       k     = index( datestring, 'YY' )
       if ( k > 0 ) then
           found = .true.
           write( piece, '(i2.2)' ) mod(date%year, 100)
           datestring(k:k+1) = piece
       endif

       k     = index( datestring, 'yyyy' )
       if ( k > 0 ) then
           found = .true.
           write( piece, '(i4)' ) date%year
           datestring(k:k+3) = piece
       endif
   ENDDO

   END SUBROUTINE format_date


END MODULE LibDate
