! select_precision.f90 --
!     Module for defining kinds with single and double precision
!
!     sp: kind for single precision
!     dp: kind for double precision
!
!     wp: kind for working precision (set to either sp or dp)
!
!     Note:
!     Originally the kind for double precision was determined by
!     selecting a precision strictly larger than single precision.
!     But what if via compiler options single precision constants
!     are promoted to double precision?
!
!     $Id: select_precision.f90,v 1.3 2008/10/02 09:02:33 arjenmarkus Exp $
!
module select_precision
    implicit none

    integer, parameter :: sp = kind( 1.0 )
    integer, parameter :: dp = kind( 1.0d0 )

    integer, parameter :: wp = sp

end module select_precision
