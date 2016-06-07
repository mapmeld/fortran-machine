! data_for_sets.f90 --
!     Implementation of unordered sets
!     (part: underlying data structure)
!
!     See the example/test program for the way to use this
!
!     This part defines the VECTOR data type and the
!     encompassing module
!
!     Note:
!     The using code must declare the type SET_DATA and
!     the logical function element_isequal
!
!     $Id: data_for_sets.f90,v 1.2 2006/03/26 19:03:53 arjenmarkus Exp $
!
type VECTOR_DATA
    type(SET_DATA) :: data
endtype
public :: SET_DATA
public :: element_isequal, operator(.eq.)

type(VECTOR_DATA), save :: empty_vector_data ! Do not initialise it -
                                             ! it is just a place holder

include 'vectors.f90'
