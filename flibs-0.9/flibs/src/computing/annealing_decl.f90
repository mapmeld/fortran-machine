! annealing_decl.f90 --
!     Declarations for the reverse communication interface
!     to the simulated annealing module
!
!     Use as an include file
!
     type(ANNEALING_PARAMETERS) :: anneal_data
     integer                    :: task
     real                       :: value
