! DOC
!
!  chkcomff.f90 - source code to check certain features of the
!                 FORTRAN 90 compiler
!
!  Copyright (C) 1998 Arjen Markus
!
!  Arjen Markus
!
!
!  General information:
!  This file contains source code that uses various violations of the
!  FORTRAN 90 standard and intentionally bad programming fragments.
!  Comments explain each of them.
!  Its purpose is to check if the compiler (in combination with the
!  options) will flag the features.
!
! ENDDOC
!
!  $Author: arjenmarkus $
!  $Date: 2008/03/17 17:57:56 $
!  $Source: /cvsroot/flibs/chksys/chkcompff.f90,v $
!  $Log: chkcompff.f90,v $
!  Revision 1.1  2008/03/17 17:57:56  arjenmarkus
!  Added directory "chksys" - programs to probe compiler properties
!
!
! --------------------------------------------------------------------
!
! --------------------------------------------------------------------
!   Module:   SCOPE_MOD
!   Author:   Arjen Markus
!   Purpose:  Introduce scope problems
!   Context:  Used by main
!   Summary:
!             Define some variables to check scoping issues:
!             - Variable "int_var" is also defined in the
!               main program.
!             - Variable "real_mod" is passed as an actual argument
!               in the main program to subroutine "double_access"
!             - The module uses module "sub_module" to see what happens
!               to variables that are defined in sub modules: do they
!               automatically become visible?
! --------------------------------------------------------------------
!
module sub_module
   integer                   :: int_submodule
end module
module scope_mod
   integer , dimension(1:10) :: int_var
   real                      :: real_mod
end module

! --------------------------------------------------------------------
!   Routine:  arglist_extern
!   Author:   Arjen Markus
!   Purpose:  Show the compiler's ability to detect interface problems
!   Context:  Used by main program
!   Summary:
!             Simple subroutine, called with the wrong type of
!             actual arguments
! --------------------------------------------------------------------
!
subroutine arglist_extern( intv )
!
   implicit none
!
   integer ::   intv

   write(*,*) 'Value:' , intv
   return
end subroutine

! --------------------------------------------------------------------
!   Program:  CHKCOMP
!   Author:   Arjen Markus
!   Purpose:  Main program
!   Context:  -
!   Summary:
!             Show the properties of the compiler by expressly
!             introducing (semantical) errors and extensions
! --------------------------------------------------------------------
!
program chkcmp
!
! -------- Use module "scope_mod": two levels of scope problems
!          The module defines variables "int_var", "real_mod" and
!          via a submodule "int_submodule".
!          The scope problems are:
!          - Program CHKCOMP defines a local variable "int_var"
!          - Program CHKCOMP assumes "int_submodule" is available
!            via "scope_mod"
!          - Program CHKCOMP uses "real_mod" as an actual parameter:
!            to the subroutine "double_access"
!            - once for a dummy parameter with a different name
!            - once for a dummy parameter with the same name
!
   use scope_mod
!
! -------- A very useful feature: make sure that all variables
!          have to be declared. It should be placed before any
!          declarations
!
   implicit none
!
! -------- FORTRAN 90 introduced the problem of scope into the
!          FORTRAN world. Does the compiler flag such problems?
!          The variables string_var and int_var are also used in
!          subroutine scope_problem.
!
   character                      :: string_var
   integer                        :: int_var      ! Also in "scope_mod"
   real                           :: real_var
   real , pointer , dimension(:)  :: ptor
!
! -------- Does the compiler check for:
!          - scoping problems
!          - problems with actual argument lists (internal and external)
!          - problems with double access
!          - problems with incomplete logic in function
!          - problems with deallocation via a pointer
!          - missing interface (required when working with pointers)
!          - use of variables before they are set
!
   call scope_problem
   call arglist_problem( real_var )
   call arglist_extern(  real_var )
   call double_access(   real_var , real_var , real_mod )
   int_var = -1
   write( * , * ) 'Result: ' , func_incomplete( int_var )
   write( * , * ) 'Result: ' , func_no_return ( int_var )

   call alloc_dealloc
   call set_null( ptor )
   call undefined_vars

!
!  end of program
!
   stop

! --------------------------------------------------------------------
!   Routines: internal routines
!   Author:   Arjen Markus
!   Contains:
!   scope_problem   - Show scoping problems
!   arglist_problem - Show that the calls to internal routines are checked
!   double_access   - Show problems with double access
!   func_incomplete - Show that incomplete logic brings trouble to
!                     functions
! --------------------------------------------------------------------
contains

! @@------------------------------------------------------------------
!   Routine:  scope_problem
!   Author:   Arjen Markus
!   Purpose:  Show scoping problems
!   Context:  Used by main program
!   Summary:
!             Redefine two variables that have global scope
! --------------------------------------------------------------------
!
   subroutine scope_problem( )
!
   implicit none
!
! -------- These variables are also defined in CHKCOMP
!
   character    string_var
   complex      int_var        ! This is an integer in CHKCOMP and in
                               ! scope_mod

   int_var = (1.0,1.0)
   write(*,*) 'Complex:' , int_var
   return
   end subroutine

! --------------------------------------------------------------------
!   Routine:  arglist_problem
!   Author:   Arjen Markus
!   Purpose:  Show that the calls to internal routines are checked
!   Context:  Used by main program
!   Summary:
!             Define a subroutine with a different list than used
!             in the main program
! --------------------------------------------------------------------
!
   subroutine arglist_problem( one_arg )
!
   implicit none
!
! -------- The one argument in main is a real
!
   character :: one_arg

   write(*,*) 'One arg' , one_arg
   return
   end subroutine

! --------------------------------------------------------------------
!   Routine:  double_access
!   Author:   Arjen Markus
!   Purpose:  Show problems with double access
!   Context:  Used by main program
!   Summary:
!             The first dummy argument has the same name as a variable
!             in the main routine. The second dummy argument has a
!             different name, but the actual one substituted is
!             the same. The third argument has the same name as a
!             module variable.
!
!             This is trouble according to the standard.
! --------------------------------------------------------------------
!
   subroutine double_access( real_var , second_arg , real_mod )
!
   implicit none
   real :: real_var , second_arg , real_mod

   real_var = second_arg + real_mod

   return
   end subroutine

! --------------------------------------------------------------------
!   Routine:  func_incomplete
!   Author:   Arjen Markus
!   Purpose:  Show that incomplete logic brings trouble to functions
!   Context:  Used by main program
!   Summary:
!             Set the function's return value ONLY if the argument is
!             positive. Forget about the other possibilities. Is this
!             flagged? (The function does not always return an
!             explicitly set value!)
! --------------------------------------------------------------------
!
   integer function func_incomplete( one_arg )
!
   implicit none
!
   integer :: one_arg

   if ( one_arg > 0 ) then
      func_incomplete = 1
   endif
!
   return
   end function

! --------------------------------------------------------------------
!   Routine:  func_no_return
!   Author:   Arjen Markus
!   Purpose:  Show that functions that do not set a return value are
!             flagged
!   Context:  Used by main program
!   Summary:
!             Do not set the function's return value
! --------------------------------------------------------------------
!
   integer function func_no_return( one_arg )
!
   implicit none
!
   integer :: one_arg
   integer :: return_val

   if ( one_arg > 0 ) then
      return_val = 1 ! Just a local!
   endif
!
   return
   end function

! --------------------------------------------------------------------
!   Routines: end of internal procedures
! --------------------------------------------------------------------
end program

! --------------------------------------------------------------------
!   Routine:  set_null
!   Author:   Arjen Markus
!   Purpose:  Show whether the compiler complains about lacking interfaces
!   Context:  Used by main program
!   Summary:
!             Set the one argument to NULL. As this is a pointer,
!             an interface is required. Does the compiler complain?
! --------------------------------------------------------------------
!
subroutine set_null( ptor )
!
   implicit none
!
   real, pointer, dimension(:) :: ptor

   nullify( ptor )
   return
end subroutine

! --------------------------------------------------------------------
!   Routine:  use_interface
!   Author:   Arjen Markus
!   Purpose:  Show the strange error messages when using interfaces wrongly
!   Context:  Used by main program
!   Summary:
!             Define an interface with a name at the end
! --------------------------------------------------------------------
!
subroutine use_interface
!
   implicit none
!
   real, pointer, dimension(:) :: ptor
   real, pointer, dimension(:) :: ptor2

!
! The end interface statement should not contain the name!
! Note:
! Some compilers will accept this. Others do not.
!
   interface null_pointer
      subroutine set_null( ptor )
      real, pointer, dimension(:) :: ptor
      end subroutine
   end interface null_pointers

   call null_pointer( ptor  )
   call null_pointer( ptor2 )

   return
end subroutine

! --------------------------------------------------------------------
!   Routine:  alloc_dealloc
!   Author:   Arjen Markus
!   Purpose:  Show that the compiler can detect obvious deallocation
!             problems
!   Context:  Used by main program
!   Summary:
!             Allocate an allocatable array, set a pointer to it
!             and deallocate via the pointer - this violates the
!             standard's access rules.
! --------------------------------------------------------------------
!
subroutine alloc_dealloc
!
   implicit none
!
   real, pointer, dimension(:)     :: ptor
   real, allocatable, dimension(:) :: rarray

!
! Allocate the array and deallocate via the pointer
!
   allocate( rarray(1:10) )

   ptor => rarray
   deallocate( ptor )

   return
end subroutine

! --------------------------------------------------------------------
!   Routine:  undefined_vars
!   Author:   Arjen Markus
!   Purpose:  Show that the compiler can detect assignments that
!             use undefined variables
!   Context:  Used by main program
!   Summary:
!             Use unset local variables in an assignment.
!             Compare reals (another obvious problem).
!             Convert double to real (yet another obvious problem).
! --------------------------------------------------------------------
!
subroutine undefined_vars
!
   implicit none
!
   real                            :: result
   real                            :: operand_a
   real                            :: operand_b
   real , pointer                  :: ptor
   double precision                :: dbl_value

!
! Assignment with undefined variables
!
   result = operand_a + operand_b

!
! Silently convert double precision to single
!
   dbl_value = atan( 1.0d+00 )
   result    = dbl_value

!
! Compare reals
!
   if ( result .ne. dbl_value ) then
      write( * , * ) 'Some truncation occurred!'
   endif

!
! Write unassigned pointer to screen
!
   write( * , * ) ptor

   return
end subroutine
