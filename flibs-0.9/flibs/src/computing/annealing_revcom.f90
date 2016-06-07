! annealing_revcom.f90 --
!     Template for the reverse communication interface
!     to the simulated annealing module
!
!     Usage:
!     - Fill in the code to evaluate the function (in the
!       widest sense of the word) that must be minimised.
!     - If needed, fill in the code to write a report on
!       the progress
!     - Use the file as an include file for a function
!       or subroutine:
!
!       subroutine minimize( f, range, params, x )
!           interface
!               real function f( x, params )
!                   use PARAM_DEF
!                   real, dimension(:), intent(in) :: x
!                   type(FUNC_PARAMS), intent(in)  :: params
!               end function
!           end interface
!
!           real, dimension(2,:) :: range    ! range and x are predefined
!           real, dimension(:)   :: x        ! variables, as are value and task
!
!           type(FUNC_PARAMS)    :: params
!
!           include 'annealing_decl.f90'
!
!           call set_parameters( anneal_data, ...)
!
!           include 'filled_in.f90' ! Filled in template
!
!       end subroutine
!

     do
         call get_next_step( anneal_data, range, x, value, task )

         select case ( task )
             case ( annealing_value )
                 !
                 ! Fill in the evaluation of the function
                 !
                 value = f(x)

             case ( annealing_report )
                 !
                 ! Fill in the reporting code
                 !
                 write(*,*) 'Value so far: ', value ' - vector: ', x

             case ( annealing_done )
                 exit
         end select
     enddo
