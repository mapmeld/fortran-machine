!
! Program ad hoc: implement quantum computing principles
! Based on an article by Lov K. Grover about the quantum
! search algorithm, DDJ, april 2001
!
module quantum_computing
   type quantum_integer
      integer :: i
      complex :: ampl
   end type quantum_integer

   interface operator(==)
      module procedure quantum_single_is_equal
      module procedure quantum_array_equals_value
   end interface

contains

subroutine q_init( qi )
   type(quantum_integer), dimension(:), intent(inout) :: qi

   integer                                            :: i
   complex                                            :: uniform_ampl

   uniform_ampl = cmplx( 1.0/sqrt(real(size(qi))), 0.0 )
   do i = 1,size(qi)
      qi(i)%i    = i
      qi(i)%ampl = uniform_ampl
   enddo
end subroutine q_init

logical function quantum_single_is_equal( qi, iv )
   type(quantum_integer), intent(in) :: qi
   integer, intent(in)               :: iv

   quantum_single_is_equal = qi%i == iv
end function quantum_single_is_equal

function quantum_array_is_equal( qi, iv ) &
   result(log_array)
   type(quantum_integer), dimension(:), intent(in) :: qi
   integer, dimension(:), intent(in)               :: iv

   logical, dimension(1:size(qi))                  :: log_array
   integer                                         :: i

   do i = 1,size(qi)
      log_array(i) = qi(i)%i == iv(i)
   enddo
end function quantum_array_is_equal

function quantum_array_equals_value( qi, iv ) &
   result(log_array)
   type(quantum_integer), dimension(:), intent(in) :: qi
   integer, intent(in)                             :: iv

   logical, dimension(1:size(qi))                  :: log_array
   integer                                         :: i

   do i = 1,size(qi)
      log_array(i) = qi(i)%i == iv
   enddo
end function quantum_array_equals_value

function q_random( nostates, qi ) &
   result( qr )

   type(quantum_integer), dimension(:), intent(in) :: qi
   type(quantum_integer), dimension(1:size(qi))    :: qr
   integer, intent(in)                             :: nostates

   integer :: i
   integer :: j
   integer :: k
   integer :: and_bits
   complex :: new_ampl
   complex :: signed_ampl

   new_ampl = cmplx( 1.0/sqrt(real(nostates)), 0.0 )

   do i = 1,nostates
      qr(i)%i = i
      qr(i)%ampl = (0.0,0.0)
      do j = 1,nostates
         signed_ampl = new_ampl
         and_bits    = iand( qi(j)%i-1, qr(i)%i-1 )
         do k = 0,bit_size(and_bits)-1
            if ( btest(and_bits,k) ) signed_ampl = -signed_ampl
         enddo
         qr(i)%ampl = qr(i)%ampl + signed_ampl * qi(j)%ampl
      enddo
   enddo

end function q_random

function q_invert_phase( qi ) &
   result( qr )

   type(quantum_integer), dimension(:), intent(in) :: qi
   type(quantum_integer), dimension(1:size(qi))    :: qr

   integer :: i

   do i = 1,size(qi)
      qr(i)%i = i
      qr(i)%ampl = -qi(i)%ampl
   enddo

end function q_invert_phase

integer function q_observe( qi )
   type(quantum_integer), dimension(:) :: qi

   integer :: i
   real    :: rnd
   real    :: probability

   do i = 1,20
      call random_number( rnd )
   enddo

   probability = 0.0
   do i = 1,size(qi)
      probability = probability + abs( qi(i)%ampl ) ** 2
      if ( probability > rnd ) then
         q_observe = qi(i)%i
         exit
      endif
   enddo

   do i = 1,size(qi)
      if ( qi(i)%i /= q_observe ) then
         qi(i)%ampl = (0.0,0.0)
      else
         qi(i)%ampl = (1.0,0.0)
      endif
   enddo
end function q_observe

end module quantum_computing

program quantum_search
   use quantum_computing
   implicit none

   integer, parameter                           :: nostates = 4
   integer, parameter                           :: nosteps  = 1
   type(quantum_integer), dimension(1:nostates) :: qi
   integer, dimension(1:nostates)               :: iv
   integer                                      :: i
   integer                                      :: j

   !
   ! Initialise the quantum "system"
   !
   call q_init( qi )
   write(*,'(a,10f5.2)' ) 'Init', ( real(qi(j)%ampl), j = 1,nostates )

   !
   ! The test function is represented as an array
   !
   iv = 0
   iv(3) = 1

   !
   ! Preliminary randomisation - done by q_init()
   !
   !qi = q_random( nostates, qi )
   !write(*,'(a,10f5.2)' ) 'FR', ( real(qi(j)%ampl), j = 1,nostates )

   !
   ! Find the correct state
   ! Note:
   ! Counting starts at 1, not 0.
   !
   do i = 1,nosteps
      where ( iv == 1 )
         qi = q_invert_phase( qi )
      endwhere

      write(*,'(a,10f5.2)' ) 'First', ( real(qi(j)%ampl), j = 1,nostates )

      qi = q_random( nostates, qi )
      write(*,'(a,10f5.2)' ) 'Random', ( real(qi(j)%ampl), j = 1,nostates )

      where ( qi == 1 )
         qi = q_invert_phase( qi )
      endwhere
      write(*,'(a,10f5.2)' ) 'Second', ( real(qi(j)%ampl), j = 1,nostates )

      qi = q_random( nostates, qi )
      write(*,'(a,10f5.2)' ) 'SR', ( real(qi(j)%ampl), j = 1,nostates )
   enddo

   write(*,*) q_observe( qi )

   stop
end program quantum_search
