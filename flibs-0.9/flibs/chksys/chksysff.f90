! DOC
!
!  chksysff.f90 - program to check the run-time environment for
!                 FORTRAN 90 programs (just the new aspects)
!
!  Copyright (C) 1998 Arjen Markus
!
!  Arjen Markus
!
!
!  General information:
!  This file contains the following routines:
!  - main:               Main program
!  - WriteMessage:       Write the text of the messages to the screen
!  - WriteLog:           Write the value of a logical to the screen
!  - CheckSet:           Check if a certain test should be done or not
!  - ListKinds:          List all integer and real kinds
!  - CharOrder:          Check the collating sequence for characters
!  - CheckUndefs:        Check what "undefined" means for pointers and
!                        allocatable arrays
!  - CheckRandomNumber:  Check the uniformity of the random number
!                        generator
!  - CheckArrayAssign:   Check various array assignments
!  - CheckPtrAlloc:      Check the properties of pointers and allocatables
!
! ENDDOC
!
! $Author: arjenmarkus $
! $Date: 2008/08/10 14:48:48 $
! $Source: /cvsroot/flibs/chksys/chksysff.f90,v $
! $Log: chksysff.f90,v $
! Revision 1.1  2008/08/10 14:48:48  arjenmarkus
! Correcting omission: chksysff was not previously committed; added small test regarding hexadecimal and binary constants
!
!
!
! --------------------------------------------------------------------
!  Routine:  chksysff
!  Author:   Arjen Markus
!  Purpose:  Main program
!  Context:  -
!  Summary:
!            Work through the various checks:
!            - List all integer and real kinds
! ---------------------------------------------------------------------
!
program chksysff

!
! Always use "implicit none"
!
   implicit none
!
! Local variables
!
   integer , dimension(1:10)           :: array
   integer , dimension(:)    , pointer :: ptr
   logical                             :: dotest

   call WriteMessage( '@INTRODUCTION' )
!
! General (and safe) tests
!
   call CheckSet( '@GENERAL' , dotest )

   if ( dotest ) then
!
! List all integer and real kinds
!
      call ListKinds
!
! Check the collating sequence for characters
!
      call CharOrder
!
! Check the uniformity of the random number generator
!
      call CheckRandomNumber
!
! Check what "undefined" means for pointers and allocatable arrays
!
      call CheckUndefs
!
! Check various (more or less complicated) array assignments
!
      call CheckArrayAssign
   endif
!
! The next tests may disrupt the program.
! So the routines check whether to perform them.
!
! Check properties of pointers and allocatables
!
   call CheckPtrAlloc
!
! End of program
!
   write( * , * ) ' '
   stop 'End of program'

contains

! --------------------------------------------------------------------
!   Routine:  WriteMessage
!   Author:   Arjen Markus
!   Purpose:  Write the message that belongs to a keyword
!   Context:  Used to print the messages
!   Summary:
!             If this is the first call, read the file "chksysff.msg"
!             In all cases: look for a line starting with the keyword
!             then write all lines that follow up to the next keyword.
!   Arguments:
!   Name      Type I/O  Descrption
!   KEYWRD    CHAR I    Keyword with which the message is identified
! --------------------------------------------------------------------
!
subroutine WriteMessage( keywrd )
!
   implicit none
   integer  mxmesg
   parameter ( mxmesg = 800 )
!
   character*(*) keywrd
!
   integer      init   , nomesg , ierr   , ifound , iprint , i
   character*75 messag(mxmesg)
!
   save         messag
   save         init   , nomesg
   data         init   , nomesg / 1 , 0 /
!
! -------- At the first call, read the messages file
!
   if ( init   .eq. 1 ) then
      init   = 0
      open( 10 , file   = 'chksysff.msg' , status = 'old' , &
                 iostat = ierr                            )
      if ( ierr   .eq. 0 ) then
         do i = 1,mxmesg
            read( 10 , '(a)' , iostat = ierr ) messag(i)
!
            if ( ierr  .eq. 0 ) then
               nomesg = nomesg + 1
            else
               exit
            endif
         enddo
!
         close( 10 )
      else
         write( * , * ) &
           'could not read messages file - printing keywords only'
      endif
   endif
!
! -------- find the keyword, print the message
!
   ifound = 0
   iprint = 0
   do i = 1,nomesg
      if ( messag(i)(1:1) .eq. '@'    ) iprint = 0
      if ( iprint         .eq. 1      ) &
         write( * , '(1x,a)' ) messag(i)
      if ( messag(i)      .eq. keywrd ) then
         ifound = 1
         iprint = 1
      endif
   enddo
!
   if ( ifound .eq. 0 ) &
      write( * , * ) '(Message not found) ' , keywrd
!
   return
end subroutine WriteMessage

! --------------------------------------------------------------------
!   Routine:  WriteLog
!   Author:   Arjen Markus
!   Purpose:  Write the value of a logical to the screen
!   Context:  Used by various routines
!   Summary:
!             Write TRUE or FALSE together with a string to
!             the screen
!   Arguments:
!   Name      Type I/O  Descrption
!   MESSG     CHAR I    Text to write
!   VALUE     LOG  I    Value of the logical
! --------------------------------------------------------------------
!
   subroutine WriteLog( messg  , value  )
!
   implicit none
!
   character*(*) messg
   logical       value
!
   if ( value ) then
      write( * , * ) messg , ' TRUE'
   else
      write( * , * ) messg , ' FALSE'
   endif

   return
end subroutine WriteLog

! --------------------------------------------------------------------
!   Routine:  CheckSet
!   Author:   Arjen Markus
!   Purpose:  Check whether a certain type of tests should be done
!   Context:  Used to select test sets
!   Summary:
!             If this is the first call, read the file "chksysff.set"
!             In all cases: look for a line starting with the keyword.
!             If it exists, then set the second argument to true.
!   Arguments:
!   Name      Type I/O  Descrption
!   KEYWRD    CHAR I    Keyword to look for
!   CHECK     LOG  O    Whether to perform these tests or not
! --------------------------------------------------------------------
!
   subroutine CheckSet( keywrd , check  )
!
   implicit none
!
   integer       mxkeyw
   parameter (   mxkeyw = 50 )
!
   character*(*) keywrd
   logical       check
!
   integer      init   , nokeyw , ierr   , length , i      , i2
   character*25 keyw(mxkeyw)
!
   save         keyw
   save         init   , nokeyw
   data         init   , nokeyw / 1 , 0 /
!
! -------- at the first call, read the keywords file:
!          a line starting with a # is comment
!
   if ( init   .eq. 1 ) then
      init   = 0
      open( 10 , file   = 'chksysff.set' , status = 'old' , &
                 iostat = ierr                          )
      if ( ierr   .eq. 0 ) then
         i2     = 1
         do i = 1,mxkeyw
            read( 10 , '(a)' , iostat = ierr ) keyw(i2)
!
            if ( ierr  .eq. 0 ) then
               if ( keyw(i2)(1:1)   .ne. '#' ) then
                  i2     = i2     + 1
                  nokeyw = nokeyw + 1
               endif
            else
               exit
            endif
         enddo
!
         close( 10 )
      else
         write( * , * ) &
           'could not read keywords file - general test only'
         nokeyw    = 1
         keyw(1)   = '@general'
      endif
   endif
!
! -------- find the keyword, print the message
!
   check = .false.
!
   do i = 1,nokeyw
      length = len( keywrd )
      if ( keyw(i)(1:length) .eq. keywrd ) then
         check  = .true.
         exit
      endif
   enddo
!
   if ( .not. check ) write( * , * ) '(skipping: ' , keywrd , ')'
!
   return
end subroutine CheckSet

! --------------------------------------------------------------------
!  Routine:  ListKinds
!  Author:   Arjen Markus
!  Purpose:  List all integer and real kinds
!  Context:  Used by main program
!  Summary:
!            Assume that the integers can have a range up 10^100.
!            Try to detect all different types by enumerating the
!            ranges. Store the different kinds with their range.
!
!            Do something similar for the reals. However, we need
!            two ranges now: one for the precision, one for the
!            exponent.
!  Note:
!            The original algorithm of finding out the kinds by
!            construing values of these kinds failed, because of
!            the requirement that the kind parameter in INT() and
!            REAL() be a constant.
! ---------------------------------------------------------------------
!
subroutine ListKinds
   implicit none

   integer , parameter              :: max_kinds = 100
   integer , dimension(1:max_kinds) :: range_int  , kind_int
   integer , dimension(1:max_kinds) :: range_real , kind_real
   integer , dimension(1:max_kinds) :: prec_real

   double precision                 :: dbl_val

   logical                          :: found
   integer                          :: i , j , k
   integer                          :: no_kinds , new_kind

   integer                          :: max_range_int  = 100
   integer                          :: max_range_real = 1000
   integer                          :: max_prec_real  = 50

!
! The integers are simple enough
!
   call WriteMessage( '@LIST-KINDS' )
   write( * , '(1X,A)' ) 'Integer kinds:'
   write( * , '(1X,A,I5,A,I20)'  ) &
                         'Default kind:'     , KIND( 0 ) , &
                         ' - largest value:' , HUGE( 0 )
!
! Enumerate the kinds of integers by asking what kind is required
! for a certain range.
!
   no_kinds = 0
   do i = 1,max_range_int
      new_kind = selected_int_kind( i )

!
! Only continue if we have found a new kind!
!
      if ( new_kind == -1 ) then
         exit
      endif

      found = .false.
      do k = 1,no_kinds
         if ( new_kind == kind_int(k) ) then
            found        = .true.
            range_int(k) = max( range_int(k) , i )
            exit                                   ! Look at next range
         endif
      enddo
!
! Have we found a new kind?
!
      if ( .not. found ) then
         no_kinds = no_kinds + 1
         kind_int(no_kinds)  = new_kind
         range_int(no_kinds) = i
      endif
   enddo

!
! Print them
!
   write(*,'(1X,A,A5,A10)' ) '     ' , '     ' , '     Range'
   do k = 1,no_kinds
      write(*,'(1X,A,I5,5X,A,I3)' ) 'Kind ' , kind_int(k) , &
         '10**' , range_int(k)
   enddo

!
! Check the kinds for hexadecimal and binary constants
!
   write(*,'(/,1X,A)'    ) 'Kinds for hexadecimal and binary literals:'
   write(*,'(  1X,A,I5)' ) 'Hexadecimal: Z''1000'' - kind: ', kind(z'1000')
   write(*,'(  1X,A,I5)' ) 'Binary:      B''1000'' - kind: ', kind(b'1000')

!
! Enumerate the kinds of reals by asking what kind is required
! for a certain range. The difficulty is that we have to scan
! in two dimensions.
!
! Note:
! Some types of reals have a ridiculously large range for the exponent
! To cover this (and similarly for the precision), estimate an upper
! limit first.
!
   do
      new_kind = selected_real_kind( 1 , max_range_real )
      if ( new_kind < 0 ) then
         exit
      else
         max_range_real = max_range_real + 500
      endif
   enddo

   do
      new_kind = selected_real_kind( max_prec_real , 1 )
      if ( new_kind < 0 ) then
         exit
      else
         max_prec_real = max_prec_real + 5
      endif
   enddo

   dbl_val = 0.0D00
   write( * , '(/,1X,A)'  ) 'Real kinds:'
   write( * , '(1X,A,I5)' ) 'Default kind:     ' , KIND( 0.0 )
   write( * , *           ) '   Largest value: ' , HUGE( 0.0 )
   write( * , *           ) '   Smallest value:' , TINY( 0.0 )
   write( * , *           ) '   Precision:     ' , PRECISION( 0.0 )
   write( * , '(1X,A,I5)' ) 'Double precision: ' , KIND( dbl_val )
   write( * , *           ) '   Largest value: ' , HUGE( dbl_val )
   write( * , *           ) '   Smallest value:' , TINY( dbl_val )
   write( * , *           ) '   Precision:     ' , PRECISION( dbl_val )

   no_kinds = 0

real_loop: &
   do i = 1,max_range_real
prec_loop: &
      do j = 1,max_prec_real
         new_kind = selected_real_kind( j , i )

!
! Only continue if we have found a new kind!
!
         if ( new_kind == -3 ) then
            exit real_loop
         endif
         if ( new_kind <   0 ) then
            cycle prec_loop
         endif

         found = .false.
         do k = 1,no_kinds
            if ( new_kind == kind_real(k) ) then
               found        = .true.
               range_real(k) = max( range_real(k) , i )
               prec_real(k)  = max( prec_real(k)  , j )
               exit                                   ! Look at next range
            endif
         enddo
!
! Have we found a new kind?
!
         if ( .not. found ) then
            no_kinds = no_kinds + 1
            kind_real(no_kinds)  = new_kind
            range_real(no_kinds) = i
            prec_real(no_kinds)  = j
         endif
      enddo prec_loop
   enddo real_loop

!
! Print them
!
   write(*,'(1X,A,A5,2A)' ) '     ' , '     ' , '     Range' , &
                                                '          Precision'
   do k = 1,no_kinds
      write(*,'(1X,A,I5,A,I5,I15)' ) 'Kind ' , kind_real(k) , &
         '     10**' , range_real(k) , prec_real(k)
   enddo

   write( * , * )

   return
end subroutine ListKinds

! --------------------------------------------------------------------
!  Routine:  CharOrder
!  Author:   Arjen Markus
!  Purpose:  Check the collating sequence for characters
!  Context:  Used by main program
!  Summary:
!            The Standard defines a collating sequence for the characters
!            in the FORTRAN character set that follows ASCII. Strictly
!            speaking you need to use LLE() and others for alphabetic-
!            lexicographic comparions. Related functions are IACHAR().
!            The classical ICHAR() function returns values that are
!            implementation dependent.
!            This subroutine checks whether ASCII is used for the default
!            characters and whether two identities hold - as they should.
! ---------------------------------------------------------------------
!
subroutine CharOrder
   implicit none

!
! These are the characters in the default FORTRAN character set
! that are guaranteed to be useable.
!
   character , parameter            :: fortran_chars = &
      '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ&
      &_ =+-*/(),;"'':!&%<>?$'

   character                        :: one_char
   integer                          :: i
   logical                          :: ansi_seq
   logical                          :: ichar_ident
   logical                          :: iachar_ident
!
! Check whether the functions ICHAR() and IACHAR() return the same
! value. Also check the relation CHAR(ICHAR(c) = c and
! ACHAR(IACHAR(c) = c
!
   call WriteMessage( '@CHAR-PROPERTIES' )

   ansi_seq     = .true.
   ichar_ident  = .true.
   iachar_ident = .true.

   do i = 1,len(fortran_chars)
      one_char = fortran_chars(i:i)

      if ( ichar( one_char ) /= iachar( one_char ) ) then
         ansi_seq     = .false.
      endif

      if ( char( ichar( one_char ) ) /= one_char ) then
         ichar_ident  = .false.
      endif

      if ( achar( iachar( one_char ) ) /= one_char ) then
         iachar_ident = .false.
      endif
   enddo

   if ( ansi_seq ) then
      write( * , '(1X,A)' ) &
         'The present implementation uses the ANSI representation'
   else
      write( * , '(1X,A)' ) &
         'The present implementation uses a different representation than ANSI'
   endif

   write( * , * )

   if ( ichar_ident ) then
      write( * , '(1X,A)' ) &
         'The present implementation guarantees the identity:' ,         &
         '   CHAR( ICHAR( c ) ) == c'
   else
      write( * , '(1X,A)' ) &
         'The present implementation does not guarantee the identity:' , &
         '   CHAR( ICHAR( c ) ) == c' , &
         'This is not compliant!'
   endif

   write( * , * )

   if ( iachar_ident ) then
      write( * , '(1X,A)' ) &
         'The present implementation guarantees the identity:' ,         &
         '   ACHAR( IACHAR( c ) ) == c'
   else
      write( * , '(1X,A)' ) &
         'The present implementation does not guarantee the identity:' , &
         '   ACHAR( IACHAR( c ) ) == c' , &
         'This is not compliant!'
   endif

   write( * , * )

   return
end subroutine CharOrder

! --------------------------------------------------------------------
!  Routine:  CheckUndefs
!  Author:   Arjen Markus
!  Purpose:  Check what "undefined" means for pointers and allocatable
!            arrays
!  Context:  Used by main program
!  Summary:
!            The Standard says that uninitialised pointers and allocatable
!            arrays have an "undefined" status. If this means that the
!            ASSOCIATED() and ALLOCATED() functions return random values
!            (or TRUE), you must be careful!
!            Also certain limitations hold for deallocations.
!            This subroutine checks:
!            - whether the "undefined" status seems systematically
!              FALSE or not
!            - whether illegal deallocations are flagged
! ---------------------------------------------------------------------
!
subroutine CheckUndefs
   implicit none

   integer , pointer     , dimension(:)          :: ptr_int1 , ptr_int2
   integer , allocatable , dimension(:) , target :: array_int1 , &
                                                    array_int2
   integer                                       :: ierr
   logical                                       :: status1  , status2
!
! Check if the same FALSE value is returned or not:
! - for ASSOCIATED() on uninitialised pointers
! - for ALLOCATED() on uninitialised allocatable arrays
!
   call WriteMessage( '@UNDEFINED-POINTERS' )

   status1 = associated( ptr_int1 )
   status2 = associated( ptr_int2 )

   call WriteMessage( '@UNDEFINED-POINTERS-STATE' )
   if ( status1 .or. status2 ) then
      call WriteMessage( '@UNDEFINED-POINTERS-TRUE' )
   else
      call WriteMessage( '@UNDEFINED-POINTERS-FALSE' )
   endif

   status1 = allocated( array_int1 )
   status2 = allocated( array_int2 )

   call WriteMessage( '@UNDEFINED-ALLOCATED-STATE' )
   if ( status1 .or. status2 ) then
      call WriteMessage( '@UNDEFINED-ALLOCATED-TRUE' )
   else
      call WriteMessage( '@UNDEFINED-ALLOCATED-TRUE' )
   endif

!
! Associate a pointer to an unallocated allocatable array
!
   call WriteMessage( '@UNDEFINED-POINTERS-ASSOC' )
   ptr_int1 => array_int1
   if ( associated( ptr_int1 ) ) then
      call WriteMessage( '@UNDEFINED-ASSOC-SUCCESS' )
   else
      call WriteMessage( '@UNDEFINED-ASSOC-FAILURE' )
   endif

!
! Allocate an allocatable array, try to deallocate via a pointer
!
! See CheckPtrAlloc()
!  allocate( array_int1(1:10) , stat = ierr )
!  if ( ierr .ne. 0 ) then
!     write( * , '(1x,a)' ) &
!        'Error: allocating a small array - stopping program'
!     stop 'Error while allocating a small array'
!  endif
!
!
! Note: on HP this causes asynchronous messages and an abort!
!       Even though "ierr" stays zero
!
!  ptr_int1 => array_int1
!
!  deallocate( ptr_int1 , stat = ierr )
!  if ( ierr .ne. 0 ) then
!     write( * , '(1x,a)' ) &
!        'Error occurred while deallocating via a pointer - this was expected'
!  else
!     write( * , '(1x,a)' ) &
!        'No error was reported while deallocating via a pointer. Be very careful here!'
!  endif

!
! Note: the HP gave run-time errors on simply:
!
!  deallocate( array_int1 , stat = ierr )
!
! but also on:
!  if ( allocated( array_int1 ) then
!     deallocate( array_int1 , stat = ierr )
!  endif
!
! Leave it for now

   write( * , * )

   return
end subroutine CheckUndefs

! --------------------------------------------------------------------
!  Routine:  CheckRandomNumber
!  Author:   Arjen Markus
!  Purpose:  Check the uniformity of the random number generator
!  Context:  Used by main program
!  Summary:
!            The Standard defines a random number function that should
!            produce uniformly distributed random numbers.
!            It does not prescribe an algorithm for this. So any
!            implementation may have its own.
!            Via a simple statistical check we test the quality of the
!            default generator.
! ---------------------------------------------------------------------
!
subroutine CheckRandomNumber
   implicit none

   integer , parameter                          :: no_class = 10
   integer , dimension(1:no_class)              :: class_count
   real    , dimension(1:no_class)              :: class_average
   real    , dimension(1:no_class)              :: class_stddev

   integer                                      :: i
   integer                                      :: no_samples = 10000
   integer                                      :: class_id
   real                                         :: sample
   real                                         :: step
   real                                         :: typical_err , exp_average
   real                                         :: upper_limit , lower_limit
   logical                                      :: outside
!
! Print the first ten random numbers. Then select "no_samples" random
! numbers
!
   call WriteMessage( '@RANDOM-NUMBERS' )

   write( * , '(1x,a)' ) 'The first ten random numbers:'
   do i = 1,10
      call random_number( sample )
      write( * , '(1x,i5,a,f10.5)' ) i , ': ', sample
   enddo

   step = 1.0 / no_class

   do i = 1,no_class
      class_count(i)   = 0
      class_average(i) = 0.0
      class_stddev(i)  = 0.0
   enddo

   do i = 1,no_samples
      call random_number( sample )
      class_id = 1 + sample / step
      if ( class_id <= 0        ) class_id = 1
      if ( class_id >= no_class ) class_id = no_class
      class_count(class_id)   = class_count(class_id)   + 1
      class_average(class_id) = class_average(class_id) + sample
      class_stddev(class_id)  = class_stddev(class_id)  + sample ** 2
   enddo

   do i = 1,no_class
      if ( class_count(i) /= 0 ) then
         class_average(i) = class_average(i) / class_count(i)
         class_stddev(i)  = &
            sqrt( class_stddev(i) - &
                  class_count(i)  * class_average(i) ** 2 ) / &
            sqrt( float ( class_count(i)  - 1 ) )
      endif
   enddo

   write( * , '(1x,a)' ) &
         'Statistical result of the random numbers:' , &
         'Class                 Average    Std.dev.   Number'
   do i = 1,no_class
      if ( class_count(i) /= 0 ) then
         write( * , '(1x,f10.2,"-",f10.2,1x,f10.2,1x,f10.5,1x,i6)' ) &
            (i-1) * step , i * step , &
            class_average(i) , class_stddev(i) , class_count(i)
      else
         write( * , '(1x,f10.2,"-",f10.2,1x,a)' ) &
            (i-1) * step , i * step , 'No samples'
      endif
   enddo

   write( * , '(1x,a,f10.5)' ) &
         'Expected standard deviation: ' , step / sqrt( 12.0 )

   outside = .false.
   do i = 1,no_class
      if ( class_count(i) /= 0 ) then
         typical_err = 3.0 * class_stddev(i) / sqrt( real( class_count(i) ) )
         lower_limit = class_average(i) - typical_err
         upper_limit = class_average(i) + typical_err
         exp_average = ( i - 0.5 ) * step
         if ( exp_average >= lower_limit .and. &
              exp_average <= upper_limit       ) then
            write( * , '(1x,f10.2," - ",f10.2,1x,f10.2,f10.4,1x,a)' ) &
               (i-1) * step , i * step , &
               class_average(i) , class_stddev(i) , 'within range'
         else
            write( * , '(1x,f10.2," - ",f10.2,1x,a,2f10.5)' ) &
               (i-1) * step , i * step , &
               'outside range' , lower_limit , upper_limit
            outside = .true.
         endif
      else
         write( * , '(1x,f10.5,"-",f10.2,1x,a)' ) &
            (i-1) * step , i * step , 'No samples'
      endif
   enddo

!
! The conclusion
!
   if ( outside ) then
      call WriteMessage( '@RANDOM-NUMBERS-SUCCESS' )
   else
      call WriteMessage( '@RANDOM-NUMBERS-FAILURE' )
   endif

   return
end subroutine CheckRandomNumber

! --------------------------------------------------------------------
!  Routine:  CheckArrayAssign
!  Author:   Arjen Markus
!  Purpose:  Check various array assignments
!  Context:  Used by main program
!  Summary:
!            FORTRAN 90 allows all kinds of compilcated array assignments.
!            Sometimes these are difficult to read or interpret.
!            So, probably it is not simple for the compiler either.
!            This routine checks the result of several such assignments.
!            Of course, the check is not conclusive, unless at least
!            one test fails.
! ---------------------------------------------------------------------
!
subroutine CheckArrayAssign

   implicit none
   integer , parameter                          :: notstmx = 10
   integer                                      :: i , j , notest
   integer , dimension(1:10)                    :: array
   integer , dimension(1:10,1:notstmx)          :: expected
   character(len=50), dimension(1:notstmx)      :: description
   logical                                      :: success

!
! Fill the array expected with the expected sequence
!
   data description(1) , ( expected(i,1) ,i=1,10 ) / &
      'Simple copy - a = b'                        , &
      1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 , 9 , 10 /
   data description(2) , ( expected(i,2) ,i=1,10 ) / &
      'Left shift - a(1:9) = a(2:10)'              , &
      2 , 3 , 4 , 5 , 6 , 7 , 8 , 9 , 10 , 10 /
   data description(3) , ( expected(i,3) ,i=1,10 ) / &
      'Right shift - a(2:10) = a(1:9)'             , &
      1 , 1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 , 9 /
   data description(4) , ( expected(i,4) ,i=1,10 ) / &
      'Reversion - a(1:10) = a(10:1:-1)'           , &
      10 , 9 , 8 , 7 , 6 , 5 , 4 , 3 , 2 , 1 /
   data description(5) , ( expected(i,5) ,i=1,10 ) / &
      'Even indices copied - a(1:10:2) = a(2:10:2)', &
       2 , 2 , 4 , 4 , 6 , 6 , 8 , 8 , 10 , 10 /
   data description(6) , ( expected(i,6) ,i=1,10 ) / &
      'Even/odd indices via WHERE'                 , &
       -1 , 1 , -1 , 1 , -1 , 1 , -1 , 1 , -1 , 1 /
   data description(7) , ( expected(i,7) ,i=1,10 ) / &
      'Folded array - a(1:5) = a(10:6:-1)'         , &
       10 , 9 , 8 , 7 , 6 , 6 , 7 , 8 , 9 , 10 /

   data notest / 7 /

!
! Perform the assignments and check the results
!
   call WriteMessage( '@ARRAY-ASSIGNMENT' )

   do i = 1,notest

      array = expected(1:10,1)

      select case ( i )

      case ( 1 )
         array = expected(1:10,1)
      case ( 2 )
         array(1:9)  = array(2:10)
      case ( 3 )
         array(2:10) = array(1:9)
      case ( 4 )
         array(1:10) = array(10:1:-1)
      case ( 5 )
         array(1:10:2) = array(2:10:2)
      case ( 6 )
         where ( mod(array,2) .eq. 0 )
            array = 1
         elsewhere
            array = -1
         endwhere
      case ( 7 )
         array(1:5) = array(10:6:-1)
      case default
         write( * , * ) 'Case not implemented: ' , i
      endselect

      success = .true.
      do j = 1,10 ! The ALL() function does not like to use a 1D and a 2D array
         if ( array(j) .ne. expected(j,i) ) then
            success = .false.
         endif
      enddo
      if ( success ) then
         write( * , '(1x,a,a)' ) description(i) , ' passed successfully'
      else
         write( * , '(1x,a,a)'    ) description(i) , ' failed!'
         write( * , '(4x,a,10i3)' ) 'Expected: '   , expected(1:10,i:i)
         write( * , '(4x,a,10i3)' ) 'Actually: '   , array
      endif
   enddo

   write( * , * )

   return
end subroutine CheckArrayAssign

! --------------------------------------------------------------------
!  Routine:  CheckPtrAlloc
!  Author:   Arjen Markus
!  Purpose:  Check properties of pointers and allocatables
!  Context:  Used by main program
!  Summary:
!            FORTRAN 90 allows memory to be assigned to pointers and
!            to allocatable variables/arrays. The manipulations are
!            not always trivial and the precise restrictions are
!            somewhat fuzzy (unless you read the long list in the
!            standard and understand it!).
! ---------------------------------------------------------------------
!
subroutine CheckPtrAlloc

   implicit none
   integer                                      :: ierr
   real , pointer , dimension(:)                :: ptor
   real , target  , dimension(:) , allocatable  :: allocr , allocr2
   logical                                      :: dotest

!
! First establish the defaults
!
   call WriteMessage( '@ALLOCATED-POINTERS' )

   call CheckSet( '@ALLOCATED-POINTERS' , dotest )
   if ( .not. dotest ) then
      call WriteMessage( '@ALLOCATED-POINTERS-SKIPPED' )
   else
      call WriteMessage( '@ALLOCATED-POINTERS-STEP1' )
      call WriteLog( 'Default status pointer:     ' , associated( ptor ) )
      call WriteLog( 'Default status allocatable: ' , allocated( allocr ) )
!
! Allocate some memory and deallocate via pointer
!
      allocate( allocr(1:10) )
      ptor => allocr
      call WriteLog( 'Status associated pointer:  ' , associated( ptor ) )
!
! allocated() not allowed on pointer - alas!
!  write( * , * ) 'Status allocated pointer:   ' , allocated(  ptor   )
!
      deallocate( ptor   , stat = ierr   )
      if ( ierr .eq. 0 ) then
         call WriteMessage( '@ALLOCATED-STEP1-SUCCESS' )
         if ( allocated( allocr ) ) then
            call WriteMessage( '@ALLOCATED-STEP1-WARNING' )
         !
         ! VAST/f90: gives run-time error. But trying to re-allocate
         ! always gives a run-time error.
         ! deallocate( allocr )
         else
            call WriteMessage( '@ALLOCATED-STEP1-NOALLOC' )
         endif
      else
         call WriteMessage( '@ALLOCATED-STEP1-FAILURE' )
         deallocate( allocr )
      endif
!
! Allocate memory, deallocate: what does the pointer point to?
!
      call WriteMessage( '@ALLOCATED-POINTERS-STEP2' )
!
! VAST/f90: gives run-time error. But trying to re-allocate
!  allocate( allocr(1:10) )
!
! Using a different target instead!
!
      nullify( ptor   )
      allocate( allocr2(1:10) )
      ptor => allocr2
      deallocate( allocr2 )

      call WriteMessage( '@ALLOCATED-STEP2-STATUS' )
      call WriteLog(     '   Status associated pointer:     ' , &
              associated( ptor   ) )
      call WriteLog(     '   Associated to original target: ' , &
              associated( ptor   , allocr2 ) )
!
! Associate pointer to freed target
!
      call WriteMessage( '@ALLOCATED-STEP2-REASSOCIATE' )
      nullify( ptor   )
      ptor   => allocr2
      call WriteLog(     '   Status associated pointer (to freed target):' , &
              associated( ptor ) )
      write( * , * ) ' '
!
! Try to free allocated memory twice
!
      call WriteMessage( '@ALLOCATED-POINTERS-STEP3' )
      deallocate( allocr2 , stat = ierr )
      if ( ierr .eq. 0 ) then
         call WriteMessage( '@ALLOCATED-STEP3-SUCCESS' )
      else
         call WriteMessage( '@ALLOCATED-STEP3-FAILURE' )
      endif

      write( * , * )
   endif ! Not skipped

   return
end subroutine CheckPtrAlloc

end program chksysff
