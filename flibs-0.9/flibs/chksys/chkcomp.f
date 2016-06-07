* @begin@ */
*
*  chkcomp.f(or) - source code to check certain fetaures of the FORTRAN
*                  compiler
*
*  Copyright (C) 1998 Arjen Markus
*
*  Arjen Markus
*
*
*  General information:
*  This file contains source code that uses various extensions to the
*  FORTRAN standard and intentionally bad programming fragments.
*  Comments explain each of them.
*  Its purpose is to check if the compiler (in combination with the
*  options) will flag the features.
*
* @end@
*
*  $Author: arjenmarkus $
*  $Date: 2008/03/17 17:57:56 $
*  $Source: /cvsroot/flibs/chksys/chkcomp.f,v $
*  $Log: chkcomp.f,v $
*  Revision 1.1  2008/03/17 17:57:56  arjenmarkus
*  Added directory "chksys" - programs to probe compiler properties
*
*
* @@------------------------------------------------------------------
*   Routine:  CHKSYS
*   Author:   Arjen Markus
*   Purpose:  Main program
*   Context:  -
*   Summary:
*             Show the properties of the compiler by expressly
*             introducing errors and extensions
* --------------------------------------------------------------------
*
      PROGRAM CHKCMP
*
* -------- A very useful feature: make sure that all variables
*          have to be declared. Unfortunately, it is an extension
*          to the FORTRAN standard
*
      IMPLICIT NONE
*
      CHARACTER*40     STRING
      INTEGER          INTVAL , INTV2   , I
      REAL             REAVAL
      DOUBLE PRECISION DBLVAL
*
* -------- Does the compiler accept:
*          - lowercase letters
*          - underscores in variable names
*          - long variable names
*          - length of data type
*
*          Note:
*          None of these variables are actually used, so it may
*          flag that!
*
      INTEGER   lowcas
      INTEGER   UND_SC
      INTEGER   LONGVARIABLENAME
      INTEGER*4 INT4
      INTEGER*2 INT2
*
* -------- Does the compiler support the INCLUDE-statement?
*
      INCLUDE 'chkcomp.inc'
*
* -------- Some compilers accept:
*          - double quotes (") to delimit strings
*          - C-like escape-sequences
*
      STRING = "Double quote"
      STRING = '\'
*
* -------- Does the compiler register the fact that 
*          we are assigning too large a value? Or that
*          we may loose precision?
*
      INTVAL = INT( 1.0E20 ) 
      INTVAL = 1.0E20
      REAVAL = 1.0E100
      REAVAL = 1.0D100
      DBLVAL = 1.12345678901234567890D00
      REAVAL = DBLVAL
*
* -------- Some compilers do not accept invalid substrings
*          and can quite smart about it
*
      WRITE(*,*) STRING(1:0)
      INTVAL = LEN(STRING) + 1
      WRITE(*,*) STRING(1:INTVAL)
*
* -------- Some compilers accept very long statements
*          (though the number of spaces may be important as well)
*
      STRING =                                             '1' //
     1                                                     '2' //
     2                                                     '3' //
     3                                                     '4' //
     4                                                     '5' //
     5                                                     '6' //
     6                                                     '7' //
     7                                                     '8' //
     8                                                     '9' //
     9                                                     '0' //
     &                                                     'A' //
     1                                                     'B' //
     2                                                     'C' //
     3                                                     'D' //
     4                                                     'E' //
     5                                                     'F' //
     6                                                     'G' //
     7                                                     'H' //
     8                                                     'I' //
     9                                                     'J' //
     &                                                     'K' //
     1                                                     'L'
*
* -------- Use a variable before it is set
*
      INTVAL = INTV2
*
* -------- Check DO-loops and IF-statements:
*          - Can we change the variable?
*          - Can we jump into a DO-loop or an IF block?
*
      DO 100 I = 1,10
         IF ( I      .EQ. 5 ) I      = 6
         WRITE(*,*) I
  100 CONTINUE
*
      IF ( INTVAL .GT.  1 ) GOTO 120
      IF ( INTVAL .LT. -1 ) GOTO 130
*
      DO 110 I = 1,10
  120    CONTINUE
         WRITE(*,*) I
  110 CONTINUE
*
      IF ( INTVAL .GT. 2 ) THEN
  130    CONTINUE
         INTVAL = 10
      ENDIF
*
* -------- Statements not reachable?
*
      GOTO 210
      WRITE(*,*) 'This statement is not reachable'
*
  210 CONTINUE
      IF ( INTVAL .GT. 10 ) THEN
         GOTO 220
      ELSE
         GOTO 230
      ENDIF
      WRITE(*,*) 'Neither is this'
*
  220 CONTINUE
  230 CONTINUE
*
* -------- Check calls to routines (wrong arguments)
*
      CALL SUBR1( INTVAL )
      CALL SUBR2( INTVAL , INTV2  )
*
* -------- Check special comments
*
      CALL PDEBUG
*
      STOP
      END

      SUBROUTINE SUBR1(  REAVAL )
*
* -------- Subroutine has one argument: a real
*
      REAL REAVAL
      WRITE(*,*) 'Real argument:' , REAVAL
      RETURN
      END

      SUBROUTINE SUBR2(  INTVAL )
*
* -------- Subroutine has one argument: an integer
*
      INTEGER INTVAL
      WRITE(*,*) 'Integer argument:' , INTVAL
      RETURN
      END

      SUBROUTINE PDEBUG
*
* -------- Some compilers support the D and ! comments:
*          D: hide/show debug-statements
*          !: in-line comments
*
*          (This code is put at the end because at least one
*          compiler aborts on the D in the first column)
*
      WRITE(*,*) 'PDEBUG'        ! Dummy routine
D     WRITE(*,*) 'In DEBUG mode'
*
      RETURN
      END
