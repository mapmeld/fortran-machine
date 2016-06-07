* @begin@ */
*
*  chksys.f(or) - program to check the run-time environment for FORTRAN
*                 programs
*
*  Copyright (C) 1998 Arjen Markus
*
*  Arjen Markus
*
*
*  General information:
*  This file contains the following routines:
*  - CHKSYS:          Main program
*  - WRMESG:          Write the message that belongs to a keyword
*  - SETVDO:          Set the value of an argument to some larger value
*  - SETVAL:          Set a special value in the argument
*  - CLRSTK:          Clear the stack
*  - GETSTR:          Set the value of a string argument to some value
*  - CHKINT:          Check the length of an integer
*  - CHKREA:          Check the numerical precision of reals
*  - CHKEQR:          Check if two reals are equal
*  - CHKFIL:          Perform tests on file behaviour
*  - CHKNUM:          Set the arguments to specific numerical values
*  - CHKARR:          Check if array bounds are checked
*  - CHKSUB:          Check if substrings are checked
*  - CHKSHF:          Check if substrings can be shifted correctly
*  - CHKSET:          Check whether a certain type of tests should
*                     be done
*  - CHKSTD:          Check if any standard LU-numbers are opened
*
* @end@
*
*  $Author: arjenmarkus $
*  $Date: 2008/03/17 17:57:56 $
*  $Source: /cvsroot/flibs/chksys/chksys.f,v $
*  $Log: chksys.f,v $
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
*             Work through the various checks:
*             - Report the compiler and linker
*             - Identify as far as possible the operating system
*             - Find out what "memory model" is used
*             - Check if binary files are allowed
*             - Check the record length of direct-access files
*             - Check the behaviour of the file I/O in general
*             - Check the behaviour of DO-loops
*             - Perform some questionable operations (such as
*               generating an overflow and using zero-length substrings)
*             Report the results of each step
* --------------------------------------------------------------------
*
      PROGRAM CHKSYS
*
* -------- One common extension to the standard
*
      IMPLICIT NONE
*
      REAL         XLARGE , XSMALL , XINVAL , XZERO  , XNEG
      INTEGER      ITYPE  , IFIRST , ISECND , NOSTP  , IDONE  , I
      INTEGER      NRCBYT , NOBINT , NOFILE , NOSTPM , IERR
      CHARACTER*2  BACKSL
      CHARACTER*1  DIRSEP
      CHARACTER*75 STR1   , STR2
      CHARACTER*20 FORM   , ACCESS
      LOGICAL      LNGNAM , IGNCAS , ALLOWD , CHECK
*
* -------- Report the compiler and linker
*          (to make sure that we know how the program was
*          created)
*
      CALL WRMESG( '@INTRODUCTION')
C     CALL PRMESG
      CALL CHKSET( '@GENERAL' , CHECK )
      IF ( .NOT. CHECK  ) GOTO 500
*
* -------- Identify as far as possible the operating system
*
      CALL WRMESG( '@FILE-SYSTEM' )
      CALL SYQOS(  ITYPE  , LNGNAM , IGNCAS , DIRSEP )
      IF ( ITYPE  .EQ. -1 ) THEN
         CALL WRMESG( '@OS-UNKNOWN' )
      ELSE IF ( ITYPE  .EQ. 1 ) THEN
         CALL WRMESG( '@OS-UNIX' )
      ELSE IF ( ITYPE  .EQ. 2 ) THEN
         CALL WRMESG( '@OS-DOS-WINDOWS' )
      ELSE IF ( ITYPE  .EQ. 3 ) THEN
         CALL WRMESG( '@OS-WINDOWS-95-NT' )
      ELSE
         CALL WRMESG( '@PROGRAM-ERROR' )
      ENDIF
*
      IF ( LNGNAM ) THEN
         CALL WRMESG( '@FILE-SYSTEM-LONG-NAMES' )
      ELSE
         CALL WRMESG( '@FILE-SYSTEM-SHORT-NAMES' )
      ENDIF
*
      IF ( IGNCAS ) THEN
         CALL WRMESG( '@FILE-SYSTEM-IGNORE-CASE' )
      ELSE
         CALL WRMESG( '@FILE-SYSTEM-RESPECT-CASE' )
      ENDIF
*
* -------- Find out what "memory model" is used
*
      CALL SETVAL( IFIRST )
      CALL CLRSTK
      CALL SETVAL( ISECND )
      CALL WRMESG( '@MEMORY')
      IF ( IFIRST .EQ. ISECND ) THEN
         CALL WRMESG( '@MEMORY-STATIC')
      ELSE
         CALL WRMESG( '@MEMORY-DYNAMIC')
      ENDIF
*
* -------- Find out the length of an integer
*
      CALL WRMESG( '@LENGTH-INTEGER' )
      CALL CHKINT( NOBINT )
      WRITE( * , * ) 'Number of bytes in an integer: ' ,
     &   NOBINT
      WRITE( * , * )
*
* -------- Find out the precision of reals
*
      CALL WRMESG( '@NUMERICAL-PRECISION' )
      CALL CHKREA
*
* -------- Find out the unit for record length
*
      CALL WRMESG( '@FILE-DIRECT-ACCESS' )
      CALL SYQREC( NRCBYT )
      WRITE( * , * ) 'Number of bytes in record of length "1": ' ,
     &   NRCBYT
      WRITE( * , * )
*
* -------- Find out if binary files are allowed
*
      CALL WRMESG( '@FILE-BINARY-FILES' )
      CALL SYQBIN( ALLOWD , FORM   , ACCESS )
      IF ( ALLOWD ) THEN
         CALL WRMESG( '@FILE-BINARY-FILES-ALLOWED' )
         WRITE( * , * ) '   FORM   = ''', FORM   , ''''
         WRITE( * , * ) '   ACCESS = ''', ACCESS , ''''
         WRITE( * , * ) ' '
      ELSE
         CALL WRMESG( '@FILE-BINARY-FILES-NOT-ALLOWED' )
      ENDIF
*
* -------- Find out how many files can be opened at once
*          Note:
*          Due to an error in CHKINT (not closing the scratch-file
*          on unit 10) I first got the astonishing result of 0 open
*          files with MicroSoft FORTRAN. May be that should yet another
*          check! Opening a second file to the same unit.
*
      CALL WRMESG( '@NUMBER-OPEN-FILES' )
      NOFILE = 0
      DO 210 I = 10,99
         OPEN( I      , STATUS = 'SCRATCH' , IOSTAT = IERR   )
         IF ( IERR   .EQ. 0 ) THEN
            NOFILE = NOFILE + 1
         ELSE
            GOTO 220
         ENDIF
  210 CONTINUE
*
  220 CONTINUE
      DO 230 I = 10,9+NOFILE
         CLOSE( I      , IOSTAT = IERR   )
         IF ( IERR   .NE. 0 ) WRITE( * , * ) 'Error closing file' , I
  230 CONTINUE
*
      IF ( NOFILE .LT. 90 ) THEN
         CALL WRMESG( '@NUMBER-OPEN-FILES-LIMITED' )
         WRITE( * , * ) 'Number of open files: ' , NOFILE
         WRITE( * , * ) ' '
      ELSE
         CALL WRMESG( '@NUMBER-OPEN-FILES-UNLIMITED' )
      ENDIF
*
* -------- Look for standard files (in the range of LU-number 0-10)
*
      CALL WRMESG( '@FILE-STANDARD' )
      CALL CHKSTD
*
* -------- Find out how files behave
*
      CALL WRMESG( '@FILE-HANDLING' )
      CALL CHKFIL
*
* -------- Check the behaviour of DO-loops:
*          - Can you change the value of a DO-variable?
*          - What if you change the upper limit?
*          - Are DO-loops performed at least once?
*
      CALL WRMESG( '@DO-LOOPS' )
*
      NOSTP  = 0
      DO 310 I = 1,10
         CALL SETVDO( I      )
         NOSTP  = NOSTP + 1
         WRITE( * , * ) 'Step: ' , NOSTP , ' - DO-variable: ' , I
         IF ( NOSTP  .GE. 20 ) GOTO 320
  310 CONTINUE
*
  320 CONTINUE
      WRITE( * , * ) 'Number of steps:     ' , NOSTP
      WRITE( * , * ) 'Value of DO-variable:' , I
      WRITE( * , * )
      IF ( NOSTP .LT. 10 ) THEN
         CALL WRMESG( '@DO-LOOP-ALTERABLE' )
      ENDIF
      IF ( NOSTP .EQ. 10 ) THEN
         CALL WRMESG( '@DO-LOOP-PROTECTED' )
      ENDIF
      IF ( NOSTP .GT. 10 ) THEN
         CALL WRMESG( '@DO-LOOP-INFINITE' )
      ENDIF
*
* -------- The following loop should be done 10 times as well
*
      NOSTP  =  0
      NOSTPM = 10
      CALL WRMESG( '@DO-LOOP-CHANGE-UPPER' )
      DO 350 I = 1,NOSTPM
         IF ( I      .EQ. 4 ) NOSTPM = 6
         NOSTP  = NOSTP + 1
         WRITE( * , * ) 'Step: ' , NOSTP , ' - Upper limit: ' , NOSTPM
         IF ( NOSTP  .GE. 20 ) GOTO 360
  350 CONTINUE
*
  360 CONTINUE
      WRITE( * , * ) 'Number of steps: ' , NOSTP
      WRITE( * , * ) 'Upper limit:     ' , NOSTPM
      WRITE( * , * )
      IF ( NOSTP .LT. 10 ) THEN
         CALL WRMESG( '@DO-LOOP-LIMIT-ALTERABLE' )
      ENDIF
      IF ( NOSTP .EQ. 10 ) THEN
         CALL WRMESG( '@DO-LOOP-LIMIT-PROTECTED' )
      ENDIF
      IF ( NOSTP .GT. 10 ) THEN
         CALL WRMESG( '@DO-LOOP-LIMIT-INFINITE' )
      ENDIF
*
* -------- The following loop should not be performed
*
      CALL WRMESG( '@DO-LOOP-PASS' )
      NOSTP  = 0
      IDONE  = 0
      DO 380 I = 1,NOSTP
         IDONE  = 1
  380 CONTINUE
*
      IF ( IDONE  .EQ. 1 ) THEN
         CALL WRMESG( '@DO-LOOP-PASS-ONCE' )
      ELSE
         CALL WRMESG( '@DO-LOOP-PASS-ZERO' )
      ENDIF
*
* -------- Shifting strings
*
      CALL WRMESG( '@SHIFTING-STRINGS' )
      CALL CHKSHF
*
* -------- Miscellaneous behaviour
*
      CALL WRMESG( '@MISCELLANEOUS1' )
*
      BACKSL = '\\'
      IF ( BACKSL(1:1) .EQ. BACKSL(2:2) ) THEN
         CALL WRMESG( '@BACKSLASH-ASIS' )
      ELSE
         CALL WRMESG( '@BACKSLASH-ESCAPE' )
      ENDIF
*
      CALL WRMESG( '@MISCELLANEOUS2' )
      STR1    = 'String1'
      STR2    = 'String2'
      WRITE( * , * ) STR1
      WRITE( * , * ) STR2
      WRITE( * , * ) 'Number 1 as string: ' , '1'
      WRITE( * , * ) 'Number 1 as number: ' ,  1
      CALL WRMESG( '@TREATMENT-ASTERISK' )
*
*
      CALL WRMESG( '@MISCELLANEOUS3' )
      WRITE( * , '(A)' ) '%1234567890'
      WRITE( * , * )
*
* -------- Possibly disruptive tests
*
  500 CONTINUE
      CALL CHKSET( '@OVERFLOW' , CHECK )
      IF ( CHECK  ) THEN
         CALL WRMESG( '@OVERFLOW' )
         CALL CHKNUM( XLARGE , XSMALL , XZERO  , XNEG   )
         XINVAL = XLARGE / XSMALL
         CALL WRMESG( '@NUMERIC-GENERATE' )
         WRITE( * , * ) 'Value: ' , XINVAL
         XINVAL = 2.0 * XINVAL
         CALL WRMESG( '@NUMERIC-NO-ERROR' )
      ENDIF
*
      CALL CHKSET( '@UNDERFLOW' , CHECK )
      IF ( CHECK  ) THEN
         CALL WRMESG( '@UNDERFLOW' )
         CALL CHKNUM( XLARGE , XSMALL , XZERO  , XNEG   )
         XINVAL = XSMALL / XLARGE
         CALL WRMESG( '@NUMERIC-GENERATE' )
         WRITE( * , * ) 'Value: ' , XINVAL
         XINVAL = 2.0 * XINVAL
         CALL WRMESG( '@NUMERIC-NO-ERROR' )
      ENDIF
*
      CALL CHKSET( '@DIVISION' , CHECK )
      IF ( CHECK  ) THEN
         CALL WRMESG( '@DIVISION' )
         CALL CHKNUM( XLARGE , XSMALL , XZERO  , XNEG   )
         XINVAL = 1.0 / XZERO
         CALL WRMESG( '@NUMERIC-GENERATE' )
         WRITE( * , * ) 'Value: ' , XINVAL
         XINVAL = 2.0 * XINVAL
         CALL WRMESG( '@NUMERIC-NO-ERROR' )
      ENDIF
*
      CALL CHKSET( '@DOMAIN' , CHECK )
      IF ( CHECK  ) THEN
         CALL WRMESG( '@DOMAIN' )
         CALL CHKNUM( XLARGE , XSMALL , XZERO  , XNEG   )
         XINVAL = SQRT( XNEG   )
         CALL WRMESG( '@NUMERIC-GENERATE' )
         WRITE( * , * ) 'Value: ' , XINVAL
         XINVAL = 2.0 * XINVAL
         CALL WRMESG( '@NUMERIC-NO-ERROR' )
      ENDIF
*
      CALL CHKSET( '@ARRAYBOUND' , CHECK )
      IF ( CHECK  ) THEN
         CALL WRMESG( '@ARRAYBOUND' )
         CALL CHKARR( 11 )
         CALL WRMESG( '@ARRAY-NO-ERROR' )
      ENDIF
*
      CALL CHKSET( '@SUBSTRING' , CHECK )
      IF ( CHECK  ) THEN
         CALL WRMESG( '@SUBSTRING' )
         CALL CHKSUB( 0 , 11 )
         CALL WRMESG( '@SUBSTRING-NO-ERROR' )
      ENDIF
*
* -------- End of the program
*
      WRITE( * , * ) ' '
      STOP 'Normal end of program'
*
      END

* @@------------------------------------------------------------------
*   Routine:  WRMESG
*   Author:   Arjen Markus
*   Purpose:  Write the message that belongs to a keyword
*   Context:  Used to print the messages
*   Summary:
*             If this is the first call, read the file "chksys.msg"
*             In all cases: look for a line starting with the keyword
*             then write all lines that follow up to the next keyword.
*   Arguments:
*   Name      Type I/O  Descrption
*   KEYWRD    CHAR I    Keyword with which the message is identified
* --------------------------------------------------------------------
*
      SUBROUTINE WRMESG( KEYWRD )
*
      IMPLICIT NONE
      INTEGER  MXMESG
      PARAMETER ( MXMESG = 800 )
*
      CHARACTER*(*) KEYWRD
*
      INTEGER      INIT   , NOMESG , IERR   , IFOUND , IPRINT , I
      CHARACTER*75 MESSAG(MXMESG)
*
      SAVE         MESSAG
      SAVE         INIT   , NOMESG
      DATA         INIT   , NOMESG / 1 , 0 /
*
* -------- At the first call, read the messages file
*
      IF ( INIT   .EQ. 1 ) THEN
         INIT   = 0
         OPEN( 10 , FILE   = 'chksys.msg' , STATUS = 'OLD' ,
     &              IOSTAT = IERR                          )
         IF ( IERR   .EQ. 0 ) THEN
            DO 110 I = 1,MXMESG
               READ( 10 , '(A)' , IOSTAT = IERR ) MESSAG(I)
*
               IF ( IERR  .EQ. 0 ) THEN
                  NOMESG = NOMESG + 1
               ELSE
                  GOTO 120
               ENDIF
  110       CONTINUE
*
  120       CONTINUE
            CLOSE( 10 )
         ELSE
            WRITE( * , * )
     &        'Could not read messages file - printing keywords only'
         ENDIF
      ENDIF
*
* -------- Find the keyword, print the message
*
      IFOUND = 0
      IPRINT = 0
      DO 210 I = 1,NOMESG
         IF ( MESSAG(I)(1:1) .EQ. '@'    ) IPRINT = 0
         IF ( IPRINT         .EQ. 1      )
     &      WRITE( * , '(1X,A)' ) MESSAG(I)
         IF ( MESSAG(I)      .EQ. KEYWRD ) THEN
            IFOUND = 1
            IPRINT = 1
         ENDIF
  210 CONTINUE
*
      IF ( IFOUND .EQ. 0 )
     &   WRITE( * , * ) '(Message not found) ' , KEYWRD
*
      RETURN
      END

* @@------------------------------------------------------------------
*   Routine:  SETVDO
*   Author:   Arjen Markus
*   Purpose:  Set the value of an argument to some larger value
*   Context:  Used to check the protection of DO-variables
*   Summary:
*             If the argument is equal to 5, set it to 20
* --------------------------------------------------------------------
*
      SUBROUTINE SETVDO( IARG   )
*
      IMPLICIT NONE
*
      INTEGER IARG
*
      IF ( IARG   .EQ. 5 ) THEN
         IARG   = 20
      ENDIF
*
      RETURN
      END

* @@------------------------------------------------------------------
*   Routine:  SETVAL
*   Author:   Arjen Markus
*   Purpose:  Set a special value in the argument
*   Context:  Used to check the memory model (stack used for locals?)
*   Summary:
*             If this is the first call, then set the variable IVALUE
*             Then set the argument
*             If it is the second call, only set the argument (IVALUE
*             may have retained its value)
* --------------------------------------------------------------------
*
      SUBROUTINE SETVAL( IARG   )
*
      IMPLICIT NONE
*
      INTEGER IARG
*
      INTEGER IVALUE , INIT
*
* -------- Only INIT should be SAVEd
*
      SAVE INIT
      DATA INIT / 1 /
*
* -------- Initialisation
*
      IF ( INIT   .EQ. 1 ) THEN
         INIT   = 0
         IVALUE = 12345
      ENDIF
*
* -------- Set the argument
*
      IARG   = IVALUE
*
      RETURN
      END

* @@------------------------------------------------------------------
*   Routine:  CLRSTK
*   Author:   Arjen Markus
*   Purpose:  Clear the stack
*   Context:  Used to check the memory model (stack used for locals?)
*   Summary:
*             By defining a local array we try to access a largish part
*             of the stack. Set the elements to zero, so that any
*             old values are not retained.
* --------------------------------------------------------------------
*
      SUBROUTINE CLRSTK
*
      IMPLICIT NONE
*
      INTEGER IARRAY(100)
      INTEGER I
*
      DO 110 I = 1,100
         IARRAY(I) = 0
  110 CONTINUE
*
      RETURN
      END

* @@------------------------------------------------------------------
*   Routine:  GETSTR
*   Author:   Arjen Markus
*   Purpose:  Set the value of a string argument to some value
*   Context:  Used by CHKFIL to avoid compiler detection of invalid
*             OPEN parameter
*   Summary:
*             Set the argument to 'INVALID!'
* --------------------------------------------------------------------
*
      SUBROUTINE GETSTR( STRING )
*
      IMPLICIT NONE
*
      CHARACTER*(*) STRING
*
      STRING = 'INVALID!'
*
      RETURN
      END

* @@------------------------------------------------------------------
*   Routine:  CHKINT
*   Author:   Arjen Markus
*   Purpose:  Check the length of an integer
*   Context:  Used by CHKFIL to determine the range of an integer
*   Summary:
*             Write a single integer to an unformatted file and
*             read it into a character string. The maximum length
*             of the character string determines the number of bytes.
*   Arguments:
*   Name      Type I/O  Descrption
*   NOBINT    INT  O    Number of bytes in an integer
* --------------------------------------------------------------------
*
      SUBROUTINE CHKINT( NOBINT )
*
      IMPLICIT NONE
*
      INTEGER      NOBINT
*
      CHARACTER*16 STRING
      INTEGER      I      , IERR
*
      OPEN(  10 , STATUS = 'SCRATCH' , FORM= 'UNFORMATTED' ,
     &            IOSTAT = IERR                            )
      IF ( IERR   .NE. 0 ) THEN
         NOBINT = -1
         RETURN
      ENDIF
*
      NOBINT = -1
      WRITE( 10 ) NOBINT
*
      DO 110 I = 1,16
         REWIND( 10 )
         READ(   10 , IOSTAT = IERR   ) STRING(1:I)
         IF ( IERR   .NE. 0 ) THEN
            NOBINT = I      - 1
            GOTO 200
         ENDIF
  110 CONTINUE
*
* -------- If after 16 trials still no error resulted, then
*          we did not get any run-time error. So NOBINT remains -1
*
  200 CONTINUE
      CLOSE( 10 )
      RETURN
      END

* @@------------------------------------------------------------------
*   Routine:  CHKREA
*   Author:   Arjen Markus
*   Purpose:  Check the precision of reals
*   Context:  Used to determine the number of significant decimals
*   Summary:
*             In a loop, calculate 1 + 1.0E-(n) and compare to 1.0.
*             If they are equal, then we have found a rough estimate.
*             Do this for single and double precision
*             Similar DO-loops to test x*y equal y*x,
*             x*y equal (-x)*(-y) and -x equal abs(x) (x<0)
*
*   Arguments:
*             None
* --------------------------------------------------------------------
*
      SUBROUTINE CHKREA
*
      IMPLICIT NONE
*
      INTEGER          NODSIN , NODDBL
      INTEGER          NDSIN2 , NDDBL2
      INTEGER          NODIFS , NODIFD
      INTEGER          I
      REAL             RVAL   , RVAL2   , SPROD1 , SPROD2
      DOUBLE PRECISION DVAL   , DVAL2   , DPROD1 , DPROD2
      LOGICAL          EQSNGL , EQDBLE
*
      NODSIN = -1
      NODDBL = -1
      NDSIN2 = -1
      NDDBL2 = -1
*
* -------- Determine the precision: the two approaches need to be
*          separated because the caching effect will not be present
*          otherwise.
*
      DO 110 I = 1,40
         RVAL   = 1.0    + 10.0    ** (-I)
         DVAL   = 1.0D00 + 10.0D00 ** (-I)
*
         IF ( RVAL   .EQ. 1.0    .AND. NODSIN .EQ. -1 ) NODSIN = I - 1
         IF ( DVAL   .EQ. 1.0D00 .AND. NODDBL .EQ. -1 ) NODDBL = I - 1
  110 CONTINUE
*
      DO 120 I = 1,40
         RVAL   = 1.0    + 10.0    ** (-I)
         DVAL   = 1.0D00 + 10.0D00 ** (-I)
*
         CALL CHKEQR( RVAL   , 1.0    , EQSNGL ,
     &                DVAL   , 1.0D00 , EQDBLE )
         IF ( EQSNGL .AND. NDSIN2 .EQ. -1 ) NDSIN2 = I - 1
         IF ( EQDBLE .AND. NDDBL2 .EQ. -1 ) NDDBL2 = I - 1
  120 CONTINUE
*
      WRITE( * , * ) 'Significant decimals in real:             ' ,
     &   NDSIN2
      WRITE( * , * ) 'Significant decimals in double precision: ' ,
     &   NDDBL2
      WRITE( * , * )
*
      IF ( NDSIN2 .NE. NODSIN .OR. NDDBL2 .NE. NODDBL .OR.
     &     NODSIN .EQ. NODDBL                              ) THEN
         CALL WRMESG( '@NUMERICAL-STRANGE-PRECISION' )
         WRITE( * , * ) 'Significant decimals in real:             ' ,
     &      NODSIN
         WRITE( * , * ) 'Significant decimals in double precision: ' ,
     &      NODDBL
         WRITE( * , * )
      ENDIF
*
* -------- Test the equality of x*y and y*x for a series of
*          x and y values
*          (The numbers are: e, pi, sqrt(2) and cos(50 degrees))
*
      CALL WRMESG( '@NUMERICAL-COMMUTATIVE' )
      NODIFS = 0
      NODIFD = 0
      DO 210 I = 1,40
         RVAL   = 2.718281828    + 1.1414213562    *  ( I - 20 )
         RVAL2  = 3.141592654    * 0.642787609     ** ( I - 10 )
         DVAL   = 2.718281828D00 + 1.1414213562D00 *  ( I - 20 )
         DVAL2  = 3.141592654D00 * 0.642787609D00  ** ( I - 10 )
*
         SPROD1 = RVAL   * RVAL2
         SPROD2 = RVAL2  * RVAL
         DPROD1 = DVAL   * DVAL2
         DPROD2 = DVAL2  * DVAL
*
         CALL CHKEQR( SPROD1 , SPROD2 , EQSNGL ,
     &                DPROD1 , DPROD2 , EQDBLE )
         IF ( .NOT. EQSNGL ) NODIFS = NODIFS + 1
         IF ( .NOT. EQDBLE ) NODIFD = NODIFD + 1
  210 CONTINUE
*
      IF ( NODIFS .NE. 0 ) THEN
         CALL WRMESG( '@NUMERICAL-UNWANTED-RESULT-SINGLE' )
      ENDIF
      IF ( NODIFD .NE. 0 ) THEN
         CALL WRMESG( '@NUMERICAL-UNWANTED-RESULT-DOUBLE' )
      ENDIF
      IF ( NODIFS .EQ. 0 .AND. NODIFD .EQ. 0 ) THEN
         CALL WRMESG( '@NUMERICAL-RESULT-ALLRIGHT' )
      ENDIF
*
* -------- Test the equality of x*y and (-x)*(-y) for a series of
*          x and y values
*          (The numbers are: e, pi, sqrt(2) and cos(50 degrees))
*
      CALL WRMESG( '@NUMERICAL-NEGATIVE' )
      NODIFS = 0
      NODIFD = 0
      DO 310 I = 1,40
         RVAL   = 2.718281828    + 1.1414213562    *  ( I - 20 )
         RVAL2  = 3.141592654    * 0.642787609     ** ( I - 10 )
         DVAL   = 2.718281828D00 + 1.1414213562D00 *  ( I - 20 )
         DVAL2  = 3.141592654D00 * 0.642787609D00  ** ( I - 10 )
*
         SPROD1 =   RVAL    *    RVAL2
         SPROD2 = (-RVAL  ) * ( -RVAL2  )
         DPROD1 =   DVAL    *    DVAL2
         DPROD2 = (-DVAL  ) * ( -DVAL2  )
*
         CALL CHKEQR( SPROD1 , SPROD2 , EQSNGL ,
     &                DPROD1 , DPROD2 , EQDBLE )
         IF ( .NOT. EQSNGL ) NODIFS = NODIFS + 1
         IF ( .NOT. EQDBLE ) NODIFD = NODIFD + 1
  310 CONTINUE
*
      IF ( NODIFS .NE. 0 ) THEN
         CALL WRMESG( '@NUMERICAL-UNWANTED-RESULT-SINGLE' )
      ENDIF
      IF ( NODIFD .NE. 0 ) THEN
         CALL WRMESG( '@NUMERICAL-UNWANTED-RESULT-DOUBLE' )
      ENDIF
      IF ( NODIFS .EQ. 0 .AND. NODIFD .EQ. 0 ) THEN
         CALL WRMESG( '@NUMERICAL-RESULT-ALLRIGHT' )
      ENDIF
*
* -------- Test the equality of (-x) and abs(x) for a series of
*          x values
*          (The numbers are: e, pi, sqrt(2) and cos(50 degrees))
*
      CALL WRMESG( '@NUMERICAL-ABSOLUTE' )
      NODIFS = 0
      NODIFD = 0
      DO 410 I = 1,40
         RVAL   = -2.718281828    * 3.141592654    ** ( I - 10 )
         DVAL   = -2.718281828D00 * 3.141592654D00 ** ( I - 10 )
*
         SPROD1 =     -RVAL
         SPROD2 = ABS( RVAL  )
         DPROD1 =     -DVAL
         DPROD2 = ABS( DVAL  )
*
         CALL CHKEQR( SPROD1 , SPROD2 , EQSNGL ,
     &                DPROD1 , DPROD2 , EQDBLE )
         IF ( .NOT. EQSNGL ) NODIFS = NODIFS + 1
         IF ( .NOT. EQDBLE ) NODIFD = NODIFD + 1
  410 CONTINUE
*
      IF ( NODIFS .NE. 0 ) THEN
         CALL WRMESG( '@NUMERICAL-UNWANTED-RESULT-SINGLE' )
      ENDIF
      IF ( NODIFD .NE. 0 ) THEN
         CALL WRMESG( '@NUMERICAL-UNWANTED-RESULT-DOUBLE' )
      ENDIF
      IF ( NODIFS .EQ. 0 .AND. NODIFD .EQ. 0 ) THEN
         CALL WRMESG( '@NUMERICAL-RESULT-ALLRIGHT' )
      ENDIF
*
      RETURN
      END

* @@------------------------------------------------------------------
*   Routine:  CHKEQR
*   Author:   Arjen Markus
*   Purpose:  Check if two reals are equal
*   Context:  Used by CHKREA to avoid caching of intermeidate results
*   Summary:
*             If the two reals are equal, return true, otherwise
*             return false. Do the same for double precision
*   Arguments:
*   Name      Type I/O  Descrption
*   RSING1    REAL I    First single precision real
*   RSING2    REAL I    Second single precision real
*   EQSNGL    LOG  O    Whether RSING1 and RSING2 are equal
*   RDBLE1    DBLE I    First double precision real
*   RDBLE2    DBLE I    Second double precision real
*   EQDBLE    LOG  O    Whether RDBLE1 and RDBLE2 are equal
* --------------------------------------------------------------------
*
      SUBROUTINE CHKEQR( RSING1 , RSING2 , EQSNGL ,
     &                   RDBLE1 , RDBLE2 , EQDBLE )
*
      IMPLICIT NONE
*
      REAL             RSING1 , RSING2
      DOUBLE PRECISION RDBLE1 , RDBLE2
      LOGICAL          EQSNGL , EQDBLE
*
      EQSNGL = RSING1 .EQ. RSING2
      EQDBLE = RDBLE1 .EQ. RDBLE2
*
      RETURN
      END

* @@------------------------------------------------------------------
*   Routine:  CHKFIL
*   Author:   Arjen Markus
*   Purpose:  Perform tests on the file behaviour
*   Context:  Used to check the way files work in the run-time environment
*   Summary:
*             The routine performs these tests:
*             - Are the parameters to the OPEN statement checked
*               immediately?
*             - Using unformatted READs on a formatted file
*             - Writing to an unopened file
*             - Reading too many data from a record of an
*               unformatted file
*             - Writing too many data to a record of a direct-access
*               file
*             - Opening the same file twice
*             - Closing an unopened file
*             Each test is accompanied by suitable messages describing
*             the test and its results. Most important: can we catch
*             such errors or will the program be ignorant about them?
* --------------------------------------------------------------------
*
      SUBROUTINE CHKFIL
*
      IMPLICIT NONE
*
      LOGICAL      OPEN10 , OPEN11
      CHARACTER*20 STRING
      CHARACTER*20 FILNAM , FILN2
      INTEGER      IERR   , IDUMMY , K      , I
*
* -------- Initialise local variables
*          (Note: to avoid compiler messages, the same file name is
*          constructed and stored in a second variable)
*
      FILNAM = 'chksys.1'
      FILN2  = 'chksys.2'
      FILN2(8:8) = '1'
      IDUMMY = 0
*
* -------- Test 1:
*          Are the parameters to the OPEN statement checked
*          immediately?
*
      CALL WRMESG( '@FILE-TEST-CHECK-OPEN' )
      CALL GETSTR( STRING )
      OPEN( 10 , STATUS = 'SCRATCH' , IOSTAT = IERR , FORM = STRING )
      IF ( IERR   .EQ. 0 ) THEN
         CALL WRMESG( '@FILE-TEST-CHECK-NO' )
         CLOSE( 10 )
      ELSE
         CALL WRMESG( '@FILE-TEST-CHECK-YES' )
      ENDIF
*
* -------- Test 2:
*          Using unformatted READs on a formatted file
*
      CALL WRMESG( '@FILE-TEST-MIXED-FORMAT' )
      OPEN( 10 , FILE   = FILNAM , STATUS = 'UNKNOWN'   ,
     &           IOSTAT = IERR   , FORM   = 'FORMATTED' )
      IF ( IERR   .NE. 0 ) THEN
         CALL WRMESG( '@FILE-TEST-CREATE-ERROR' )
         GOTO 300
      ENDIF
*
* -------- Write the file and close it. Then open it again - this time
*          unformatted.
*          The error may occur upon opening the file or reading the
*          first time. There may not even an error reported then.
*
      WRITE( 10 , * ) STRING
      CLOSE( 10 )
*
      OPEN( 10 , FILE   = FILNAM , STATUS = 'OLD'         ,
     &           IOSTAT = IERR   , FORM   = 'UNFORMATTED' )
*
      IF ( IERR   .EQ. 0 ) THEN
         READ(  10 , IOSTAT = IERR     ) IDUMMY
         CLOSE( 10 , STATUS = 'DELETE' )
         IF ( IERR   .NE. 0 ) THEN
            CALL WRMESG( '@FILE-TEST-MIXED-ERROR-READ' )
         ELSE
            CALL WRMESG( '@FILE-TEST-MIXED-NO-ERROR-AT-ALL' )
         ENDIF
      ELSE
         CALL WRMESG( '@FILE-TEST-MIXED-ERROR-OPEN' )
      ENDIF
*
* -------- Test 3:
*          Writing to an unopened file
*          Note:
*          The run-time library may ask for a name (as it will do
*          with a MicroSoft FORTRAN compiler). Then, sometimes, though
*          not always, it will use the command-line arguments to
*          fill in the blanks. If it asks the user directly, we have
*          little idea how to detect this is a fool-proof manner.
*
  300 CONTINUE
      CALL WRMESG( '@FILE-TEST-WRITE-UNOPENED' )
*
      WRITE( 11 , * , IOSTAT = IERR ) STRING
*
      IF ( IERR   .NE. 0 ) THEN
         CALL WRMESG( '@FILE-TEST-UNOPENED-ERROR' )
      ELSE
         INQUIRE( 11 , NAME = FILNAM )
         K     = INDEX( FILNAM , '11' )
         IF ( K      .NE. 0 ) THEN
            CALL WRMESG( '@FILE-TEST-UNOPENED-OPEN' )
         ELSE
            K     = INDEX( FILNAM , 'ask' )
            IF ( K      .NE. 0 ) THEN
               CALL WRMESG( '@FILE-TEST-UNOPENED-ASK' )
            ELSE
               CALL WRMESG( '@FILE-TEST-UNOPENED-OPEN' )
            ENDIF
         ENDIF
         WRITE( * , '(1X,A)' )
     &      'The program used INQUIRE( NAME=...) to find out the name'
         WRITE( * , '(1X,2A)' )
     &      'File name (used LU-number 11): ' , FILNAM
         WRITE( * , * )
      ENDIF
      CLOSE( 11 , STATUS = 'DELETE' )
*
* -------- Test 4:
*          Reading too many data from an unformatted file
*
  400 CONTINUE
      CALL WRMESG( '@FILE-TEST-READ-TOO-MANY' )
*
      OPEN( 10 , FILE   = FILNAM , STATUS = 'UNKNOWN'     ,
     &           IOSTAT = IERR   , FORM   = 'UNFORMATTED' )
      IF ( IERR   .NE. 0 ) THEN
         CALL WRMESG( '@FILE-TEST-CREATE-ERROR' )
         GOTO 500
      ENDIF
*
      WRITE(  10 ) IDUMMY
      REWIND( 10 )
      READ(  10  , IOSTAT = IERR ) IDUMMY , IDUMMY
*
      IF ( IERR   .NE. 0 ) THEN
         CALL WRMESG( '@FILE-TEST-TOO-MANY-ERROR' )
      ELSE
         CALL WRMESG( '@FILE-TEST-TOO-MANY-NO-ERROR' )
      ENDIF
*
      CLOSE( 10 , STATUS = 'DELETE' )
*
* -------- Test 5:
*          Writing too many data to a record of a direct-access file
*
  500 CONTINUE
      CALL WRMESG( '@FILE-TEST-WRITE-TOO-MANY' )
      OPEN( 10 , STATUS = 'SCRATCH'     ,
     &           IOSTAT = IERR     , FORM   = 'UNFORMATTED' ,
     &           ACCESS = 'DIRECT' , RECL   = 4             )
      IF ( IERR   .NE. 0 ) THEN
         CALL WRMESG( '@FILE-TEST-CREATE-ERROR' )
         GOTO 600
      ENDIF
*
      WRITE(  10 , REC = 1 , IOSTAT = IERR )
     &   ( IDUMMY , I = 1,10 )
*
      IF ( IERR   .NE. 0 ) THEN
         CALL WRMESG( '@FILE-TEST-TOO-MANY-ERROR' )
      ELSE
         CALL WRMESG( '@FILE-TEST-TOO-MANY-NO-ERROR' )
      ENDIF
*
      CLOSE( 10 )
*
* -------- Test 6:
*          Opening the same file twice
*
  600 CONTINUE
      CALL WRMESG( '@FILE-TEST-OPENING-FILE-TWICE' )
      OPEN( 10 , FILE   = FILNAM , STATUS = 'UNKNOWN' ,
     &           IOSTAT = IERR                        )
      IF ( IERR   .NE. 0 ) THEN
         CALL WRMESG( '@FILE-TEST-CREATE-ERROR' )
         GOTO 700
      ENDIF
*
      OPEN( 11 , FILE   = FILN2  , STATUS = 'UNKNOWN' ,
     &           IOSTAT = IERR                        )
      IF ( IERR   .NE. 0 ) THEN
         CALL WRMESG( '@FILE-TEST-OPEN-TWICE-ERROR' )
      ELSE
         CALL WRMESG( '@FILE-TEST-OPEN-TWICE-NO-ERROR' )
      ENDIF
*
      INQUIRE( 10 , OPENED = OPEN10 )
      INQUIRE( 11 , OPENED = OPEN11 )
      IF ( OPEN10 ) THEN
         IF ( OPEN11 ) THEN
            CALL WRMESG( '@FILE-TEST-OPEN-TWICE-BOTH-OPEN'  )
         ELSE
            CALL WRMESG( '@FILE-TEST-OPEN-TWICE-ONLY-FIRST' )
         ENDIF
      ELSE
         IF ( OPEN11 ) THEN
            CALL WRMESG( '@FILE-TEST-OPEN-TWICE-ONLY-SECOND' )
         ELSE
            CALL WRMESG( '@FILE-TEST-OPEN-TWICE-BOTH-CLOSED' )
         ENDIF
      ENDIF
*
* -------- Make sure the file is deleted
*          Note:
*          If the file was opened to both units, it is necessary
*          to close them both! In an early version of the program
*          the file was opened again and removed via STATUS='DELETE'
*          but this nevertheless resulted in an empty file "chksys.1".
*          With a different compiler, an error occurred trying to close
*          the second file unit.
*
      IF ( OPEN10 ) THEN
         CLOSE( 10 , STATUS = 'DELETE' , IOSTAT = IERR )
         IF ( IERR   .NE. 0 ) THEN
            CALL WRMESG( '@FILE-TEST-OPEN-ERROR-CLOSE' )
         ENDIF
      ENDIF
      IF ( OPEN11 ) THEN
         CLOSE( 11 , STATUS = 'DELETE' , IOSTAT = IERR )
         IF ( IERR   .NE. 0 ) THEN
            CALL WRMESG( '@FILE-TEST-OPEN-TWICE-ERROR-CLOSE' )
         ENDIF
      ENDIF
*
* -------- Test 7:
*          Closing an unopened file
*
  700 CONTINUE
      CALL WRMESG( '@FILE-TEST-CLOSING-FILE' )
      CLOSE( 12 , IOSTAT = IERR   )
      IF ( IERR   .NE. 0 ) THEN
         CALL WRMESG( '@FILE-TEST-CLOSING-FILE-ERROR' )
      ELSE
         CALL WRMESG( '@FILE-TEST-CLOSING-FILE-NO-ERROR' )
      ENDIF
      GOTO 800
*
* -------- Test 8:
*          Opening another file to the same unit
*
  800 CONTINUE
      CALL WRMESG( '@FILE-TEST-OPEN-AGAIN' )
*
      OPEN( 10 , STATUS = 'SCRATCH' , IOSTAT = IERR   )
      IF ( IERR   .EQ. 0 ) THEN
         WRITE( 10 , * ) 'Something or other'
*
         OPEN( 10 , STATUS = 'SCRATCH' , IOSTAT = IERR   )
         IF ( IERR   .EQ. 0 ) THEN
            CALL WRMESG( '@FILE-TEST-OPEN-AGAIN-NO' )
         ELSE
            CALL WRMESG( '@FILE-TEST-OPEN-AGAIN-ERROR' )
         ENDIF
         CLOSE( 10 )
      ENDIF
*
      GOTO 900
*
* -------- Test 9:
*          Treatment of incomplete lines (no end-of-line)
*
  900 CONTINUE
      CALL WRMESG( '@FILE-TEST-INCOMPLETE-LINES' )
      FILNAM = 'chksys.1'
      OPEN( 10 , FILE   = FILNAM   , STATUS = 'UNKNOWN' ,
     &           ACCESS = 'DIRECT' , RECL   = 30        ,
     &           IOSTAT = IERR                          )
      IF ( IERR   .NE. 0 ) THEN
         CALL WRMESG( '@FILE-TEST-CREATE-ERROR' )
         GOTO 1000
      ENDIF
*
      STRING = '1234567890'
      WRITE( 10 , REC = 1 ) STRING
      CLOSE( 10 )
*
      OPEN( 10 , FILE   = FILNAM       , STATUS = 'OLD'  ,
     &           ACCESS = 'SEQUENTIAL' , IOSTAT = IERR   )
      IF ( IERR   .NE. 0 ) THEN
         CALL WRMESG( '@FILE-TEST-INCOMPLETE-OPEN-ERROR' )
         GOTO 1000
      ENDIF
*
      READ( 10 , '(A)' , IOSTAT = IERR   ) STRING
      IF ( IERR   .EQ. 0 ) THEN
         CALL WRMESG( '@FILE-TEST-INCOMPLETE-ACCEPTED' )
      ELSE IF ( IERR   .LT. 0 ) THEN
         CALL WRMESG( '@FILE-TEST-INCOMPLETE-END-OF-FILE' )
      ELSE
         CALL WRMESG( '@FILE-TEST-INCOMPLETE-READ-ERROR' )
      ENDIF
      CLOSE( 10 , STATUS = 'DELETE' )
      GOTO 1000
*
* -------- End of test set
*
 1000 CONTINUE
      RETURN
      END

* @@------------------------------------------------------------------
*   Routine:  CHKNUM
*   Author:   Arjen Markus
*   Purpose:  Set the arguments to specific numerical values
*   Context:  Used to avoid messages from smart compilers
*   Summary:
*             Simply set the arguments
*   Arguments:
*   Name      Type I/O  Descrption
*   XLARGE    REAL O    A very large real value
*   XSMALL    REAL O    A very small real value
*   XZERO     REAL O    Zero
*   XNEG      REAL O    -1.0
* --------------------------------------------------------------------
*
      SUBROUTINE CHKNUM( XLARGE , XSMALL , XZERO  , XNEG   )
*
      IMPLICIT NONE
*
      REAL     XLARGE , XSMALL , XZERO  , XNEG
*
      XLARGE =  1.0E30
      XSMALL =  1.0E-20
      XZERO  =  0.0
      XNEG   = -1.0
*
      RETURN
      END

* @@------------------------------------------------------------------
*   Routine:  CHKARR
*   Author:   Arjen Markus
*   Purpose:  Check if array bounds are checked
*   Context:  Used to check a common programming error
*   Summary:
*             Access an array element outside the (local) array
*   Arguments:
*   Name      Type I/O  Descrption
*   IELEM     INT  I    Index of element to be printed
* --------------------------------------------------------------------
*
      SUBROUTINE CHKARR( IELEM  )
*
      IMPLICIT NONE
*
      INTEGER     IELEM
*
      INTEGER     NOARR
      PARAMETER ( NOARR  = 10 )
      REAL        BVALUE , ARRAY( NOARR  ) , VALUE
      INTEGER     I
*
      BVALUE = 3.1415926
      VALUE  = 2.0
      DO 110 I = 1,NOARR
         ARRAY(I)  = 1.0
  110 CONTINUE
*
      WRITE( * , * ) 'Array-element' , IELEM , '=' , ARRAY(IELEM)
      WRITE( * , * ) ' '
*
      RETURN
      END

* @@------------------------------------------------------------------
*   Routine:  CHKSUB
*   Author:   Arjen Markus
*   Purpose:  Check if substrings are checked
*   Context:  Used to check a common programming error
*   Summary:
*             Access two invalid substrings. The string limits
*             are specified as arguments to avoid errors from
*             smart compilers.
*   Arguments:
*   Name      Type I/O  Descrption
*   IZERO     INT  I    Index 0 (beyond left end)
*   IBEYND    INT  I    Index 11 (beyond right end)
* --------------------------------------------------------------------
*
      SUBROUTINE CHKSUB( IZERO  , IBEYND )
*
      IMPLICIT NONE
*
      INTEGER     IZERO   , IBEYND
*
      INTEGER     NOARR
      PARAMETER ( NOARR  = 10 )
      CHARACTER*10 STRING
*
      STRING = '1234567890'
*
      WRITE( * , * ) 'String (10 chars): >' , STRING            , '<'
      WRITE( * , * ) 'Substring (1:0)  : >' , STRING(1:IZERO)   , '<'
      WRITE( * , * ) 'Substring (10:11): >' , STRING(10:IBEYND) , '<'
*
      RETURN
      END

* @@------------------------------------------------------------------
*   Routine:  CHKSHF
*   Author:   Arjen Markus
*   Purpose:  Check if substrings can be shifted correctly
*   Context:  Used to check an implicit assumption
*   Summary:
*             Set a string to a value. Then set a substring to
*             substring one to the right (creating a left shit).
*             Do the same for a right shift. Inspect the results.
*   Arguments:
*             None
* --------------------------------------------------------------------
*
      SUBROUTINE CHKSHF
*
      IMPLICIT NONE
*
      CHARACTER*10 STROVL
      LOGICAL      INCORR
*
      STROVL  = '1234567890'
      WRITE( * , * ) 'Original string: ' , STROVL
      STROVL(1:9) = STROVL(2:10)
      WRITE( * , * ) 'Left shift:      ' , STROVL
      INCORR  = STROVL .NE. '2345678900'
*
      STROVL  = '1234567890'
      STROVL(2:10) = STROVL(1:9)
      WRITE( * , * ) 'Right shift:     ' , STROVL
      WRITE( * , * ) ' '
      INCORR  = INCORR .OR. STROVL .NE. '1123456789'
*
      IF ( INCORR ) THEN
         CALL WRMESG( '@SHIFTING-INCORRECT' )
      ENDIF
*
      RETURN
      END

* @@------------------------------------------------------------------
*   Routine:  CHKSET
*   Author:   Arjen Markus
*   Purpose:  Check whether a certain type of tests should be done
*   Context:  Used to select test sets
*   Summary:
*             If this is the first call, read the file "chksys.set"
*             In all cases: look for a line starting with the keyword.
*             If it exists, then set the second argument to true.
*   Arguments:
*   Name      Type I/O  Descrption
*   KEYWRD    CHAR I    Keyword to look for
*   CHECK     LOG  O    Whether to perform these tests or not
* --------------------------------------------------------------------
*
      SUBROUTINE CHKSET( KEYWRD , CHECK  )
*
      IMPLICIT NONE
*
      INTEGER       MXKEYW
      PARAMETER (   MXKEYW = 50 )
*
      CHARACTER*(*) KEYWRD
      LOGICAL       CHECK
*
      INTEGER      INIT   , NOKEYW , IERR   , LENGTH , I      , I2
      CHARACTER*25 KEYW(MXKEYW)
*
      SAVE         KEYW
      SAVE         INIT   , NOKEYW
      DATA         INIT   , NOKEYW / 1 , 0 /
*
* -------- At the first call, read the keywords file:
*          A line starting with a # is comment
*
      IF ( INIT   .EQ. 1 ) THEN
         INIT   = 0
         OPEN( 10 , FILE   = 'chksys.set' , STATUS = 'OLD' ,
     &              IOSTAT = IERR                          )
         IF ( IERR   .EQ. 0 ) THEN
            I2     = 1
            DO 110 I = 1,MXKEYW
               READ( 10 , '(A)' , IOSTAT = IERR ) KEYW(I2)
*
               IF ( IERR  .EQ. 0 ) THEN
                  IF ( KEYW(I2)(1:1)   .NE. '#' ) THEN
                     I2     = I2     + 1
                     NOKEYW = NOKEYW + 1
                  ENDIF
               ELSE
                  GOTO 120
               ENDIF
  110       CONTINUE
*
  120       CONTINUE
            CLOSE( 10 )
         ELSE
            WRITE( * , * )
     &        'Could not read keywords file - general test only'
            NOKEYW    = 1
            KEYW(1)   = '@GENERAL'
         ENDIF
      ENDIF
*
* -------- Find the keyword, print the message
*
      CHECK = .FALSE.
*
      DO 210 I = 1,NOKEYW
         LENGTH = LEN( KEYWRD )
         IF ( KEYW(I)(1:LENGTH) .EQ. KEYWRD ) THEN
            CHECK  = .TRUE.
            GOTO 220
         ENDIF
  210 CONTINUE
*
  220 CONTINUE
      IF ( .NOT. CHECK ) WRITE( * , * ) '(Skipping: ' , KEYWRD , ')'
*
      RETURN
      END

* @@------------------------------------------------------------------
*   Routine:  CHKSTD
*   Author:   Arjen Markus
*   Purpose:  Check if any standard LU-numbers are opened
*   Context:  Used to check a once common feature
*   Summary:
*             Inquire if the unit numbers 0 to 10 are connected to
*             a "file" (5 and 6 used to be standard input and output).
*             Report the findings
*   Arguments:
*             None
* --------------------------------------------------------------------
*
      SUBROUTINE CHKSTD
*
      IMPLICIT NONE
*
      INTEGER      LU     , IERR
      LOGICAL      OPEND
      CHARACTER*40 FILNAM
*
      DO 110 LU = 0,10
         INQUIRE( LU     , OPENED = OPEND  , NAME = FILNAM ,
     &                     IOSTAT = IERR                   )
         IF ( IERR   .EQ. 0 ) THEN
            IF ( OPEND  ) THEN
               WRITE( * , * )
     &            'LU-number' , LU     , ' opened to file ' , FILNAM
            ENDIF
         ELSE
               WRITE( * , * )
     &            'LU-number' , LU     , ' caused an error in INQUIRE'
         ENDIF
 110  CONTINUE
*
      WRITE( * , * ) ' '
*
      RETURN
      END
