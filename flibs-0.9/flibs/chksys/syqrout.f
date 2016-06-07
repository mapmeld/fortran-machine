* @begin@ */
*
*  syqrout.f(or) - routines to determine some properties of the
*                  run-time environment for FORTRAN programs
*
*  Copyright (C) 1998 Arjen Markus
*
*  Arjen Markus
*
*
*  General information:
*  This file contains the following routines:
*  - SYQOS:           Determine some capabilities of the operating
*                     system
*  - SYQBIN:          Determine how to open BINARY files (if possible)
*  - SYQREC:          Determine the length unit for direct access files
*
* @end@
*
*  $Author$
*  $Date$
*  $Source$
*  $Log$
*

* @@------------------------------------------------------------------
*   Routine:  SYQOS
*   Author:   Arjen Markus
*   Purpose:  Determine the type of the operating system
*   Context:  Used by applications
*   Summary:
*             Open specific files that will present if the OS is
*             of some type. The presence of the files indicates the OS.
*             Create a few files to check the conventions for file
*             names. This further narrows the possibilities.
*
*   Arguments:
*   Name     Type    I/O  Description
*   ITYPE    INT      O   Type of OS:
*                         -1   - Could not be determined
*                          1   - UNIX
*                          2   - DOS/Windows 3.x
*                          3   - Windows 95/NT
*   LNGNAM   LOG      O   Are long file names allowed (> 14 characters)?
*   IGNCAS   LOG      O   Are file names case-sensitive?
*   DIRSEP   CHAR*1   O   Character for separating directories
* --------------------------------------------------------------------
*
      SUBROUTINE SYQOS(  ITYPE  , LNGNAM , IGNCAS , DIRSEP )
*
      IMPLICIT NONE
*
      INTEGER       ITYPE
      LOGICAL       LNGNAM , IGNCAS
      CHARACTER*(*) DIRSEP
*
      LOGICAL       OPEND  , EXISTS
      INTEGER       I      , LUN    , IERR
      CHARACTER*40  FILNAM
      CHARACTER*20  FILUX
*
      DATA FILUX  / '/bin/sh' /
*
* -------- Find a free logical unit number first
*
      ITYPE  = -2
      LNGNAM = .FALSE.
      IGNCAS = .FALSE.
*
      DO 110 I = 10,99
         INQUIRE( I , OPENED = OPEND  )
         IF ( .NOT. OPEND ) THEN
            LUN    = I
            GOTO 200
         ENDIF
  110 CONTINUE
*
* -------- If we get to this point, we could not open a file
*
      RETURN
*
* -------- If the platform is UNIX, then the file /bin/sh will
*          exist and be readable
*
  200 CONTINUE
      INQUIRE( FILE = FILUX  , EXIST = EXISTS )
      IF ( EXISTS ) THEN
         ITYPE  = 1
         DIRSEP = '/'
      ENDIF
*
* -------- For all other platforms, there is no fixed file to be
*          looked for, except perhaps "autoexec.bat" but even then
*          the procedure is complicated. So rely on the characteristics
*          of the file names instead:
*
*          Type       long names       ignore case in names
*          UNIX         yes/no              no!
*          DOS/WIN3.x   no                  yes
*          WIN95/NT     yes                 yes (?)
*
*          Long file names:
*          - if not supported, the file name will be truncated or
*            an error occurs
*          Ignore case in file names:
*          - the file can be opened as SYQOS.TST and syqos.tst
*
*          Note:
*          Some old versions of UNIX do not support long file names
*          (they are truncated to 14 characters)
*
      FILNAM = 'syqos_long_file_name'
      OPEN(  LUN    , FILE   = FILNAM , STATUS = 'UNKNOWN' ,
     &                IOSTAT = IERR                        )
      IF ( IERR    .EQ. 0 ) THEN
         WRITE( LUN    , * ) 'Test'
         CLOSE( LUN    )
*
         IF ( ITYPE  .EQ. 1 ) THEN
            OPEN(  LUN    , FILE   = FILNAM(1:14) , STATUS = 'OLD' ,
     &                      IOSTAT = IERR                          )
         ELSE
            OPEN(  LUN    , FILE   = FILNAM(1:8)  , STATUS = 'OLD' ,
     &                      IOSTAT = IERR                          )
         ENDIF
*
* -------- Are long names supported?
*
         IF ( IERR   .EQ. 0 ) THEN
            LNGNAM = .FALSE.
            CLOSE( LUN    , STATUS = 'DELETE' )
         ELSE
            LNGNAM = .TRUE.
         ENDIF
*
* -------- Get rid of the file with the long name as well
*
         OPEN(  LUN    , FILE   = FILNAM , STATUS = 'OLD' ,
     &                   IOSTAT = IERR                    )
         IF ( IERR   .EQ. 0 ) THEN
            CLOSE( LUN    , STATUS = 'DELETE' )
         ENDIF
*
* -------- An error occurred in the first place
*
      ELSE
         LNGNAM = .FALSE.
      ENDIF
*
* -------- Now test the distinction between uppercase and lowercase
*
      FILNAM = 'syqos_xx'
      OPEN(  LUN    , FILE   = FILNAM , STATUS = 'UNKNOWN' ,
     &                IOSTAT = IERR                        )
      IF ( IERR    .EQ. 0 ) THEN
         WRITE( LUN    , * ) 'Test'
         CLOSE( LUN    )
*
         FIlNAM = 'SYQOS_XX'
         OPEN(  LUN    , FILE   = FILNAM , STATUS = 'OLD' ,
     &                   IOSTAT = IERR                    )
*
* -------- Are uppercase and lowercase distinct?
*
         IF ( IERR   .EQ. 0 ) THEN
            IGNCAS = .TRUE.
            CLOSE( LUN    , STATUS = 'DELETE' )
         ELSE
            LNGNAM = .TRUE.
         ENDIF
*
* -------- Get rid of the file with the lowercase name as well
*
         FILNAM = 'syqos_xx'
         OPEN(  LUN    , FILE   = FILNAM , STATUS = 'OLD' ,
     &                   IOSTAT = IERR                    )
         IF ( IERR   .EQ. 0 ) THEN
            CLOSE( LUN    , STATUS = 'DELETE' )
         ENDIF
*
* -------- An error occurred in the first place:
*          this represents an unsupported situation!
*
      ELSE
         ITYPE  = -1
      ENDIF
*
* -------- Finally we can make a good guess about the operating system
*
      IF ( ITYPE  .NE. 1 .AND. ITYPE  .NE. -1 ) THEN
         DIRSEP = '\\'
         IF ( .NOT. LNGNAM .AND. IGNCAS ) ITYPE  = 2
         IF ( LNGNAM                    ) ITYPE  = 3
      ENDIF
*
      RETURN
      END

* @@------------------------------------------------------------------
*   Routine:  SYQBIN
*   Author:   Arjen Markus
*   Purpose:  Determine if and how a BINARY file can be opened
*   Context:  Used by applications
*   Summary:
*             Open a scratch-file, with various possible keywords
*             Write a single number to it to check if that works.
*             If so, binary files can be opened that way.
*
*   Arguments:
*   Name     Type    I/O  Description
*   ALLOWD   LOG      O   If true, binary files are allowed. If false,
*                         you need to use unformatted sequential files.
*   FORM     CHAR*20  O   Value for keyword FORM=
*   ACCESS   CHAR*20  O   Value for keyword ACCESS=
* --------------------------------------------------------------------
*
      SUBROUTINE SYQBIN( ALLOWD , FORM   , ACCESS )
*
* -------- One common extension to the standard
*
      IMPLICIT NONE
*
      LOGICAL        ALLOWD
      CHARACTER*(*)  FORM   , ACCESS
*
      INTEGER        NOTYPE
      PARAMETER (    NOTYPE = 2 )
*
      INTEGER      IERR   , LUN    , I      , ITYPE  , IDUMMY
      CHARACTER*20 FRMTYP(NOTYPE)  , ACCTYP(NOTYPE)
      LOGICAL      OPEND
*
      SAVE         FRMTYP , ACCTYP , ITYPE
      DATA         ITYPE  / -1 /
      DATA ( FRMTYP(I)      , ACCTYP(I)     , I = 1,NOTYPE ) /
     &       'BINARY'       , 'SEQUENTIAL'  ,
     &       'UNFORMATTED'  , 'TRANSPARENT' /
*
* -------- Open the scratch file
*
      DO 110 I = 10,99
         INQUIRE( I , OPENED = OPEND  )
         IF ( .NOT. OPEND ) THEN
            LUN    = I
            GOTO 200
         ENDIF
  110 CONTINUE
*
* -------- If we end up here, then we did not find a free unit
*
      ALLOWD = .FALSE.
      FORM   = 'UNFORMATTED'
      ACCESS = 'SEQUENTIAL'
      RETURN
*
* -------- Try to open a scratch-file and write to it
*
  200 CONTINUE
      IDUMMY  =  1
      ITYPE   = -1
*
      DO 210 I = 1,NOTYPE
         OPEN( LUN    , IOSTAT = IERR   , FORM   = FRMTYP(I) ,
     &                                    ACCESS = ACCTYP(I) )
         IF ( IERR   .EQ. 0 ) THEN
            WRITE( LUN    , IOSTAT = IERR   ) IDUMMY
            CLOSE( LUN    )
            IF ( IERR   .EQ. 0 ) THEN
               ITYPE  = I
               GOTO 300
            ENDIF
         ENDIF
  210 CONTINUE
*
* -------- Conclusion: set the parameters
*
  300 CONTINUE
      IF ( ITYPE  .EQ. -1 ) THEN
         ALLOWD = .FALSE.
         FORM   = 'UNFORMATTED'
         ACCESS = 'SEQUENTIAL'
      ELSE
         ALLOWD = .TRUE.
         FORM   = FRMTYP(ITYPE)
         ACCESS = ACCTYP(I)
      ENDIF
*
      RETURN
      END

* @@------------------------------------------------------------------
*   Routine:  SYQREC
*   Author:   Arjen Markus
*   Purpose:  Determine the length unit for direct access files
*   Context:  Used by applications
*   Summary:
*             Open a scratch-file, with record length 1.
*             Write a CHARACTER*1 string to eight sequential records.
*             Set a CHARACTER*8 string to an empty string.
*             Read eight characters from the first record into the
*             string.
*             Find out how many characters were filled.
*
*   Arguments:
*   Name     Type    I/O  Description
*   NRCBYT   INT      O   Number of characters in a record
*                         of 1 unit (-1 if the file could not
*                         be opened)
* --------------------------------------------------------------------
*
      SUBROUTINE SYQREC( NRCBYT )
*
* -------- One common extension to the standard
*
      IMPLICIT NONE
*
      INTEGER     NRCBYT
*
      INTEGER     IERR   , LUN    , I      , J
      CHARACTER*1 STROUT
      CHARACTER*8 STRIN  , STRDEF
      LOGICAL     OPEND
*
* -------- Open the scratch file
*
      DO 110 I = 10,99
         INQUIRE( I , OPENED = OPEND  )
         IF ( .NOT. OPEND ) THEN
            LUN    = I
            OPEN ( LUN    , STATUS = 'SCRATCH' , IOSTAT = IERR   ,
     &                      FORM   = 'UNFORMATTED'               ,
     &                      ACCESS = 'DIRECT'  , RECL   = 1      )
            IF ( IERR   .NE. 0 ) THEN
               GOTO 900
            ELSE
               GOTO 200
            ENDIF
         ENDIF
  110 CONTINUE
*
* -------- If we end up here, then we did not find a free unit
*
      GOTO 900
*
* -------- Write the eight records
*
  200 CONTINUE
      STROUT = 'A'
      DO 210 I = 1,8
         WRITE( LUN    , IOSTAT = IERR   , REC = I ) STROUT
         IF ( IERR   .NE. 0 ) THEN
            CLOSE( LUN    )
            GOTO 900
         ENDIF
  210 CONTINUE
*
* -------- Try reading the eight characters
*
      NRCBYT = 8
      STRDEF = '12345678'
      DO 290 I = 1,8
         STRIN  = STRDEF
         READ( LUN    , IOSTAT = IERR   , REC = I )
     &      ( STRIN(J:J)  , J = 1,I )
*
* -------- What could we read?
*          Assumption: if an error occurs, the rest of the input is
*                      not changed!
*
         IF ( IERR   .NE. 0 ) THEN
            DO 220 J = 1,8
               IF ( STRIN(J:J)  .EQ. STRDEF(J:J) ) THEN
                  NRCBYT = J      - 1
                  GOTO 300
               ENDIF
  220       CONTINUE
         ENDIF
  290 CONTINUE
*
* -------- We have found something
*
  300 CONTINUE
      CLOSE( LUN    )
      RETURN
*
* -------- Some type of error
*
  900 CONTINUE
      NRCBYT = -1
      RETURN
*
      END
