@INTRODUCTION
CHKSYS: A program to determine the properties of the run-time
        environment for FORTRAN programs

The program presents information about:
- What type of system are you running on (e.g. UNIX or MS Windows)?
- What restrictions for file names?
- Are local variables in subroutines SAVEd or not?
- Other matters concerning portability and the detection of
  run-time errors.

Note:
Some systems will ask for a file name when a READ/WRITE occurs to an
openend file (unit number) as occurs in the third file-handling test.
The program can not prevent the question but it tries to detect the
result. So:
First run the program without redirection. Then you know what happens.
The recommended file name is "askfile".

Note:
Some tests may cause the program to crash or, in the worst to let
the whole system hang (on PC). So the file "chksys.set" can be used
to control whether such tests are performed.

@FILE-SYSTEM
File system
-----------
The following information about the operating system and the file
system is obtained:

@PROGRAM-ERROR
*** Error:
*** The program has come up with an unknown condition.
*** Please check this!

@OS-UNKNOWN
Operating system: Unknown. The following information might be misleading

@OS-UNIX
Operating system: The program is running under UNIX

@OS-DOS-WINDOWS
Operating system: The program is running under MS DOS or MS Windows 3.x

@OS-WINDOWS-95-NT
Operating system: The program is running under MS Windows 95 or NT
(No further distinction can be made)

@FILE-SYSTEM-SHORT-NAMES
The file system supports short file names only (either the DOS convention
or an old UNIX System V convention)

@FILE-SYSTEM-LONG-NAMES
The file system supports long file names (more than 14 characters)

@FILE-SYSTEM-IGNORE-CASE
File names are insensitive to uppercase and lowercase (e.g. CHKSYS.MSG
and chksys.msg are equivalent)

@FILE-SYSTEM-RESPECT-CASE
The file system distinguishes between uppercase and lowercase in file
names (e.g. CHKSYS.MSG and chksys.msg are not the same files!)

@FILE-DIRECT-ACCESS
Record length for direct access files
-------------------------------------
A widespread misunderstanding of direct-access files is, that the
record length is expressed in bytes (or perhaps characters). Thus a
record that should contain 10 ordinary integers should have at least
length 40:

    OPEN(  10 , FILE = 'direct.dat' , RECL = 40 )
    WRITE( 10 , REC = 1 ) ( IARRAY(I) , I = 1,10 )

In fact this is compiler-dependent! Some compilers use a "word"
(commonly 32 bits or 4 bytes) as the unit of length. The OPEN
statement would then need to be:

    OPEN( 10 , FILE = 'direct.dat' , RECL = 10 )

In this case:

@FILE-BINARY-FILES
Use of binary files
-------------------
Some FORTRAN compilers allow BINARY files as an extension to
the standard. Such files have the advantage of being independent
of the FORTRAN compiler, in contrast to the ordinary UNFORMATTED
files. Binary files can easily be used in a C program for instance.

Unfortunately, each compiler has its own way of specifying such files.

@FILE-BINARY-FILES-ALLOWED
The current compiler supports binary files. Use the following keywords
in the OPEN statement:

@FILE-BINARY-FILES-NOT-ALLOWED
The current compiler does NOT support binary files.

@NUMBER-OPEN-FILES
Maximum number of open files
----------------------------
All systems have a limit to the number of files that can opened at the
same time.

@NUMBER-OPEN-FILES-LIMITED
The current system allows a limited number of open files:

@NUMBER-OPEN-FILES-UNLIMITED
The current system allows at least 90 open files, which for almost all
practical purposes is quite enough.

@FILE-STANDARD
Standard LU-numbers
-------------------
FORTRAN 77 uses several standard LU numbers, such as 5 for reading
input from the keyboard. The list below shows which standard LU numbers
are used an to what files they are connected:

@LENGTH-INTEGER
Number of bytes for integers
----------------------------
Many compilers offer an option to set the default length for integer
variables. Thus the range for variables declared as INTEGER is
sometimes from -32767 to 32767 (the length of an integer is 2 bytes)
and sometimes from -2147483647 to 2147483647 (the length of an
integer is 4 bytes).

In contrast to FORTRAN 90, FORTRAN 77 does not allow these limits to
be easily detected.

According to the test however:
@NUMERICAL-PRECISION
Numerical precision
-------------------
The program tries to obtain some information about the numerical aspects
of the compiler and run-time environment:
- The number of significant decimals in single and double precision reals
- The precision of certain operations: is x*y equal to y*x,
  is x*y equal to(-x)*(-y), is -x equal to abs(x) if x < 0
  (Reports from several sources show that this need not be the case!)

Note:
A program like "paranoia" is capable of doing a thorough test on
numerical operations. The tests here are quick and dirty, to get
some insight quickly.

Test 1: Number of significant decimals in single and double precision reals
-------
In most cases the number of significant decimals is 7 for single precision
and 15 for double precision. The numbers are established in two ways:
- A simple approach
- A roundabout approach
The latter is necessary because some compilers seem to use caching
techniques, which gives rise to strange results.

@NUMERICAL-COMMUTATIVE
Test 2: Is multiplication commutative (i.e. x*y equal y*x)?
-------
The test is not exhaustive, but if the test shows that multiplication
is NOT commutative, then you are in trouble.

@NUMERICAL-NEGATIVE
Test 3: Is the sign of influence on multiplication?
-------
The test looks for differences in: x*y and (-x)*(-y). The test is NOT
exhaustive, but merely indicates a potential for trouble.

@NUMERICAL-ABSOLUTE
Test 4: Is (-x) equal to abs(x)?
-------
The test looks for differences in: -x and abs(x), with x negative.
The test is NOT exhaustive, but merely indicates a potential
for trouble.

@NUMERICAL-UNWANTED-RESULT-SINGLE
The test showed differences between the two calculation results
for SINGLE precision. This may give trouble in numerical calculations.

@NUMERICAL-UNWANTED-RESULT-DOUBLE
The test showed differences between the two calculation results
for DOUBLE precision. This may give trouble in numerical calculations.

@NUMERICAL-RESULT-ALLRIGHT
The test showed NO differences between the two calculation results.
This is no guarantee, but indicates that there are no obvious problems.

@NUMERICAL-STRANGE-PRECISION
Warning:
The simple approach showed that single and double precision have
a very large, equal, precision. This is probably due to caching of
intermediate results of calculations. Try again without any optimisation.

The simple approach gives the following results:

@FILE-HANDLING
File handling
-------------
A number of tests follow designed to check the behaviour of the
file I/O system.

@FILE-TEST-CHECK-OPEN
Test 1: Are the parameters to the OPEN statement checked immediately?
-------
A scratch file is opened with the statement:
      OPEN( 10 , STATUS = 'SCRATCH'  , IOSTAT = IERR ,
                 FORM   = 'INVALID!' )

@FILE-TEST-CHECK-NO
Warning:
The parameters in the OPEN statement are NOT checked immediately. This
will probably be delayed until the first READ or WRITE.
@FILE-TEST-CHECK-YES
Note:
The parameters in the OPEN statement are checked immediately. You can
not open the file with invalid parameters.

@FILE-TEST-MIXED-FORMAT
Test 2: Using unformatted READs on a formatted file
-------
Create a formatted file and read it as if it were unformatted.
(The program tries to read the first record only)
Three situations may occur:
- Reopening the file as unformatted causes an error
- Reading data from the file causes an error
- The program reports no error at all

@FILE-TEST-CREATE-ERROR
Warning:
The file that is used in this test (called "chksys.1") could not
be opened. This means the test has been skipped.

Possible causes:
- The file name does not conform to the conventions of this platform
- You have no permission to create files in this directory

@FILE-TEST-MIXED-ERROR-OPEN
In this case: An error occurs when trying to open the file.

@FILE-TEST-MIXED-ERROR-READ
In this case: An error occurs when trying to read data.

@FILE-TEST-MIXED-NO-ERROR-AT-ALL
Warning:
No error occurred AT ALL! This is a very awkward situation.
It may be that the string that was written ("INVALID!")
starts exactly as an unformatted file would. Most likely,
the compiler does not flag any error. That would be serious
indeed!

@FILE-TEST-WRITE-UNOPENED
Test 3: Writing to an unopened file
-------
What happens if you inadvertently write to an unopened file (that
is a LU-number that has not been connected to a file)?

From past experience:
- The program may ask for a file name
- The program opens a file like "fort.10"
- The program produces a run-time error

Note: if the program asks for a file name, type "askfile" (in lowercase
      letters). This way the program may be able to detect the fact that
      at some low level a question was put on screen.

Note: text appearing after these line that does not start "In this case"
      was written by the run-time library, not by this program itself.

@FILE-TEST-UNOPENED-ERROR
In this case: the program will produce a run-time error

@FILE-TEST-UNOPENED-OPEN
In this case: the program seems to use a default file name

@FILE-TEST-UNOPENED-ASK
In this case: the program (probably) asks you for a name

@FILE-TEST-READ-TOO-MANY
Test 4: Reading too many data from a record of an unformatted file
-------
The test is simple: does the program identify a problem reading too
many data from an unformatted record? If not, you have a very faulty
compiler, or you are using the wrong options.

@FILE-TEST-TOO-MANY-ERROR
In this case: The program correctly detects the error.

@FILE-TEST-TOO-MANY-NO-ERROR
Warning:
The program seems to think everything went well! You should not
use this compiler or at least check the set of compiler options.

@FILE-TEST-WRITE-TOO-MANY
Test 5: Writing too many data to a record of a direct-access file
-------
The test consists of opening a scratch file with record length 4 and
writing ten integers to the first record. This should result in a
run-time error.

@FILE-TEST-OPENING-FILE-TWICE
Test 6: Opening the same file twice
-------
The test consists of opening the same file ("chksys.1") twice,
to units 10 and 11. There are four possibilities:
- All attempts fail (the file is not opened at all after the two OPEN
  statements)
- The second attempt fails
- The first unit is implicitly closed
- The file remains open for both units

@FILE-TEST-OPEN-TWICE-ERROR
Note:
The second OPEN statement caused a run-time error

@FILE-TEST-OPEN-TWICE-NO-ERROR
Warning:
The second OPEN statement caused NO run-time error at all

@FILE-TEST-OPEN-TWICE-BOTH-OPEN
Warning:
The units are successfully opened to the same file. This may cause
all kinds of trouble (interaction between READs and WRITEs to the
two units).

@FILE-TEST-OPEN-TWICE-ONLY-FIRST
Note:
Only the first OPEN statement is succssful. The file remains opened
to the first unit.

@FILE-TEST-OPEN-TWICE-ONLY-SECOND
Note:
The second OPEN statement caused the first unit to be closed.
The file is opened to the second unit.

@FILE-TEST-OPEN-TWICE-BOTH-CLOSED
Warning:
The second OPEN statement caused the first unit to be closed and it
failed to open the file. So neither unit is opened!

@FILE-TEST-OPEN-TWICE-ERROR-CLOSE
Warning:
An error occurred while closing the first or second unit. So, apparently
the file was not opened to two units independently.

@FILE-TEST-CLOSING-FILE
Test 7: Closing an unopened file
-------
The test consists of closing a unit number that was never connected
to a file. If this produces a run-time error, closing such a
unit number will abort your program.

Special note:
Some systems produce a "Too many open files" error if you attempt to
close all unit numbers without regard to whether they were opened in
the first place, like:

*
* Close all possible files, just for safety
*
      DO 110 LUN = 10,99
         CLOSE( LUN    )
  110 CONTINUE

@FILE-TEST-CLOSING-FILE-ERROR
Warning:
A run-time error occurs if you try to close an unopened file.

@FILE-TEST-CLOSING-FILE-NO-ERROR
Note:
No run-time error occurred, though this may not be the case on other
platforms or with a different compiler (or even different options).

@FILE-TEST-OPEN-AGAIN
Test 8: Opening another file to the same LU-number
-------
The test consists of opening a file to unit 10 and then opening yet
another file to the same LU-number. The result might be that the first
file is closed.

@FILE-TEST-OPEN-AGAIN-NO
Warning:
No run-time error occurred. This means that you could inadvertently
loose the connection to an open file.

@FILE-TEST-OPEN-AGAIN-ERROR
Note:
This causes a run-time error, as it probably should.

@FILE-TEST-INCOMPLETE-LINES
Test 9: Are lines without an end-of-line character accepted?
-------
The test consists of creating a simple text file with one record which
does not have a proper end-of-line. Opening the file again and trying
to read that one line might give a problem.

@FILE-TEST-INCOMPLETE-ACCEPTED
Note:
The program accepts the line without a problem. (With some compilers
this would cause an end-of-file or a read error!)

@FILE-TEST-INCOMPLETE-OPEN-ERROR
Warning:
An OPEN error occurred. Can not complete the test.

@FILE-TEST-INCOMPLETE-READ-ERROR
Warning:
This causes a run-time error, not an end-of-file condition.

@FILE-TEST-INCOMPLETE-END-OF-FILE
Warning:
This causes an end-of-file condition, the last line can not be read.

@SHIFTING-STRINGS
Shifting strings
----------------
In this test substrings are shifted one character to the left and one
to the right. As the operations involve overlapping strings, it is
not obvious that the program will handle this correctly.

@SHIFTING-INCORRECT
Warning:
The results of the shifts in at least one direction are incorrect.
Be careful with such shifts!

@MISCELLANEOUS1
Miscellaneous issues
--------------------
Treatment of backslash characters (\):
Some compilers treat backslashes as C-like escape sequences, such
that '\t' becomes a TAB character

@BACKSLASH-ESCAPE
Warning:
This is the case with the current compiler (and the options used)!

@BACKSLASH-ASIS
This is NOT the case with the current compiler (and the options used).
So backslashes are safe to use. Be aware of this behaviour though.

@MISCELLANEOUS2
The next two lines should appear without an empty line. The lines
after that show the treatment of numbers (right-justified or
left-justified):

@TREATMENT-ASTERISK

If there is a line in between, then note that code like:
     CHARACTER*75 STR1 , STR2
     WRITE(*,*) STR1
     WRITE(*,*) STR2

may give different results than you expect! It means that the
output is less than 75 columns wide, hence wrapping occurs.

@MISCELLANEOUS3
Characters in the first column that are written to the screen may be
eliminated or regarded as carriage control. The string '%1234567890'
was written with the statement:
     WRITE( * , '(A)' ) '%1234567890'

@MEMORY
Memory model
------------
Contrary to what many programmers believe, variables in subroutines are
not automatically saved. In fact the SAVE statement should be used
to ensure this.

@MEMORY-STATIC
Static memory:
From the test it appears that local variables retain their
values between calls. Thus most likely, local variables and
arrays are stored in static memory.
Consequences:
- The SAVE statement has no other effect than documentation
- Large arrays do not cause a stack overflow
Warnings:
- Use the SAVE statement to document persistent variables
- Use SAVE for any large arrays, as a different environment
  might pose stack problems!

@MEMORY-DYNAMIC
Dynamic memory:
From the test it appears that local variables DO NOT retain
their values between calls. Thus most likely, local variables
and arrays are stored in the stack.
Consequences:
- The SAVE statement should be used to preserve the values
- Large arrays may cause stack overflow in the middle of the
  program run
Solutions:
- Use the SAVE statement to preserve the values
  (This provides documentation as well)
- Use SAVE for any large arrays to force them in static memory

@DO-LOOPS
Test the behaviour of DO-loops
------------------------------
Test 1: Is the number of iterations in a DO-loop protected?
-------
If the variable that controls the DO-loop is changed by mistake,
then various things can happen. It is even possible that the DO-loop
will never end!

In this test, the number of steps should be 10, and the DO-variable
should (probably) become 26, because it is changed to 20 at step 5.

@DO-LOOP-ALTERABLE
Warning:
The number of iterations can be changed! If the variable is changed,
then the DO-variable jumps from 4 to 20. If the variable is 21 at the
end of the loop, then the value is saved.

@DO-LOOP-PROTECTED
The number of iterations can not be changed, though the iteration
variable may be changed. If the variable is changed, then the step
number jumps from 4 to 20. If the variable is 26 at the end of the
loop, then the value is saved.

@DO-LOOP-INFINITE
Warning:
The implementation of DO-loops is such that a change to the DO-variable
leads to INFINITE loops! In our test, the DO-loop is terminated after
20 steps, indicating this somewhat bizarre behaviour.

@DO-LOOP-CHANGE-UPPER
Test 2: What happens if the upper limit in a DO_loop is changed?
-------
The DO-loop looks like this:
      NOSTPM = 10
      DO 110 I = 1,NOSTPM
         IF ( I      .EQ. 4 ) NOSTPM = 6
         ...
  110 CONTINUE

Is it still run 10 times or not?

@DO-LOOP-LIMIT-ALTERABLE
Warning:
The number of iterations can be changed! If the upper limit is changed,
then the number of iterations also changes.

@DO-LOOP-LIMIT-PROTECTED
The number of iterations can not be changed.

@DO-LOOP-LIMIT-INFINITE
Warning:
The implementation of DO-loops is such that a change to the upper limit
may lead to INFINITE loops! In our test, the DO-loop is terminated after
20 steps, indicating this somewhat bizarre behaviour.

@DO-LOOP-PASS
Test 3: DO-loops conform to standard?
-------
In older versions of FORTRAN (FORTRAN 66 for instance), a DO-loop was
always executed at least once. This might make quite a difference.

@DO-LOOP-PASS-ONCE
Warning:
DO-loops are always executed at least once!

@DO-LOOP-PASS-ZERO
DO-loops seem to behave correctly.

@OVERFLOW
Test: What happens if an overflow is generated?
-----
The program calculates 1.0E30 / 1.0E-20, which can not be represented
in single precision (at least if it is a REAL*4 number, according to
IEEE guidelines). An overflow should occur.

@UNDERFLOW
Test: What happens if an underflow is generated?
-----
The program calculates 1.0E-20 / 1.0E30, which can not be represented
in single precision (at least if it is a REAL*4 number, according to
IEEE guidelines). It is either truncated to zero, or an underflow occurs.

@DIVISION
Test: What happens if the program divides by zero?
-----
The program calculates 1.0 / 0.0, which is clearly incorrect. It should
result in a "division by zero" error.

@DOMAIN
Test: The program tries to calculate SQRT( -1.0 )
-----
The program calculates SQRT( -1.0 ), which is clearly incorrect.
It should result in a "domain" error.

@ARRAYBOUND
Test: The program tries to access array elements outside the array
-----
The program uses an array of length 10 and accesses element 11.
As a bonus:
If the reported value for this element is 2.0, the array element is
actually a variable defined after the array. Similarly a value
of 3.1415926 indicates that the program used a variable prior to the
array. If neither value is reported, the memory layout is not obvious.

@ARRAY-NO-ERROR
Warning:
No error was generated. This means that we can access elements
outside the actual array without a run-time error. As this can cause
all kinds of nasty errors, which are likely to become apparent in
a totally different place, there are serious risks involved here.

Check if the compiler has an array bound check option and use this
at least during testing!

@NUMERIC-GENERATE
Warning:
The invalid calculation was done without a run-time error.
Now we will multiply the result by 2.0.

@NUMERIC-NO-ERROR
Warning:
No error was generated. This means that we can manipulate the
result of an invalid calculation. The severity of this behaviour
depends of course on the type of calculation.
(Use a program like "paranoia" to find out more about the numerical
properties of your compiler/computer.)

@SUBSTRING
Test: How does the program react to taking illegal substrings?
-----
This test consists of taking a zero-length substring and a substring
beyond the end of the string. It seems unclear what should happen in
these cases, but if range-checking is enabled, the program will abort.

@SUBSTRING-NO-ERROR
Note:
No error occurred, the program can continue. This means such errors may
remain in the program for a long time.

@end-of-file
