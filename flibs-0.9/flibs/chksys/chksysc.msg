@INTRODUCTION
CHKSYSC: A program to determine the properties of the run-time
         environment for C programs

The program presents information about:
- What type of system are you running on (e.g. UNIX or MS Windows)?
- What restrictions for file names?
- Sizes and ranges for common data types
- Possible alignment issues
- Other matters concerning portability and the detection of
  run-time errors.

Note:
Some tests may cause the program to crash or, in the worst to let
the whole system hang (on PC). So the file "chksysc.set" can be used
to control whether such tests are performed.

@ENVIRONMENT
Environment variables
---------------------
Most systems use environment variables to transfer information between
programs. Two such variables that may influence the behaviour of a
program are: PATH (searching executable programs) and LANG (specifying
the locale, which controls such things as a comma instead of a period
as decimal separator).

Furthermore, most systems supply the name of the program as the first
command-line argument, argv[0]. Many systems include the full path
in this variable, some, notably the Korn shell under UNIX, do not.

In this case:
@ENVIRONMENT-NO-COMMAND-ARGS
Note:
The run-time environment provides no arguments!

@ENVIRONMENT-NO-PATH
Note:
The environment variable PATH has not been defined

@ENVIRONMENT-NO-LANG
Note:
The environment variable LANG has not been defined

@ENVIRONMENT-CURR-TIME
Current time:
-------------
According to the system clock and formatting using the "%c" format
of strftime():
@ENVIRONMENT-NO-TIME
Warning:
The run-time environment does not support a system clock

@ENVIRONMENT-SETLOCALE
Locale:
-------
The function setlocale() can be used to set the locale, that is, items
that are specific to a country or a culture. These include:
- The order for day of the month and the name of the month
- The use of a comma instead of a period to separate decimals
- The order of characters in comparisons

Unfortunately, the C standard only defines the locales "C" (the standard
locale) and "" (the locale defined by the run-time environment, in a
system-specific way, such as via LANG).

It is not possible to check which locales are defined in the current
run-time environment. The program simply reports the current locale
(probably "C").

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

@COMPILER-MACROS
Compiler macros
---------------
In general compilers predefine a number of macros that can be used to
identify certain characteristics of the compiler and the run-time
environment.

Standard macros include:
__STDC__     Used by standard-compliant compilers to indicate that the
             standard is in effect. Particularly useful for enabling
             prototypes.
             This macro is not always defined if you are tacitly allowing
             extensions.
__cplusplus  Macro indicating the compiler is expecting C++.
             The presence of this macro often means __STDC__ is NOT
             defined.
_POSIX_SOURCE and _XOPEN_SOURCE
             These macros indicate compliance to the POSIX and XOPEN
             standards, which are mainly used in UNIX.

Almost every compiler also defines one or more usually ill-documented
macros that can tell you something about the computer system or the
run-time environment.

The program will try and find which such macros are defined and what type
of computer you are working on. Note that the test is not exhaustive nor
is it intended to be - it mainly distinguishes between a number of
popular brands.

@COMPILER-MACROS-GENERAL
The following general macros are defined:

@COMPILER-MACROS-SPECIFIC
The following macros are compiler-specific:

@COMPILER-MACROS-COMPILER
These indicate the following:

@COMPILER-MACROS-PREPROCESSOR
Preprocessor macros
-------------------
The C/C++ preprocessor defines several macros that can be useful
for introducing debug messages:
__FILE__    - the name of the source file
__LINE__    - the line in the source file
__DATE__    - the date of compilation
__TIME__    - the time of compilation

For instance:

@COMPILER-MACROS-PREPROC-NOFILE
Warning:
The current preprocessor does NOT work according to the Standard.
It does not define the macro __FILE__

@NUMBER-OPEN-FILES
Maximum number of open files
----------------------------
All systems have a limit to the number of files that can opened at the
same time.

@NUMBER-OPEN-FILES-MACRO
The C header file "stdio.h" provides a macro FOPEN_MAX. It is the
minimum number of files that is guaranteed can be opened.

@NUMBER-OPEN-FILES-NO-MACRO
The C header file "stdio.h" that was used does not provide a
macro FOPEN_MAX, contrary to most compilers.

@NUMBER-OPEN-FILES-LIMITED
The current system allows a limited number of open files:

@NUMBER-OPEN-FILES-UNLIMITED
The current system allows at least 90 open files, which for almost all
practical purposes is quite enough.

@BASIC-DATA-TYPES
Information about data types
----------------------------
The C header files along with the compiler's sizeof() macro
have provided the following information about the basic data types:

@BASIC-DATA-TYPES-MEMORY
Data types concerning memory:
-----------------------------
Three types are important here:
size_t     - The type that is used for the size of an object in memory.
             The maximum value it can reach defines the maximum amount
             of memory you can allocate with one call to malloc().
ptrdiff_t  - The type of value returned when taking the difference
             between two addresses.
             Warning:
             The maximum allowable value may be smaller than can actually
             occur!
void *     - The general pointer type. A "long" variable should be
             large enough to hold it.

@BASIC-DATA-TYPES-FORMAT
Data types and print formats:
-----------------------------
In C formatting strings like "%5f" can yield unexpected results:
- If the number to be printed is larger than fits in the space that
  is allowed by the format, the resulting string will simply be enlarged.
- This poses the threat of array overflows on any sprintf() call.

To illustrate this, the program prints several examples. Note the use
of the "F" suffix. This is to avoid complaints from the compiler. There
may even be a difference between:

   float_var = 1.0e20  ;
and
   float_var = 1.0e20F ;

If this is so, it may be due to local optimisations (the numerical
registers retain the value 1.0e20 in double precision and this is used
in the printf() call).

@MEMORY-ALIGNMENT
Memory alignment
----------------
The organisation of the computer's memory sometimes becomes important:
- When determining the size of a structure
- When allocating memory

The fine grain aspects of the memory organisation are known as alignment.
It basically means that the memory that holds a short integer should
start at an even address, reals at an address that is a quadruple and
so on. Of course this all depends on the specific computer system.

Such alignments may cause gaps in structures and between variables.
Though mainly a waste of space, it can also hide array overflows until
you get to a different computer system (see the section on string
overflow).

The program tries to determine the alignment rules by looking at two
structures and by allocating some memory.

The structures are:

Structure 1:                             Structure 2:
   struct _CharDoubleChar                   struct _DoubleCharChar
   {                                        {
      char    one_char   ;                     double  one_double ;
      double  one_double ;                     char    one_char   ;
      char    one_char   ;                     char    two_char   ;
   } CharDoubleChar ;                       } DoubleCharChar ;

@MEMORY-ALIGNMENT-POINTERS
The malloc() function and its companions usually return addresses that
are a multiple of 2, 4, 8 etc. In this case:

@MEMORY-ALIGNMENT-FAILED-MULT
The successive calls to malloc() returned addresses that do not show
any consistent multiple. We failed to determine such a value.

@STRING-OVERFLOW
String overflow
---------------
The test tries to determine what happens to local variables if a
string is filled with another string that is too large. (C provides
no mechanism to adequately prevent this - you will have to program it
yourself)

The local variables are defined as (in this order!):

   int  int_before ;
   char string[10] ;
   int  int_after  ;

The results may be:
- The integer defined before the string is affected
- The integer defined after the string is affected
- There is no noticeable effect:
  - The memory alignment makes sure there is enough space
  - The stack has been reorganised

@STRING-OVERFLOW-BEFORE
Warning:
The string overflow affected the variable defined BEFORE the string

@STRING-OVERFLOW-AFTER
Warning:
The string overflow affected the variable defined AFTER the string

@STRING-OVERFLOW-NO-EFFECT
Warning:
The string overflow had no effect on either the variable that was
defined BEFORE the string or the one defined AFTER the string.

Caution:
Such errors may remain undetected for a long time! In fact, during the
development of this program it seemed as if the test had no effect, but
after adding enough subroutines, the PC I used ran the program as if
everything was normal and then hang up - or rebooted! Of course, I blamed
these new subroutines at first ...

@STRING-OVERFLOW-MEMORY-ALIGNMENT
The string overflow had no effect due to the memory alignment
for integers.

@STRING-OVERFLOW-STACK-REORGANISED
The string overflow had no effect because the variables in the stack
are not arranged in the order of definition.

@IMPLICIT-CAST
Implicit casting
----------------
The C compiler uses implicit casting in a number of situations. These
may cause very obscure errors, if you are not aware of this feature.
The background is the use of old-style Kernigan and Ritchie prototypes.

The test consists of executing the following code:

   int_val    =   1  ;
   real_val   = 1.1F ;

   printf( "Real, integer: %d %d\n" , real_val , int_val ) ;
   printf( "Real:          %d\n"    , real_val           ) ;
   printf( "Integer:       %d\n"    ,            int_val ) ;

From the first print statement, you would expect a bizarre integer and
then "1". From the second and third print statements simply a repetition
of each number. The result, however:

@FILE-HANDLING
File handling
-------------
Several tests follow to establish the behaviour of the run-time system
with regard to somewhat weird file handling:
- Opening a file that has an empty string as name

Note:
To get more information out of the system, the global variable "errno"
is used. Unfortunately, this does not always work. So, after the
message "System error:", you may not see a useful text.

Well, this is an indication of the quality of the compiler as well,
of course.

Note:
Be sure to include not only the header file "errno.h" but also
the header file "string.h" - this defines the strerror() function.
If not, some systems will present rubbish.

Note:
With some compilers a run-time error may follow that is handled by
an assert() macro. If this happens the program will not continue.
--- To do: think of a solution for this! ---

@FILE-EMPTY-NAME
Test 1: Opening a file with an empty name
-------
The fopen() is called like this:

   pfile  = fopen( "" , "r" ) ;

@FILE-EMPTY-NAME-NULL
The call to fopen() returned NULL - as expected.
@FILE-EMPTY-NAME-SUCCESS
The call to fopen() returned a non-NULL pointer. This means the error
was not caught, or at least not in an obvious way!
@FILE-EMPTY-END
Dummy

@LOGICAL-EXPRESSIONS
Logical expressions
-------------------
The C standard states that programs may short-cut logical expressions
like "a && b" and "a || b":
- If after evaluating "a" it becomes clear that the expression "a && b"
  is false, "b" need not be evaluated
- Similarly, "a || b" is true if "a" is true, and "b" needs not to be
  known.

The program checks how many arguments are evaluated in four different
logical expressions and assumes that short-cuts are the proper way
to evaluate these.

For each expression the program lists the order in which the operands
are used and what value they had.

@LOGICAL-EXPR-NOT-COMPLIANT
The evaluation of the logical expression does not seem compliant

@LOGICAL-EXPR-COMPLIANT
Note:
From the four tests we conclude that the evaluation of logical
expressions is compliant to the standard.

@LOGICAL-EXPR-AND
Test 1: Evaluate a && b, with "a" false
-------
@LOGICAL-EXPR-OR
Test 2: Evaluate a || b, with "a" true
-------
@LOGICAL-EXPR-THREE-OPERANDS
Test 3: Evaluate a && b || c, with "a", "b" and "c" true
-------
@LOGICAL-EXPR-THREE-BRACKETS
Test 4: Evaluate (a && b) || c, with "a", "b" and "c" true
-------
@ARITHMETIC-EVALUATION
Arithmetic evaluations
----------------------
Contrary to logical expressions, the standard does not prescribe any
order to the evaluation of individual terms or the synthesis to one
result.

This means that side effects will depend on the compiler and possibly
compiler options (such as optimisations). Side effects are present
in such expressions as:

   for ( i= 0 ; i < 100 ; a[i++] = b[i] )
      /* No body */ ;

Or:
   a = i + b[i++] ;

The program tests a number of expressions and tries to decide what
order is used. For each expression the program lists the order in
which the operands are used and what value they had.

@ARITHMETIC-TWO-SUM
Test 1: Evaluate a + b
-------
@ARITHMETIC-THREE-SUM
Test 2: Evaluate a + b + c
-------
@ARITHMETIC-FOUR-SUM
Test 3: Evaluate a + b + c + d
-------
@ARITHMETIC-INDECISIVE
Note:
There was no consistent order in the evaluation of these expressions!

@ARITHMETIC-MIXED-TYPE-CALC
The next test consists of a somewhat naive calculation using reals
and integers:

   int_val    = 10                        ;
   float_val  = 1.4                       ;
   int_result = 1.7 + int_val + float_val ;

@INPUT-FEATURES
Features of the input routines
------------------------------
The following tests use sscanf() and atof() to check whether these
functions return the proper values and, possibly, error codes.

@INPUT-SSCANF-RETURN-REGULAR
Test 1: Read two separate strings via sscanf()
-------

@INPUT-SSCANF-RETURN-EOF
Test 2: Try to read beyond the end of the string
-------

@INPUT-SSCANF-RETURN-INVALID
Test 3: Try to read a real value, but the string does not contain one
-------

@INPUT-SSCANF-COMPLIANT
Note:
The behaviour of sscanf() seems compliant to the Standard.

@INPUT-SSCANF-NON-COMPLIANT
Warning:
The behaviour of sscanf() is NOT compliant to the Standard. It does
not return the expected numbers.

@INPUT-ATOF-INVALID-INPUT
Test 4: What does atof() return if the string is not a valid real
-------

@INPUT-NO-ERROR
Warning:
There was no indication of an error. That means that the program is
not capable of detecting the fact that the input did not consist of
a valid number - at least not in this naive way.
