/* @begin@
 *
 *  chksysc.c - program to check the run-time environment for
 *              C programs
 *
 *  Copyright (C) 1998 Arjen Markus
 *
 *  Arjen Markus
 *
 *
 *  General information:
 *  This file contains the following routines:
 *  - main():             Main program
 *  - StripBlanks()       Strip the trailing blanks
 *  - CheckSet():         Check if a set of tests should be performed
 *  - WrMessage():        Write the message text to the output
 *  - WrEnvironment():    Report about argv[0] and relevant environment
 *                        variables
 *  - WrCompilerMacros(): Report about known compiler-specific macros
 *  - WrOpenFiles():      Report about the maximum number of open files
 *  - CompareReals():     Compare a double and a float variable
 *  - WrDataTypes():      Report about the basic data types
 *  - WrMemoryAlignm():   Report about the memory alignment
 *  - WrStringOverflow(): Examine the consequences of a string being
 *                        filled with too many data
 *  - WrFileTest():       Test certain aspects of file handling
 *  - WrLogicExpr():      Test the short-cuts in logical expressions
 *  - WrEvalOrder():      Test the order of evaluation
 *  - WrInputFeatures():  Test certain features of the input functions
 *  - SetNumerics():      Set the arguments to specific values
 *  - WrNumericErrors():  Test deliberate numeric errors (overflow etc.)
 *  - SetStrings():       Set the arguments to specific string values
 *  - WrStringErrors():   Test deliberate errors manipulating strings
 *  - WrMallocErrors():   Test deliberate errors with malloc() routines
 *
 *  Note:
 *  The code uses the following macros to control the checks:
 *  NO_PROTO   - Do not use explicit prototypes, but the K&R style
 *  EXTENSIONS - Include extensions to the Standard (some compilers
 *               will complain about these features and not continue
 *               with error checking)
 *
 *  If you define NO_PROTO, no Standard (ANSI) prototypes will be
 *  defined.
 *
 *  Only if you define EXTENSIONS, the code containing certain
 *  extensions will be compiled.
 *
 *  Note on C++:
 *  The program should compile under C++ as well and would test
 *  the run-time behaviour of C++. For this reason, the 'extern "C"'
 *  prologue has been omitted. Additional checks on the C++ run-time
 *  are done in a separate program.
 *
 * @end@
 */

/*  $Author$
 *  $Date$
 *  $Source$
 *  $Log$
 */

/* Include files
   Note:
   - Including "string.h" was necessary, because strerror() would
     not give proper results on some systems! I only discovered this,
     while running one particular compiler with full warnings.
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <limits.h>
#include <float.h>
#include <errno.h>
#include <locale.h>
#include <time.h>

/* Typical prologue for C++ --- omitted - see remark

#if defined( __cplusplus )
extern "C" {
#endif
------------------------------------------------- */

/* Prototypes
*/
#ifndef NO_PROTO
static void WrEnvironment(    int   argc    , char *argv0   ) ;
static void WrCompilerMacros( void                          ) ;
static void WrOpenFiles(      void                          ) ;
static void WrDataTypes(      void                          ) ;
static void StripBlanks(      char *string                  ) ;
static void WrMessage(        char *keyword                 ) ;
static void WrMemoryAlignm(   void                          ) ;
static void WrStringOverflow( void                          ) ;
static void WrImplicitCast(   void                          ) ;
static void WrFileTest(       void                          ) ;
static void WrLogicExpr(      void                          ) ;
static void WrEvalOrder(      void                          ) ;
static void WrInputFeatures(  void                          ) ;
static void WrNumericErrors(  void                          ) ;
static void WrStringErrors(   void                          ) ;
static void WrMallocErrors(   void                          ) ;

static int  CheckSet(         char   *keyword                 ) ;
static int  OperandValue(     int     operand    , int    value     ) ;
static void CompareReals(     double  double_var , float  float_var ,
                              int    *no_greater , int    *no_lower ,
                              int    *no_equal                      ) ;
#else
static void WrEnvironment(    ) ;
static void WrCompilerMacros( ) ;
static void WrOpenFiles(      ) ;
static void WrDataTypes(      ) ;
static void StripBlanks(      ) ;
static void WrMessage(        ) ;
static void WrMemoryAlignm(   ) ;
static void WrStringOverflow( ) ;
static void WrImplicitCast(   ) ;
static void WrFileTest(       ) ;
static void WrLogicExpr(      ) ;
static void WrEvalOrder(      ) ;
static void WrInputFeatures(  ) ;
static void WrNumericErrors(  ) ;
static void WrStringErrors(   ) ;
static void WrMallocErrors(   ) ;

static int  CheckSet(         ) ;
static int  OperandValue(     ) ;
static void CompareReals(     ) ;
#endif

/* Local macro definitions
*/
#define MAXLEN       75
#define MAXKEYW      20
#define MAXCONTENTS 800
#define OP_RESET  (-9)
#define OP_REPORT (-99)
#define OP_ORDER  (-999)

/* @@------------------------------------------------------------------
    Routine:  main
    Author:   Arjen Markus
    Purpose:  Main program
    Context:  -
    Summary:
              Work through the various checks:
              - Print the sizes and extremes for a number of
                basic data types.
-------------------------------------------------------------------- */
#ifndef NO_PROTO
int main( int argc , char **argv )
#else
int main( argc , argv )
       int  argc    ;
       char **argv  ;
#endif
{
   int check ;
/* Write the introduction
*/
   WrMessage( "@INTRODUCTION" ) ;

   check = CheckSet( "@GENERAL" ) ;

/* General tests: ought to be safe
*/
   if ( check )
   {
/* Report about argv[0] and relevant environment variables
*/
      WrEnvironment( argc , argv[0] ) ;

/* Report about the known compiler-specific macros
*/
      WrCompilerMacros() ;

/* Report about the maximum number of open files
*/
      WrOpenFiles() ;

/* Report about the basic data types
*/
      WrDataTypes() ;

/* Report about memory alignments
*/
      WrMemoryAlignm() ;

/* Examine the consequences of "string overflow"
*/
      WrStringOverflow() ;

/* Examine the implicit casts
*/
      WrImplicitCast() ;

/* Test file handling in some detail
*/
      WrFileTest() ;

/* Test short-cuts in evaluating logical expressions
*/
      WrLogicExpr() ;

/* Test the order of arithmetic evaluations
*/
      WrEvalOrder() ;

/* Test certain features of the input functions
*/
      WrInputFeatures() ;
   }

/* Test some possibly disruptive numerical features
*/
   WrNumericErrors() ;

/* Test some possibly disruptive string features
*/
   WrStringErrors() ;

/* Test some possibly disruptive malloc/free() features
*/
   WrMallocErrors() ;

/* End of program
*/
   return 0 ;
}

/* @@------------------------------------------------------------------
    Routine:  StripBlanks()
    Author:   Arjen Markus
    Purpose:  Strip the trailing blanks
    Context:  Used by WrMessage()
    Summary:
              Start at the end of the string and set each space and
              tab to '\0' until we encounter a non-blank
-------------------------------------------------------------------- */
#ifndef NO_PROTO
static void StripBlanks(
   char *string       )  /* IO String to be stripped */
#else
static void StripBlanks( string   )
   char *string       ;
#endif
{
   int i      ;
   int length ;

   length = strlen( string ) ;

   for ( i = length-1 ; i >=0 ; i -- )
   {
      if ( string[i] == ' ' || string[i] == '\t' )
      {
         string[i] = '\0' ;
      }
      else
      {
         break ;
      }
   }

   return ;
}

/* @@------------------------------------------------------------------
    Routine:  CheckSet()
    Author:   Arjen Markus
    Purpose:  Write the message text to the output
    Context:  Used to select test sets
    Summary:
              If this is the first call, read the file "chksys.set"
              In all cases: look for a line starting with the keyword.
              If it exists, then return 1, otherwise 0
-------------------------------------------------------------------- */
#ifndef NO_PROTO
static int CheckSet(
   char *keyword      )  /* I Keyword that identifies the test set */
#else
static int CheckSet(
   keyword )
   char *keyword ;
#endif
{
   FILE        *infile                    ;
   int          i                         ;
   int          found                     ;
   char        *pchar                     ;
   static char  keywords[MAXKEYW][MAXLEN] ;
   static int   init   = 1                ;
   static int   nokeyw = 0                ;

/* On first entry, read the entire file into the buffer
*/
   if ( init == 1 )
   {
      init = 0 ;

      infile = fopen( "chksysc.set" , "r" ) ;
      if ( infile == NULL )
      {
         printf( "Warning: no keywords file - general test only!" ) ;
         nokeyw = 1 ;
         strcpy( keywords[0] , "@GENERAL" ) ;
      }
      else
      {
         while ( nokeyw < MAXKEYW )
         {
            fgets( &keywords[nokeyw][0] , MAXLEN , infile ) ;
            if ( feof( infile ) || ferror( infile ) )
            {
               break ;
            }
            if ( keywords[nokeyw][0] == '#' )
            {
               continue ; /* Skip comments */
            }
            pchar = strchr( &keywords[nokeyw][0] , '\n' ) ;
            if ( pchar != NULL )
            {
               *pchar = '\0' ;
            }
            else
            {
               keywords[nokeyw][MAXLEN-1] = '\0' ;
            }
            /* Strip trailing blanks
            */
            StripBlanks( &keywords[nokeyw][0] ) ;
            nokeyw ++ ;
         }
         fclose( infile ) ;
      }
   }

/* The regular processing of the keyword
   Note:
   The order of the if-statements is important
*/
   found  = 0 ;

   for ( i = 0 ; i < nokeyw ; i ++ )
   {
      if ( strcmp( &keywords[i][0] , keyword ) == 0 )
      {
         found = 1 ;
      }
   }

   if ( ! found )
   {
      printf( "(Skipping: %s)\n" , keyword ) ;
   }

   return found ;
}

/* @@------------------------------------------------------------------
    Routine:  WrMessage()
    Author:   Arjen Markus
    Purpose:  Write the message text to the output
    Context:  Used to print the message text uniformly
    Summary:
              *** dummy for the moment ***
-------------------------------------------------------------------- */
#ifndef NO_PROTO
static void WrMessage(
   char *keyword      )  /* I Keyword that identifies the message */
#else
static void WrMessage(
   keyword )
   char *keyword ;
#endif
{
   FILE        *infile                        ;
   int          i                             ;
   int          found                         ;
   int          prline                        ;
   char        *pchar                         ;
   static char  contents[MAXCONTENTS][MAXLEN] ;
   static int   init   = 1                    ;
   static int   noline = 0                    ;

/* On first entry, read the entire file into the buffer
*/
   if ( init == 1 )
   {
      init = 0 ;

      infile = fopen( "chksysc.msg" , "r" ) ;
      if ( infile == NULL )
      {
         printf( "Warning: no messages file!" ) ;
      }
      else
      {
         for ( i = 0 ; i < MAXCONTENTS ; i ++ )
         {
            fgets( &contents[i][0] , MAXLEN , infile ) ;
            if ( feof( infile ) || ferror( infile ) )
            {
               break ;
            }
            noline ++ ;
            pchar = strchr( &contents[i][0] , '\n' ) ;
            if ( pchar != NULL )
            {
               *pchar = '\0' ;
            }
            else
            {
               contents[i][MAXLEN-1] = '\0' ;
            }
            /* Strip trailing blanks
            */
            StripBlanks( &contents[i][0] ) ;
         }
         fclose( infile ) ;
      }
   }

/* The regular processing of the keyword
   Note:
   The order of the if-statements is important
*/
   found  = 0 ;
   prline = 0 ;

   for ( i = 0 ; i < noline ; i ++ )
   {
      if ( contents[i][0] == '@' )
      {
         prline = 0 ;
      }
      if ( prline == 1 )
      {
         printf( "%s\n" , &contents[i][0] ) ;
      }
      if ( strcmp( &contents[i][0] , keyword ) == 0 )
      {
         prline = 1 ;
         found  = 1 ;
      }
   }

   if ( ! found )
   {
      printf( "(Not found: %s)\n" , keyword ) ;
   }

   return ;
}

/* @@------------------------------------------------------------------
    Routine:  WrEnvironment()
    Author:   Arjen Markus
    Purpose:  Report about argv[0] and relevant environment variables
    Context:  Used by main()
    Summary:
              Report the value of argv[0] and some environment
              variables. Report the cautions about putenv()
-------------------------------------------------------------------- */
#ifndef NO_PROTO
static void WrEnvironment( int argc  , char *argv0 )
#else
static void WrEnvironment( argc , argv0 )
   int  argc   ;
   char *argv0 ;
#endif
{
   char      *penv            ;
   char      *plocale         ;
   char       time_string[40] ;
   time_t     curr_time       ;
   struct tm *curr_tm         ;

   WrMessage( "@ENVIRONMENT" ) ;

   if ( argc != 0 )
   {
      printf( "Argument 0: %s\n\n" , argv0 ) ;
   }
   else
   {
      WrMessage( "@ENVIRONMENT-NO-COMMAND-ARGS" ) ;
   }

   penv = getenv( "PATH" ) ;

   if ( penv != NULL )
   {
      printf( "PATH variable: %s\n\n" , penv ) ;
   }
   else
   {
      WrMessage( "@ENVIRONMENT-NO-PATH" ) ;
   }

   penv = getenv( "LANG" ) ;

   if ( penv != NULL )
   {
      printf( "LANG variable: %s\n\n" , penv ) ;
   }
   else
   {
      WrMessage( "@ENVIRONMENT-NO-LANG" ) ;
   }

   curr_time = time( NULL ) ;
   curr_tm   = localtime( &curr_time ) ;

   if ( curr_time != (time_t) -1 )
   {
      WrMessage( "@ENVIRONMENT-CURR-TIME" ) ;
      strftime( time_string , sizeof( time_string ) , "%c" , curr_tm ) ;
      printf( "Current time: %s\n\n" , time_string ) ;
   }
   else
   {
      WrMessage( "@ENVIRONMENT-NO-TIME" ) ;
   }

   WrMessage( "@ENVIRONMENT-SETLOCALE" ) ;

   plocale = setlocale( LC_ALL , "" ) ;
   printf( "Current locale: %s\n" , plocale ) ;

   return ;
}

/* @@------------------------------------------------------------------
    Routine:  WrCompilerMacros()
    Author:   Arjen Markus
    Purpose:  Report about the known compiler-specific macros
    Context:  Used by main()
    Summary:
              Use #ifdefs and a list of known compiler-specific macros
              to selectively print statements about their existence.
              Indicate the type of compiler from this.
-------------------------------------------------------------------- */
static void WrCompilerMacros( void )
{
#define PRINT_MACRO( a , b )  printf( "Macro: %s - no value (?)\n" , (a) )
#define PRINT_INTVAL( a , b ) printf( "Macro: %s - value: %d\n" , (a) , (b) )

#define COMPILER "Unknown"

/* Write the header
*/
   WrMessage( "@COMPILER-MACROS" ) ;

/* First report some general macros
*/
   WrMessage( "@COMPILER-MACROS-GENERAL" ) ;

#if defined( __STDC__ )
   PRINT_INTVAL( "__STDC__" , __STDC__ ) ;
#else
   printf( "Warning: no macro __STDC__ defined\n" ) ;
#endif
#if defined( __cplusplus )
   PRINT_MACRO( "__cplusplus" , __cplusplus ) ;
#endif
#if defined( _POSIX_SOURCE )
   PRINT_MACRO( "_POSIX_SOURCE" , _POSIX_SOURCE ) ;
#endif
#if defined( _XOPEN_SOURCE )
   PRINT_MACRO( "_XOPEN_SOURCE" , _XOPEN_SOURCE ) ;
#endif
   printf( "\n" ) ;

/* Then report some specific macros
*/
   WrMessage( "@COMPILER-MACROS-SPECIFIC" ) ;

/* MS C/Visual C/C++
*/
#if defined( MSDOS )
   PRINT_MACRO( "MSDOS" , MSDOS ) ;
#endif
#if defined( M_I86 )
   PRINT_MACRO( "M_I86" , M_I86 ) ;
#endif
#if defined( _MSC_VER )
#undef  COMPILER
#define COMPILER "MicroSoft C or Visual C/C++"
   PRINT_INTVAL( "_MSC_VER" , _MSC_VER ) ;
#endif

/* Sun Workstation (SunOS or Solaris)
*/
#if defined( sun )
   PRINT_MACRO( "sun" , sun ) ;
#endif
#if defined( __sun__ )
   PRINT_MACRO( "__sun__" , __sun__ ) ;
#endif
#if defined( sun ) || defined( __sun__ )
#undef  COMPILER
#define COMPILER "C/C++ on SunOs or Solaris workstation"
#endif

/* HP Workstation
*/
#if defined( __hpux )
#undef  COMPILER
#define COMPILER "C/C++ for HP workstation"
   PRINT_MACRO( "__hpux" , __hpux ) ;
#endif

/* IBM AIX Workstation
*/
#if defined( _AIX )
#undef  COMPILER
#define COMPILER "C/C++ for IBM AIX workstation"
   PRINT_MACRO( "_AIX" , _AIX ) ;
#endif

/* SCO UNIX on PC
*/
#if defined( M_UNIX ) && defined( M_I86 )
#undef  COMPILER
#define COMPILER "C/C++ for SCO UNIX on PC"
   PRINT_MACRO( "M_UNIX" , M_UNIX ) ;
   PRINT_MACRO( "M_I86"  , M_I86  ) ;
#endif

/* Silicon Graphics
*/
#if defined( sgi )
#undef  COMPILER
#define COMPILER "C/C++ for Silicon Graphics workstation"
   PRINT_MACRO( "sgi" , sgi ) ;
#endif

/* DEC/Alpha
*/
#if defined( __osf__ ) && defined( __alpha )
#undef  COMPILER
#define COMPILER "C/C++ for DEC/Alpha workstation"
   PRINT_MACRO( "__osf__" , __osf__ ) ;
   PRINT_MACRO( "__alpha" , __alpha ) ;
#endif

/* Print the compiler string
*/
   printf(    "\n"                          ) ;
   WrMessage( "@COMPILER-MACROS-COMPILER"   ) ;
   printf(    "Compiler: %s\n\n" , COMPILER ) ;

/* Print a message about useful preprocessor symbols
*/
#if defined( __FILE__ )
   printf(    "\n"                           ) ;
   WrMessage( "@COMPILER-MACROS-PREPOCESSOR" ) ;
   printf(    "File, line: %s %d\n\n" , __FILE__ , __LINE__ ) ;
#else
   printf(    "\n"                           ) ;
   WrMessage( "@COMPILER-MACROS-PREPOC-NOFILE" ) ;
#endif


/* We are done
*/
   return ;
}
/* @@------------------------------------------------------------------
    Routine:  WrOpenFiles()
    Author:   Arjen Markus
    Purpose:  Report the maximum number of open files
    Context:  Used by main()
    Summary:
              Check for the macro FOPEN_MAX, report its value
              Open as many temporary file as allowed and count them.
              Then close the files again.
              Report the number.
-------------------------------------------------------------------- */
static void WrOpenFiles( void )
{
#define MAX_FILES 90

   int    i                  ;
   int    no_files           ;
   FILE  *infiles[MAX_FILES] ;

/* Write the header
*/
   WrMessage( "@NUMBER-OPEN-FILES" ) ;

#ifdef FOPEN_MAX
   WrMessage( "@NUMBER-OPEN-FILES-MACRO" ) ;
   printf( "Value of the macro: %ld\n" , (long) FOPEN_MAX ) ;
#else
   WrMessage( "@NUMBER-OPEN-FILES-NO-MACRO" ) ;
#endif

   no_files = 0 ;

   for ( i = 0 ; i < MAX_FILES ; i ++ )
   {
      infiles[i] = tmpfile() ;

      if ( infiles[i] == NULL )
      {
         break ;
      }
      else
      {
         no_files = i + 1 ;
      }
   }

/* Report the result
*/
   if ( no_files >= MAX_FILES )
   {
      WrMessage( "@NUMBER-OPEN-FILES-UNLIMITED" ) ;
   }
   else
   {
      WrMessage( "@NUMBER-OPEN-FILES-LIMITED" ) ;
      printf( "Number of files opened: %d\n\n" , no_files ) ;
   }

/* Close the files again
*/
   for ( i = 0 ; i < no_files ; i ++ )
   {
      fclose( infiles[i] ) ;
   }

/* We are done
*/
   return ;
}

/* @@------------------------------------------------------------------
    Routine:  CompareReals()
    Author:   Arjen Markus
    Purpose:  Compare a double and a float variable
    Context:  Used by WrDataTypes() to avoid influences by optimisation
    Summary:
              Compare the two variables and add the result to the
              counting arguments.
-------------------------------------------------------------------- */
#ifndef NOPROTO
static void CompareReals(
   double  double_var      ,
   float   float_var       ,
   int    *no_real_greater ,
   int    *no_real_lower   ,
   int    *no_real_equal   )
#else
static void CompareReals(
      double_var      , float_var       , no_real_greater ,
      no_real_lower   , no_real_equal                     )
   double  double_var      ,
   float   float_var       ,
   int    *no_real_greater ,
   int    *no_real_lower   ,
   int    *no_real_equal   )
#endif
{
   double sngl_prec ;

   sngl_prec = (double) float_var ;

   if ( fabs( double_var ) > fabs( sngl_prec ) )
   {
      (*no_real_greater) ++ ;
   }
   if ( fabs( double_var ) < fabs( sngl_prec ) )
   {
      (*no_real_lower) ++ ;
   }
   if ( fabs( double_var ) == fabs( sngl_prec ) )
   {
      (*no_real_equal) ++ ;
   }

   return ;
}

/* @@------------------------------------------------------------------
    Routine:  WrDataTypes()
    Author:   Arjen Markus
    Purpose:  Report the sizes and extremes for basic data types
    Context:  Used by main()
    Summary:
              Print the size in bytes and the minimum and maximum
              values for various data types. Include other useful
              information as well.
-------------------------------------------------------------------- */
static void WrDataTypes( void )
{
/* Note: all are signed types, except for "char", for which it
   depends on the implementation
*/
   char    char_var        ;
   short   short_var       ;
   int     int_var         ;
   long    long_var        ;
   float   float_var       ;
   double  double_var      ;
   void   *void_ptr        ;
   int     i               ;
   int     no_dbl_greater  ;
   int     no_dbl_lower    ;
   int     no_dbl_equal    ;
   int     no_real_greater ;
   int     no_real_lower   ;
   int     no_real_equal   ;

/* The print command uses the same pattern:
   - print the size, the (absolute) minimum and the maximum
*/
   WrMessage( "@BASIC-DATA-TYPES" ) ;
   printf( "Type              Sizeof    Minimum        Maximum\n" ) ;
   printf( "Character (char): %5ld     %10ld     %10ld\n" ,
      (long) sizeof( char_var )  , (long) CHAR_MIN , (long) CHAR_MAX ) ;
   printf( "Short integer:    %5ld     %10ld     %10ld\n" ,
      (long) sizeof( short_var ) , (long) SHRT_MIN , (long) SHRT_MAX ) ;
   printf( "Integer (int):    %5ld     %10ld     %10ld\n" ,
      (long) sizeof( int_var )   , (long) INT_MIN  , (long) INT_MAX ) ;
   printf( "Long integer:     %5ld     %10ld     %10ld\n" ,
      (long) sizeof( long_var )  ,        LONG_MIN ,        LONG_MAX ) ;
   printf( "\n" ) ;
   printf( "Type              Sizeof    Minimum         Maximum\n" ) ;
   printf( "Float:            %5ld     %12g    %12g\n" ,
      (long) sizeof( float_var ) , FLT_MIN , FLT_MAX ) ;
   printf( "Double:           %5ld     %12lg    %12lg\n" ,
      (long) sizeof( float_var ) , DBL_MIN , DBL_MAX ) ;
   printf( "\n" ) ;

/* Also provide information about memory stuff
*/
   WrMessage( "@BASIC-DATA-TYPES-MEMORY" ) ;
   printf( "Type              Sizeof    Maximum\n" ) ;
   printf( "size_t:           %5ld     %10lu\n" ,
      (long) sizeof( size_t ) , (unsigned long) UINT_MAX ) ;
   printf( "ptrdiff_t         %5ld     %10ld\n" ,
      (long) sizeof( size_t ) , (long) INT_MAX ) ;
   printf( "Void pointer:     %5ld\n" ,
      (long) sizeof( void_ptr ) ) ;
   printf( "\n" ) ;

/* Print some numbers
*/
   float_var = 1.0e20F ;
   WrMessage( "@BASIC-DATA-TYPES-FORMAT" ) ;
   printf( "1.0e20F printed with '%%g':     %g\n"  , float_var ) ;
   printf( "1.0e20F printed with '%%f':     %f\n"  , float_var ) ;
   printf( "1.0e20F printed with '%%5f':    %5f\n" , float_var ) ;
   float_var = 1.0e20 ;
   printf( "1.0e20 (!) printed with '%%f':  %f\n"  , float_var ) ;
   float_var = 1.0F ;
   printf( "1.0F printed with '%%g':        %g\n"  , float_var ) ;
   printf( "1.0F printed with '%%f':        %f\n"  , float_var ) ;
   printf( "1.0F printed with '%%5f':       %5f\n" , float_var ) ;
   float_var = 1.0e-20F ;
   printf( "1.0e-20F printed with '%%g':     %g\n"  , float_var ) ;
   printf( "1.0e-20F printed with '%%f':     %f\n"  , float_var ) ;
   printf( "1.0e-20F printed with '%%5f':    %5f\n" , float_var ) ;
   printf( "\n" ) ;

/* Test the method of conversion between floats and doubles:
   The Standard does not prescribe anything and therefore:
   - Consequent truncation
   - Consequent rounding up
   - Correct rounding
   could all result (possibly even a mixture!)

   If consequent truncation occurs, then: abs(float) <= abs(double)
   If consequent rounding up occurs, then: abs(float) >= abs(double)
   If correct rounding occurs, there is no such simple relationship
   Note:
   The naieve method gives problems (because of low-level
   optimisations), so compare that with a more complicated one
*/
   WrMessage( "@BASIC-DATA-TYPES-CONVERSION-SIMPLE" ) ;
   no_dbl_greater = 0 ;
   no_dbl_lower   = 0 ;
   no_dbl_equal   = 0 ;

   for ( i = 0 ; i < 100 ; i ++ )
   {
      double_var = (double) rand() / (double) RAND_MAX - 0.5 ;
      float_var  = (float) double_var                        ;
      if ( fabs( double_var ) > fabs( (double) float_var ) )
      {
         no_dbl_greater ++ ;
      }
      if ( fabs( double_var ) < fabs( (double) float_var ) )
      {
         no_dbl_lower ++ ;
      }
      if ( fabs( double_var ) == fabs( (double) float_var ) )
      {
         no_dbl_equal ++ ;
      }
   }
   printf( "Result of 100 trials:\n\
Double variable greater than float variable: %d\n\
Double variable lower than float variable:   %d\n\
Double variable equal to float variable:     %d\n\n" ,
      no_dbl_greater , no_dbl_lower , no_dbl_equal ) ;

   WrMessage( "@BASIC-DATA-TYPES-CONVERSION-COMPLEX" ) ;
   no_real_greater = 0 ;
   no_real_lower   = 0 ;
   no_real_equal   = 0 ;

   for ( i = 0 ; i < 100 ; i ++ )
   {
      double_var = (double) rand() / (double) RAND_MAX - 0.5 ;
      float_var  = (float) double_var                        ;
      CompareReals( double_var    , float_var    , &no_real_greater ,
                    &no_real_lower , &no_real_equal                 ) ;
   }
   printf( "Result of 100 trials:\n\
Double variable greater than float variable: %d\n\
Double variable lower than float variable:   %d\n\
Double variable equal to float variable:     %d\n\n" ,
      no_real_greater , no_real_lower , no_real_equal ) ;

/* We are done
*/
   return ;
}

/* @@------------------------------------------------------------------
    Routine:  WrMemoryAlignm()
    Author:   Arjen Markus
    Purpose:  Report about the memory alignment
    Context:  Used by main()
    Summary:
              Print the size in bytes of several structures.
              Allocate a single "char" three times and determine
              the distance in memory.
-------------------------------------------------------------------- */
static void WrMemoryAlignm( void )
{
/* Note: the structures are defined in such a way that alignment
   requirements are shown
*/
   struct _CharDoubleChar
   {
      char    one_char   ;
      double  one_double ;
      char    two_char   ;
   } CharDoubleChar ;
   struct _DoubleCharChar
   {
      double  one_double ;
      char    one_char   ;
      char    two_char   ;
   } DoubleCharChar ;

   char *pchar[3]         ;
   long diff1     , diff2 ;

/* Print and interpret the sizes of the structures
*/
   WrMessage( "@MEMORY-ALIGNMENT" ) ;

   printf( "Expected size (no alignment): %d\n" ,
               (int) ( 2 * sizeof( char ) + sizeof( double ) ) ) ;
   printf( "Structure 1 has size %d\n" ,
           (int) sizeof( CharDoubleChar ) ) ;
   printf( "Structure 2 has size %d\n" ,
           (int) sizeof( DoubleCharChar ) ) ;
   printf( "\n" ) ;

/* Allocate (and free) three chars: anything interesting like multiples
   of eight?
*/
   pchar[0] = malloc( sizeof( char ) ) ;
   pchar[1] = malloc( sizeof( char ) ) ;
   pchar[2] = malloc( sizeof( char ) ) ;

   diff1    = (long) pchar[1] - (long) pchar[0] ;
   diff2    = (long) pchar[2] - (long) pchar[1] ;

   WrMessage( "@MEMORY-ALIGNMENT-POINTERS" ) ;
   if ( diff1 == diff2 )
   {
      printf(
"malloc() seems to return addresses that are multiples of %ld\n\n" ,
         diff1 ) ;
   }
   else
   {
      WrMessage( "@MEMORY-ALIGNMENT-FAILED-MULT" ) ;
   }

   free( pchar[0] ) ;
   free( pchar[1] ) ;
   free( pchar[2] ) ;

   return ;
}

/* @@------------------------------------------------------------------
    Routine:  WrStringOverflow()
    Author:   Arjen Markus
    Purpose:  Examine what happens if a string is filled with too many
              data
    Context:  Used by main()
    Summary:
              Fill a string of ten characters with one extra.
              Examine the values of the variables "around" it in
              the stack.
    Note:
              During the development of this program I encountered
              a severe problem with this routine:
              The first version did not contain the second_string
              variable. Everything seemed to work fine, until I
              added the WrEnvironment() routine to the program.
              Then the PC I used just hung up. Some detective work
              and hard labour revealed this routine to be the problem.
              This means that even though you may not encounter a
              problem today, the beast may be lurking in some dark
              corner to hatch out tomorrow, when you have extended
              your program!
-------------------------------------------------------------------- */
static void WrStringOverflow( void )
{
/* Note: the order of the declarations is important here!
*/
   int  int_before ;
   char string[10] ;
   int  int_after  ;
   long diff1      ;
   long diff2      ;
   char second_string[10] ; /* Dummy - see note */

/* Print and interpret the sizes of the structures
*/
   strcpy( second_string , "Dummy" ) ;

   WrMessage( "@STRING-OVERFLOW"   ) ;

   int_before = -1 ; /* Assuming two-complements numbers */
   int_after  = -1 ;

   printf( "Adresses of the variables:\n\
int before: %p\n\
string:     %p\n\
int after:  %p\n" , &int_before , string , &int_after ) ;

   strcpy( string , "1234567890" ) ;

   if ( int_before != -1 )
   {
      WrMessage( "@STRING-OVERFLOW-BEFORE" ) ;
   }
   if ( int_after  != -1 )
   {
      WrMessage( "@STRING-OVERFLOW-AFTER" ) ;
   }
   if ( int_before == -1 && int_after  == -1 )
   {
      WrMessage( "@STRING-OVERFLOW-NO-EFFECT" ) ;

      diff1 = (long) &int_after - (long) &string[0]  ;
      diff2 = (long) &int_after - (long) &int_before ;

      if ( diff1 > sizeof( string ) )
      {
         WrMessage( "@STRING-OVERFLOW-MEMORY-ALIGNMENT" ) ;
      }
      else
      {
         if ( diff2 > diff1 )
         {
            WrMessage( "@STRING-OVERFLOW-STACK-REORGANISED" ) ;
         }
         else
         {
            WrMessage( "@STRING-OVERFLOW-MEMORY-ALIGNMENT" ) ;
         }
      }
   }

   return ;
}

/* @@------------------------------------------------------------------
    Routine:  WrImplicitCast()
    Author:   Arjen Markus
    Purpose:  Examine the implicit casts that may occur
    Context:  Used by main()
    Summary:
              Print a real using the wrong format.
              Pass a real to a subroutine that has no prototype.
-------------------------------------------------------------------- */
static void WrImplicitCast( void )
{
   int   int_val  ;
   float real_val ;

/* Print a real and integer - the real with the wrong format
*/
   WrMessage( "@IMPLICIT-CAST" ) ;

   int_val    =   1  ;
   real_val   = 1.1F ;

   printf( "Real, integer: %d %d\n" , real_val , int_val ) ;
   printf( "Real:          %d\n"    , real_val           ) ;
   printf( "Integer:       %d\n"    ,            int_val ) ;
   printf( "\n"                                          ) ;

/* Next part: using a subroutine with no prototype
*/
/* MixedHead( real_val , int_val ) ; */

   return ;
}
/* @@------------------------------------------------------------------
    Routine:  WrFileTest()
    Author:   Arjen Markus
    Purpose:  Test certain aspects of file handling
    Context:  Used by main()
    Summary:
              This routine performs a series of tests:
              Test 1: Call fopen() with an empty string (reading)
              Test 2: Call fopen() on the same file twice (writing)
              Test 3: Use fgets() on a binary file
    Note:
              Use "errno" to print the corresponding error message.
              As this is defined as "volatile" on some systems, copy
              the value directly into an auxiliary variable.
-------------------------------------------------------------------- */
static void WrFileTest( void )
{
   FILE *pfile         ;
   FILE *pfile2        ;
   char *pname         ;
   char  string[20]    ;
   char  tmp_file[256] ;
   int   err_no        ;

   WrMessage( "@FILE-HANDLING" ) ;

/* Test 1: Call fopen() with an empty string (reading)
*/
   WrMessage( "@FILE-EMPTY-NAME" ) ;

   errno  = 0 ;
   pfile  = fopen( "" , "r" ) ;
   err_no = errno ;
   if ( pfile == NULL )
   {
      WrMessage( "@FILE-EMPTY-NAME-NULL" ) ;
      printf( "System error: %s\n" , strerror( err_no ) ) ;
   }
   else
   {
      WrMessage( "@FILE-EMPTY-NAME-SUCCESS" ) ;
      printf( "System error: %s\n" , strerror( err_no ) ) ;
      fclose( pfile ) ;
   }
   printf( "\n" ) ;

/* Test 2: Call fopen() twice on the same file (writing)
*/
   WrMessage( "@FILE-OPEN-TWICE" ) ;

   errno  = 0                  ;
   pname  = tmpnam( tmp_file ) ;
   err_no = errno              ;
   if ( pname != NULL )
   {
      errno  = 0 ;
      pfile  = fopen( tmp_file , "w" ) ;
      err_no = errno ;
      if ( pfile == NULL )
      {
         WrMessage( "@FILE-OPEN-ONCE-ERROR" ) ;
         printf( "System error: %s\n" , strerror( err_no ) ) ;
      }
      else
      {
         pfile2 = fopen( tmp_file , "w" ) ;
         err_no = errno ;
         if ( pfile2 == NULL )
         {
            WrMessage( "@FILE-OPEN-TWICE-ERROR" ) ;
            printf( "System error: %s\n" , strerror( err_no ) ) ;
         }
         else
         {
            WrMessage( "@FILE-OPEN-TWICE-NO-ERROR" ) ;
            printf( "System error: %s\n" , strerror( err_no ) ) ;
            fclose( pfile2 ) ;
         }
         fclose( pfile ) ;

/* Now, get rid of the file
*/
         pfile = fopen( tmp_file , "r" ) ;
         if ( pfile != NULL )
         {
            fclose( pfile    ) ;
            remove( tmp_file ) ;
         }
      }
   }
   else
   {
      WrMessage( "@FILE-TMP-NAME-ERROR" ) ;
      printf( "System error: %s\n" , strerror( err_no ) ) ;
      fclose( pfile ) ;
   }
   printf( "\n" ) ;

/* Test 3: Use fgets() on a binary file
*/
   WrMessage( "@FILE-MIX-FORMATTED-BINARY" ) ;

   errno  = 0                  ;
   pname  = tmpnam( tmp_file ) ;
   err_no = errno              ;
   if ( pname != NULL )
   {
      errno  = 0 ;
      pfile  = fopen( tmp_file , "wb" ) ;
      err_no = errno ;
      if ( pfile == NULL )
      {
         WrMessage( "@FILE-OPEN-ONCE-ERROR" ) ;
         printf( "System error: %s\n" , strerror( err_no ) ) ;
      }
      else
      {
         fwrite( tmp_file , 1 , sizeof( tmp_file ) , pfile ) ;
         fclose( pfile )                                     ;
         errno  = 0                                          ;
         pfile  = fopen( tmp_file , "r" )                    ;
         err_no = errno                                      ;
         if ( pfile == NULL )
         {
            WrMessage( "@FILE-MIX-REOPEN-ERROR" ) ;
            printf( "System error: %s\n" , strerror( err_no ) ) ;
         }
         else
         {
            errno  = 0                   ;
            fgets( string , 20 , pfile ) ;
            err_no = errno               ;
            if ( feof( pfile ) == 0 && ferror( pfile ) == 0 )
            {
               WrMessage( "@FILE-MIX-READ-NO-ERROR" ) ;
            }
            else
            {
               WrMessage( "@FILE-MIX-READ-NO-ERROR" ) ;
               printf( "System error: %s\n" , strerror( err_no ) ) ;
            }
            fclose( pfile ) ;
         }
         remove( tmp_file ) ;
      }
   }

/* We are done
*/
   return ;
}

/* @@------------------------------------------------------------------
    Routine:  OperandValue()
    Author:   Arjen Markus
    Purpose:  Register the order of evaluation in expressions
    Context:  Used by WrLogicExpr() and WrEvalOrder()
    Summary:
              Register the operands in order of entry, write out
              their value and return their value.
              If asked, reset the registration or report the order
              explicitly.
    Note:
              Reset and registration: PM
-------------------------------------------------------------------- */
#ifndef NO_PROTO
static int OperandValue( int operand , int value )
#else
static int OperandValue( operand , value )
   int operand ;
   int value   ;
#endif
{
   int        retval   ;
   static int number   ;
   static int order    ;
   static int previous ;

/* Select the cases:
   - OP_RESET:  reset the registration
   - OP_REPORT: report the number of calls
   - OP_ORDER:  report the order of the arguments
*/
   if ( operand == OP_RESET )
   {
      number   = 0 ;
      order    = 0 ;
      previous = 0 ; /* Essential */
      retval   = 0 ;
   }
   else if ( operand == OP_REPORT )
   {
      retval = number ;
   }
   else if ( operand == OP_ORDER )
   {
      /* The order variable represents whether the ordering of
         the evaluation is from left to right, the other way around
         or mixed
      */
      if ( order == (number-1) )
      {
         retval = 1 ;
      }
      else if ( (-order) == (number-1) )
      {
         retval = -1 ;
      }
      else
      {
         retval = 0 ;
      }
   }
   else
   {
      printf( "   Operand %d: value %d\n" , operand , value ) ;

      number ++ ;
      if ( previous != 0 )
      {
         if ( previous < operand )
         {
            order ++ ;
         }
         else
         {
            order -- ;
         }
      }
      previous = operand ;

      retval = value ;
   }

   return retval ;
}

/* @@------------------------------------------------------------------
    Routine:  WrLogicExpr()
    Author:   Arjen Markus
    Purpose:  Test short-cuts used in logical expressions
    Context:  Used by main()
    Summary:
              This routine checks if short-cuts are used and how
              smart the compiler is in this respect.
              For several logical expressions the evaluation order
              is recorded and examined.
    Note:
              The Standard says that the program should use
              short-cuts in logical expressions:
                 if ( pstr == NULL || pstr[0] == '\0' ) { ... }
              should therefore never cause any problems.
-------------------------------------------------------------------- */
static void WrLogicExpr( void )
{
   int result ;
   int compl  ;

   WrMessage( "@LOGICAL-EXPRESSIONS" ) ;

   compl = 0 ;

/* Test 1: Evaluating a && b
*/
   WrMessage( "@LOGICAL-EXPR-AND" ) ;

   result = OperandValue( OP_RESET , 0 ) ;
   result = OperandValue( 1 , 0 ) && OperandValue( 2 , 0 ) ;
   result = OperandValue( OP_REPORT , 0 ) ;
   if ( result == 1 )
   {
      compl ++ ;
      printf( "\n" ) ;
   }
   else
   {
      WrMessage( "@LOGICAL-EXPR-NOT-COMPLIANT" ) ;
   }

/* Test 2: Evaluating a || b
*/
   WrMessage( "@LOGICAL-EXPR-OR" ) ;

   result = OperandValue( OP_RESET , 0 ) ;
   result = OperandValue( 1 , 1 ) || OperandValue( 2 , 1 ) ;
   result = OperandValue( OP_REPORT , 0 ) ;
   if ( result == 1 )
   {
      compl ++ ;
      printf( "\n" ) ;
   }
   else
   {
      WrMessage( "@LOGICAL-EXPR-NOT-COMPLIANT" ) ;
   }

/* Test 3: Evaluating a && b || c
*/
   WrMessage( "@LOGICAL-EXPR-THREE-OPERANDS" ) ;

   result = OperandValue( OP_RESET , 0 ) ;
   result = OperandValue( 1 , 1 ) && OperandValue( 2 , 1 )
                                  || OperandValue( 3 , 1 ) ;
   result = OperandValue( OP_REPORT , 0 ) ;
   if ( result == 2 )
   {
      compl ++ ;
      printf( "\n" ) ;
   }
   else
   {
      WrMessage( "@LOGICAL-EXPR-NOT-COMPLIANT" ) ;
   }

/* Test 4: Evaluating ( a && b ) || c
*/
   WrMessage( "@LOGICAL-EXPR-THREE-BRACKETS" ) ;

   result = OperandValue( OP_RESET , 0 ) ;
   result = ( OperandValue( 1 , 1 ) && OperandValue( 2 , 1 ) )
                                    || OperandValue( 3 , 1 )   ;
   result = OperandValue( OP_REPORT , 0 ) ;
   if ( result == 2 )
   {
      compl ++ ;
      printf( "\n" ) ;
   }
   else
   {
      WrMessage( "@LOGICAL-EXPR-NOT-COMPLIANT" ) ;
   }

   if ( compl == 4 )
   {
      WrMessage( "@LOGICAL-EXPR-COMPLIANT" ) ;
   }

   return ;
}

/* @@------------------------------------------------------------------
    Routine:  WrEvalOrder()
    Author:   Arjen Markus
    Purpose:  Test the order of arithmetic evaluations
    Context:  Used by main()
    Summary:
              This routine checks in which order operands in an
              arithmetic expression are evaluated. It also checks
              whether some simple mixed-type expressions are handled as
              expected.
-------------------------------------------------------------------- */
static void WrEvalOrder( void )
{
   int   result     ;
   int   order      ;
   int   int_val    ;
   float float_val  ;
   int   idx        ;
   int   array_a[2] ;
   int   array_b[2] ;

   static char found_order[][40] =
   {
      "Evaluation from left to right"  ,
      "Evaluation seemingly arbitrary" ,
      "Evaluation from right to left"
   } ;

   WrMessage( "@ARITHMETIC-EVALUATION" ) ;

   order = 0 ;

/* Test 1: Evaluating a + b
*/
   WrMessage( "@ARITHMETIC-TWO-SUM" ) ;

   result =  OperandValue( OP_RESET , 0 ) ;
   result =  OperandValue( 1 , 1 ) + OperandValue( 2 , 1 ) ;
   result =  OperandValue( OP_ORDER , 0 ) ;
   order  += result                       ;

   printf( "Note: %s\n\n" , found_order[result+1] ) ;

/* Test 2: Evaluating a + b + c
*/
   WrMessage( "@ARITHMETIC-THREE-SUM" ) ;

   result =  OperandValue( OP_RESET , 0 )  ;
   result =  OperandValue( 1 , 1 ) + OperandValue( 2 , 1 ) +
             OperandValue( 3 , 1 )         ;
   result =  OperandValue( OP_ORDER  , 0 ) ;
   order  += result                        ;

   printf( "Note: %s\n\n" , found_order[result+1] ) ;

/* Test 3: Evaluating a + b + c + d
*/
   WrMessage( "@ARITHMETIC-FOUR-SUM" ) ;

   result = OperandValue( OP_RESET , 0 ) ;
   result = OperandValue( 1 , 1 ) + OperandValue( 2 , 1 ) +
            OperandValue( 3 , 1 ) + OperandValue( 4 , 1 )   ;
   result = OperandValue( OP_ORDER , 0 ) ;
   order  += result                      ;

   printf( "Note: %s\n\n" , found_order[result+1] ) ;

   if ( order != -3 && order != 3 )
   {
      WrMessage( "@ARITHMETIC-INDECISIVE" ) ;
   }

/* Test 4: Evaluating mixed expression
   Note:
   - A well-informed C checker will complain about this code. It is
     intended this way!
*/
   WrMessage( "@ARITHMETIC-MIXED-TYPE-CALC" ) ;

   int_val   = 10                        ;
   float_val = 1.4                       ;
/* Dummy - try to force refilling the registers
*/
   (void) OperandValue( OP_RESET , 0 )   ;

   result    = 1.7 + int_val + float_val ;
   printf( "Expected result is 13, it is %d\n\n" , result ) ;

/* Test 5: Evaluating a[i++] = b[i]
   Four cases may result:
   a[0] = b[0], a[1] = b[0] , a[0] = b[1] , a[1] = b[1]
*/
   WrMessage( "@ARITHMETIC-SIDE-EFFECTS" ) ;

   idx        = 0                        ;
   array_b[0] = 1                        ;
   array_b[1] = 2                        ;
   array_a[0] = 3                        ;
   array_a[1] = 4                        ;

   array_a[idx++] = array_b[idx]         ;

   if ( array_a[0] == array_b[0] )
   {
      printf( "Statement equivalent to: a[i] = b[i] ; i ++ ;\n\n" ) ;
   }
   if ( array_a[0] == array_b[1] )
   {
      printf( "Statement equivalent to: a[i] = b[i+1] ; i ++ ;\n\n" ) ;
   }
   if ( array_a[1] == array_b[0] )
   {
      printf( "Statement equivalent to: a[i+1] = b[i] ; i ++ ;\n\n" ) ;
   }
   if ( array_a[1] == array_b[1] )
   {
      printf( "Statement equivalent to: i ++ ; a[i] = b[i] ;\n\n" ) ;
   }

   return ;
}

/* @@------------------------------------------------------------------
    Routine:  WrInputFeatures()
    Author:   Arjen Markus
    Purpose:  Test certain features of the input functions
    Context:  Used by main()
    Summary:
              This routine checks the behaviour of:
              - sscanf(): what does it return?
              - sscanf(): what happens if the input string is not
                          formatted as expected?
              - atof():   what happens if the input string is not a
                          number?
-------------------------------------------------------------------- */
static void WrInputFeatures( void )
{
   int   check             ;
   int   result            ;
   int   err_no            ;
   float float_val         ;
   char  input_string[20]  ;
   char  string1[20]       ;
   char  string2[20]       ;

   WrMessage( "@INPUT-FEATURES" ) ;
   check = 0                      ;

/* Test 1: the return value of sscanf() - regular
*/
   WrMessage( "@INPUT-SSCANF-RETURN-REGULAR" )                   ;
   strcpy( input_string , "A B" )                                ;
   result = sscanf( input_string , "%s %s" , string1 , string2 ) ;

   printf( "Result: %d - expected 2\n\n" , result ) ;
   if ( result == 2 )
   {
      check ++ ;
   }

/* Test 2: the return value of sscanf() - on end of string
*/
   WrMessage( "@INPUT-SSCANF-RETURN-EOF" )            ;
   strcpy( input_string , "A" )                       ;
   errno  = 0                                         ;
   result = sscanf( input_string , "%10s %10s" , string1 , string2 ) ;
   err_no = errno                                     ;

   printf( "Result: %d - expected 1 or 'EOF' (%d)\n\n" , result , EOF ) ;
   printf( "System error: %s\n\n" , strerror( err_no ) ) ;
   if ( result == 1 || result == EOF )
   {
      check ++ ;
   }

/* Test 3: the return value of sscanf() - on invalid real
*/
   WrMessage( "@INPUT-SSCANF-RETURN-INVALID" )              ;
   strcpy( input_string , "A" )                             ;
   errno     = 0                                            ;
   float_val = 1.23                                         ;
   result    = sscanf( input_string , "%f" , &float_val )   ;
   err_no    = errno                                        ;

   printf( "New value: %g - original: 1.23\n" , float_val ) ;
   printf( "Result: %d - expected 0\n" , result        )    ;
   printf( "System error: %s\n\n" , strerror( err_no ) )    ;
   if ( result == 0 )
   {
      check ++ ;
   }

/* Write the result
*/
   if ( check == 3 )
   {
      WrMessage( "@INPUT-SSCANF-COMPLIANT" ) ;
   }
   else
   {
      WrMessage( "@INPUT-SSCANF-NON-COMPLIANT" ) ;
   }

/* Test 4: the return value of atof() - on invalid real
*/
   WrMessage( "@INPUT-ATOF-INVALID-INPUT" )              ;
   strcpy( input_string , "A" )                          ;
   errno     = 0                                         ;
   float_val = (float) atof( input_string )              ;
   err_no    = errno                                     ;

   printf( "Float value: %f\n" , float_val             ) ;
   printf( "System error: %s\n\n" , strerror( err_no ) ) ;

   if ( err_no == 0 )
   {
      WrMessage( "@INPUT-NO-ERROR" ) ;
   }

   return ;
}

/* @@------------------------------------------------------------------
    Routine:  SetNumerics()
    Author:   Arjen Markus
    Purpose:  Set the arguments to specific values
    Context:  Used by WrNumericErrors()
    Summary:
              This routine simply sets the arguments
-------------------------------------------------------------------- */
static void SetNumerics(
   float *large_val    ,
   float *small_val    ,
   float *zero_val     ,
   float *negative_val )
{
   *large_val    =  1.0E20F  ;
   *small_val    =  1.0E-20F ;
   *zero_val     =  0.0F     ;
   *negative_val = -1.0F     ;

   return ;
}

/* @@------------------------------------------------------------------
    Routine:  WrNumericErrors()
    Author:   Arjen Markus
    Purpose:  Test deliberate numeric errors (overflow etc.)
    Context:  Used by main()
    Summary:
              This routine checks what happens if:
              - underflow occurs
              - overflow occurs
              - division by zero occurs
              - sqrt() gets a wrong argument
              The program may abort on these, so they are selected
              via the file "chksysc.set"
-------------------------------------------------------------------- */
static void WrNumericErrors( void )
{
   int   check             ;
   int   err_no            ;
   float large_val         ;
   float small_val         ;
   float zero_val          ;
   float negative_val      ;
   float result            ;

/* Use an auxiliary routine from preventing smart compilers to
   do the calculations themselves
*/
   SetNumerics( &large_val , &small_val , &zero_val , &negative_val ) ;

   WrMessage( "@NUMERIC-ERRORS" )  ;

/* Now the calculations:
   Test 1: Overflow
*/
   check = CheckSet( "@OVERFLOW" ) ;

   if ( check )
   {
      WrMessage( "@NUMERIC-OVERFLOW" )  ;
      errno  = 0                        ;
      result = large_val / small_val    ;

      err_no = errno                    ;
      printf( "Result: %g\n" , result ) ;
      printf( "System error: %s\n\n" , strerror( err_no ) ) ;

      WrMessage( "@NUMERIC-CONTINUE" )  ;
   }

/* Test 2: Underflow
*/
   check = CheckSet( "@UNDERFLOW" ) ;

   if ( check )
   {
      WrMessage( "@NUMERIC-UNDERFLOW" ) ;
      errno  = 0                        ;
      result = small_val / large_val    ;

      err_no = errno                    ;
      printf( "Result: %g\n" , result ) ;
      printf( "System error: %s\n\n" , strerror( err_no ) ) ;

      WrMessage( "@NUMERIC-CONTINUE" )  ;
   }

/* Test 3: Division by zero
*/
   check = CheckSet( "@DIVISION" ) ;

   if ( check )
   {
      WrMessage( "@NUMERIC-DIVISION" )  ;
      errno  = 0                        ;
      result = 1.0 / zero_val           ;

      err_no = errno                    ;
      printf( "Result: %g\n" , result ) ;
      printf( "System error: %s\n\n" , strerror( err_no ) ) ;


      WrMessage( "@NUMERIC-CONTINUE" )  ;
   }

/* Test 4: Domain error for sqrt()
*/
   check = CheckSet( "@DOMAIN" ) ;

   if ( check )
   {
      WrMessage( "@NUMERIC-DOMAIN" )    ;
      errno  = 0                        ;
      result = sqrt( negative_val )     ;

      err_no = errno                    ;
      printf( "Result: %g\n" , result ) ;
      printf( "System error: %s\n\n" , strerror( err_no ) ) ;

      WrMessage( "@NUMERIC-CONTINUE" )  ;
   }

/* We are done
*/
   return ;
}

/* @@------------------------------------------------------------------
    Routine:  SetStrings()
    Author:   Arjen Markus
    Purpose:  Set the arguments to specific string values
    Context:  Used by WrStringErrors()
    Summary:
              This routine simply sets the arguments
-------------------------------------------------------------------- */
static void SetStrings(
   char  **null_ptr     ,
   char  **const_ptr    )
{
   *null_ptr     =  NULL       ;
   *const_ptr    =  "Constant" ;

   return ;
}

/* @@------------------------------------------------------------------
    Routine:  WrStringErrors()
    Author:   Arjen Markus
    Purpose:  Test deliberate errors manipulating strings
    Context:  Used by main()
    Summary:
              This routine checks what happens if:
              - strcpy() is used with a NULL-pointer
              - strcpy() is used to copy one part of a string into
                an overlapping part
              - we try to alter a string literal
              The program may abort on these, so they are selected
              via the file "chksysc.set"
-------------------------------------------------------------------- */
static void WrStringErrors( void )
{
   int   check      ;
   char  string[20] ;
   char *null_ptr   ;
   char *const_ptr  ;

   SetStrings( &null_ptr , &const_ptr ) ;
   WrMessage( "@STRING-ERRORS" )  ;

/* Test 1: strcpy() with second argument NULL
*/
   check = CheckSet( "@COPYNULL" ) ;

   if ( check )
   {
      WrMessage( "@STRING-COPY-NULL" )  ;
      strcpy( string , null_ptr )       ;

      printf( "Result: %s\n" , string ) ;

      WrMessage( "@STRING-CONTINUE" )   ;
   }

/* Test 2: strcpy() with overlap
*/
   check = CheckSet( "@COPYOVERLAP" ) ;

   if ( check )
   {
      WrMessage( "@STRING-COPY-OVERLAP" )         ;
      strcpy( string , "1234567890" )             ;
      strcpy( string , &string[2]   )             ;

      printf( "Result: %s\n" , string )           ;

      WrMessage( "@STRING-COPY-OVERLAP-SECOND" )  ;
      strcpy( string , "1234567890" )             ;
      strcpy( &string[2] , string   )             ;

      printf( "Result: %s\n" , string )           ;

      WrMessage( "@STRING-CONTINUE" )             ;
   }

/* Test 3: alter a string literal
*/
   check = CheckSet( "@CHANGECONST" ) ;

   if ( check )
   {
      WrMessage( "@STRING-CHANGE-CONSTANT" )  ;
      strcpy( const_ptr , "1"              )  ;

      printf( "Result: %s\n" , const_ptr ) ;

      WrMessage( "@STRING-CONTINUE" )   ;
   }

/* We are done
*/
   return ;
}

/* @@------------------------------------------------------------------
    Routine:  WrMallocErrors()
    Author:   Arjen Markus
    Purpose:  Test deliberate errors with malloc() routines
    Context:  Used by main()
    Summary:
              This routine checks what happens if:
              - free() is used with a NULL-pointer
              - free() is used on a local automatic array
              The program may abort on these, so they are selected
              via the file "chksysc.set"
-------------------------------------------------------------------- */
static void WrMallocErrors( void )
{
   int   err_no     ;
   int   check      ;
   char  string[20] ;
   char *null_ptr   ;
   char *local_ptr  ;

   SetStrings( &null_ptr , &local_ptr ) ;
   local_ptr = &string[0]               ;

   WrMessage( "@MALLOC-ERRORS" )  ;

/* Test 1: free() with first argument NULL
*/
   check = CheckSet( "@FREENULL" ) ;

   if ( check )
   {
      WrMessage( "@MALLOC-FREE-NULL" )  ;
      errno = 0                         ;
      free( null_ptr )                  ;

      err_no = errno                    ;
      printf( "System error: %s\n\n" , strerror( err_no ) ) ;
      WrMessage( "@MALLOC-CONTINUE" )   ;
   }

/* Test 2: free() with first argument pointing to locla array
*/
   check = CheckSet( "@FREELOCAL" ) ;

   if ( check )
   {
      WrMessage( "@MALLOC-FREE-LOCAL" ) ;
      errno = 0                         ;
      free( local_ptr )                 ;

      err_no = errno                    ;
      printf( "System error: %s\n\n" , strerror( err_no ) ) ;
      WrMessage( "@MALLOC-CONTINUE" )   ;
   }

/* We are done
*/
   return ;
}

/* Epilogue for C++ --- omitted
#if defined( __cplusplus )
}
#endif
---------------------------- */
