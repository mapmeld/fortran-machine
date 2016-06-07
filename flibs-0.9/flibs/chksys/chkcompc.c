/* @begin@
 *
 *  chkcompc.c - source code to check certain fetaures of the C compiler
 *
 *  Copyright (C) 1998 Arjen Markus
 *
 *  Arjen Markus
 *
 *
 *  General information:
 *  This file contains source code that uses various extensions to the
 *  C standard and intentionally bad programming fragments.
 *  Comments explain each of them.
 *  Its purpose is to check if the compiler (in combination with the
 *  options) will flag the features.
 *
 *  Note:
 *  The code uses the following macros to control the checks:
 *  USE_PROTO  - Use explicit prototypes as defined by the Standard
 *  NO_PROTO   - Do not use explicit prototypes, but the K&R style
 *  NOEXT      - Exclude extensions to the Standard (some compilers
 *               or checkers like lint will complain about these
 *               features and not continue with error checking)
 *
 *  If you define NO_PROTO, no prototypes will be defined, otherwise
 *  the presence of __STDC__ or _cplusplus will define USE_PROTO
 *  If you know the compiler supports prototypes, but does not define
 *  __STDC__ for some reason (probably because extensions to the
 *  standard are allowed), you can define it yourself.
 *
 *  Only if you define EXTENSIONS, the code containing certain
 *  extensions will be compiled.
 *
 * @end@
 */

/*  $Author: arjenmarkus $
 *  $Date: 2008/03/17 17:57:56 $
 *  $Source: /cvsroot/flibs/chksys/chkcompc.c,v $
 *  $Log: chkcompc.c,v $
 *  Revision 1.1  2008/03/17 17:57:56  arjenmarkus
 *  Added directory "chksys" - programs to probe compiler properties
 *
 */

/* Include files
*/
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

/* Use of prototypes:
   - ANSI-compliant compilers define the macro __STDC__ to be 1
   - If the compiler supports prototypes, but does not claim to
     be ANSI-compliant, you can define the macro USE_PROTO
   - If the compiler is a C++ compiler, it defines _cplusplus and
     requires (!) prototypes.
   Note:
   In this file hardly any use is made of subroutines. So this feature
   is not very important.
*/
#if ! defined( NO_PROTO )
#if defined( __STDC__ ) || defined( _cplusplus )
#define USE_PROTO
#endif
#endif

#if ! defined( NOEXT )
#define EXTENSIONS
#endif

/* Typical prologue for C++
*/
#if defined( _cplusplus )
extern "C" {
#endif

/* Prototypes in classical K&R style: Are they flagged?
   Note:
   The meaning of an empty argument list has changed from K&R
   to Standard!
*/
int SomeFunction( ) ;
int OtherFunction( int one_arg ) ;
int ReturnMixed(   int value   ) ;

int *ReturnLocal( void ) ;
void ReturnNothing( void ) ;
void ArithmPointers( void ) ;

/* Variable that is redefined with a different scope:
   Is this noticed?
*/
int scope_var ;

/* @@------------------------------------------------------------------
    Routine:  main
    Author:   Arjen Markus
    Purpose:  Main program
    Context:  -
    Summary:
              Show the properties of the compiler by expressly
              introducing errors and extensions
-------------------------------------------------------------------- */
#if defined( USE_PROTO )
int main( int argc , char *argv[] )
#else
int main( argc , argv )
       int  argc    ;
       char *argv[] ;
#endif
{

/* Declarations:
   - Is the initialisation of automatic variables allowed?
   - Is the use of extended constant expressions allowed?
   - Will it complain about C++ keywords?
*/
   int          no_init                 ;
   int          class                   ; /* A C++ keyword */
   int          auto_var = 1            ; /* Not accepted by K&R */

#if defined( EXTENSIONS )
   float        float_var = sqrt( 2.0 ) ; /* Not accepted by Standard C */
   static float stat_var  = sqrt( 2.0 ) ; /* Not quite to the standard */
#else
   float        float_var ;
#endif

/* Do the types "long double" and "long long" exist?
*/
#if defined( EXTENSIONS )
   long double ld_var ;
   long long   ll_var ;
#endif

/* Local variables
*/
   int   *pint              ;
   char  *pchar             ;
   int   i    , x    , y    ;
   long  lng                ;
   short shrt               ;

/* Redefine the global variable (with a different type as well)
*/
   float scope_var ;

/* To make it worse: make a very local definition
*/
   if ( argc > 0 )
   {
      char scope_var ;
      printf( "%d\n" , argc ) ;
   }

/* Does the compiler warn about implicit castings?
   What about constants that are too large for the type?
   Does it warn about invisible prototypes?
   Does it complain about pointers being cast to longs?
   What does it have to say about the wrong argument list?
*/
   lng   = ChkValue() ;
   shrt  = lng        ;
   shrt  = 123456789  ;

   pchar = malloc( 20 ) ;
   lng   = pchar        ;

   i     = OtherFunction( shrt , lng ) ;

/* Using a pointer in a condition is quite to the standard, but it
   probably should be flagged.
   Using a float is not acceptable.
*/
   float_var = 1.0 ;

   if ( pchar )
   {
      printf( "pchar not NULL\n" ) ;
   }
   if ( float_var )
   {
      printf( "float_var not 0.0\n" ) ;
   }

/* Very awkward constructions:
   - NULL statements
   - if-statements with assignments making the condition superfluous
   - for-loop with incomplete conditions
   - twisted switch-statement
*/
   x == 0 ; /* NULL statement */

   if ( x = 0 ) /* Always false! */
   {
      y = 1 ;
   }
   else
   {
      printf( "Error: x not equal to zero!\n" ) ;
   }

/* Quite acceptable, but dangerous
*/
   i = 0 ;
   for ( ; ; i ++ )
   {
      printf( "Step: %d\n" , i ) ;
      if ( i > 10 )
      {
         break ;
      }
   }

/* Really twisted switch-statement, the meaning is totally unclear,
   but it could easily happen by misplacing a brace (}).
*/
   switch ( x )
   {
      case 0 :
         if ( y > 0 )
         {
            case 1 :
               printf( "Case 1\n" ) ;
               break ;
            case 2:
               printf( "Case 1\n" ) ;
               break ;
         }
         break ;

      default :
         break ;
   }

/* These statement uses side-effects. Do we get a warning?
   (One is a horrible little piece of code - 5 consecutive plusses)
*/
   i = x++   +  ++y  ;
   i = (x++) + (++y) ;
   i = x   + + + y   ;

/* This statement uses an uninitialised variable
   - Do we get a warning?
*/
   if ( no_init > 0 ) printf( "No initialisation of this variable\n" ) ;

/* These statements use a wrong format string or a wrong number of
   arguments for printing. Is this checked?
*/
   printf( "Float: %d\n" , float_var ) ;
   printf( "Invalid format: %Z\n" , float_var ) ;
   printf( "Invalid number of arguments: %f %f\n" ) ;

/* We get a pointer to an array that existed somewhere on the stack
*/
   pint = ReturnLocal() ;

/* Some pointer arithmetic
*/
   ArithmPointers() ;

/* Use the return value of a function that does not return anything
*/
   lng = ReturnNothing() ;

/* Use the return value of a function that does not always return
   something
*/
   lng = ReturnMixed( 0 ) ;

/* Return statements that should return an integer value but do not
*/
   if ( x > 0 )
   {
      return float_var ; /* A float value */
   }

/* In any case:
*/
   return ;

/* These statements are not reachable:
*/
   x = 2 * y ;
   return x  ;
}

/* Some C compilers will support the C++ style of comments:
*/
#if defined( EXTENSIONS )
// Is this accepted?
#endif

/* @@------------------------------------------------------------------
    Routine:  ReturnLocal
    Author:   Arjen Markus
    Purpose:  Is the returning of pointers to local (!) flagged?
    Context:  Used by main()
    Summary:
              Define a local array and return a pointer to it.
              Errors of this type frequently made and give rise to
              problems that are very difficult to track.
-------------------------------------------------------------------- */
#ifdef USE_PROTO
int *ReturnLocal( void )
#else
int *ReturnLocal( )
#endif
{
   int array[10] ;

   return array ;
}

/* @@------------------------------------------------------------------
    Routine:  ReturnMixed
    Author:   Arjen Markus
    Purpose:  Does the compiler recognise the implicit return statement?
    Context:  Used by main()
    Summary:
              If the argument is 1, return 1, otherwise drop out of
              the subroutine - the implicit return has NO return value.
              This ought to be caught!
-------------------------------------------------------------------- */
#ifdef USE_PROTO
int ReturnMixed( int value )
#else
int ReturnLocal( value )
    int value ;
#endif
{
   if ( value == 1 )
   {
      return 1 ;
   }
   /* No explicit return statement, hence no return value! */
}

/* @@------------------------------------------------------------------
    Routine:  ArithmPointers
    Author:   Arjen Markus
    Purpose:  What are the limitations for calculating with pointers?
    Context:  Used by main()
    Summary:
              Calculate the difference between two pointers of
              different types.
              Cast a (void *) pointer to a (double *) pointer
              Cast a (char *) pointer to a (double *) pointer
              These usually give an alignment problem!
-------------------------------------------------------------------- */
#ifdef USE_PROTO
void ArithmPointers( void )
#else
void ArithmPointers( )
#endif
{
   long   diff        ;
   char   *char_ptr   ;
   void   *void_ptr   ;
   double *double_ptr ;

   char_ptr   = "Whoa, a constant string" ;
   void_ptr   = (void *) char_ptr         ;

   double_ptr = (double *) malloc( sizeof( double ) ) ;

/* Calculate the difference between pointers of different types
   - This is explicitly undefined!
   Also cast some pointer to pointers of a different type
   - we should get warnings about alignment problems
*/
   diff       = double_ptr - void_ptr   ;
   double_ptr = void_ptr                ;
   double_ptr = char_ptr                ;
   double_ptr = (double *) void_ptr + 1 ;

   return ;
}

/* Epilogue for C++
*/
#if defined( _cplusplus )
}
#endif
