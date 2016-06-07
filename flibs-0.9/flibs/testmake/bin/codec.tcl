# DOC
#
# codec.tcl - Code fragments to be inserted into the output
#
# The test program will be written as fragments of C code.
# These fragments are defined here. Individual modules may however
# want to overwrite the defaults given in this script.
#
# Note:
# These fragments may contain references to variables, such as
# the names of the dummy parameters and their types.
#
# Note 2:
# This is a very preliminary implementation for C. As C does not
# allow function overloading, only the simplest cases are treated.
#
# The following variables are predefined:
# module    - the name of the module under test
# title     - title of the testcase
# varname   - the name of the dummy parameter
# vartype   - the type of the dummy parameter (in the host language)
# varcopy   - variable that is a copy of the dummy parameter (for testing)
# initcode  - the code that initialises the parameter
# checkcode - the code that checks the parameter
#
# Required fragments:
#
# Fragment "head" {
#   <Define the header for the program>
# }
#
# Fragment "headers" {
#   <Define a place for additional headers/use statements etc.>
# }
#
# Fragment "initdecl" {
#   <Define all standard declarations>
# }
#
# Fragment "declaration" {
#   <Define a declaration for an individual parameter>
# }
#
# Fragment "dummy_declaration" {
#   <Introduce comments for an individual parameter, defined elsewhere>
# }
#
# Fragment "init_preparations" {
#   <Code to prepare the test facilities>
# }
#
# Fragment "preparations" {
#   <Code to be called once, at the very start of the program>
# }
#
# Fragment "run_test" {
#   <Code to run the initialisation, the module and the check>
# }
#
# Fragment "copy_variable" {
#   <Code to copy the actual parameter into the checking variable>
# }
#
# Fragment "call_initialise" {
#   <Call the initialisation routine and print the testcase's title>
# }
#
# Fragment "initialise" {
#   <Subroutine header that initialises the variables before each test case>
# }
#
# Fragment "user_initialisation" {
#   <Fragment after ordinary initialisation>
# }
#
# Fragment "end_initialise" {
#   <Subroutine footer for "initialise">
# }
#
# Fragment "run_module" {
#   <Subroutine header that calls the actual module>
# }
#
# Fragment "call_module" {
#   <Code to actually call the module>
# }
#
# Fragment "end_run_module" {
#   <Subroutine footer for "run_module">
# }
#
# Fragment "check_result" {
#   <Subroutine header that checks the results>
# }
#
# Fragment "should_be_equal" {
#   <Code that checks that the actual parameter has not changed>
# }
#
# Fragment "should_differ" {
#   <Code that checks that the actual parameter has changed>
# }
#
# Fragment "error_if_differs" {
#   <Code that reports an error if a difference is encountered>
# }
#
# Fragment "end_check_result" {
#   <Subroutine footer for "check_results">
# }
#
# Fragment "epilogue" {
#   <Define cleaning up for a test (after all checks)>
# }
#
# Fragment "summary" {
#   <Subroutine that summarises the results>
# }
#

Fragment "head" {
#include <stdlib.h>
#include <stdio.h>
#include "test_fac.h"
}

Fragment "headers" {
/*
 * User-specific header files come here, as well as definitions
 * and so on
 */
}

Fragment "initdecl" {
/* Start of declarations */
}

Fragment "user_declarations" {
/* No extra declarations */
}

#
# TODO: also strings, arrays, pointers
# For C, we need to examine the variable type!
# Note: we do not (yet) support a declaration like "int *v[10]"
#
proc C_declaration { vartype varname } {
   #
   # Declarations like "int *v ;"
   #
   if { [string first "*" $vartype] != -1 } {
      return "$vartype $varname = NULL ;"
   }
   #
   # Declarations like "int v[10] ;"
   #
   set insert [string first "\[" $vartype]
   if { $insert != -1 } {
      set insertb    [expr $insert-1]
      set first_part [string range $vartype 0 $insertb]
      set last_part  [string range $vartype $insert end]
      return "$first_part $varname $last_part ;"
   }
   #
   # "Ordinary" declarations
   #
   return "$vartype $varname ;"
}

Fragment "declaration" {
   [C_declaration $vartype $varname]
}

Fragment "dummy_declaration" {
/* Externally defined: $varname */
}

Fragment "init_preparations" {
/* TestMake variables */
static FILE *test__logf    ;
static int   test__success ;
static int   test__input   ;
static int   test__output  ;
static int   test__error   ;
static int   test__failed  ;

void test_print_text( char *text )
{
   fprintf( test__logf, "%s\\n", text ) ;
}

void test_failed( int cond, char *descr )
{
   if ( cond )
   {
      test__failed = 1 ;
      fprintf( test__logf, "%s\\n", descr ) ;
   }
}

/* Main program */
int main( int argc, char *argv\[\] )
\{
   test__success = 0 ;
   test__logf = fopen( "test_$module.log", "w" ) ;
   if ( test__logf == NULL )
   {
      fprintf( stderr, "Could not open log file!\\n" ) ;
      exit( 1 ) ;
   }
   else
   {
      fprintf( test__logf, "Test program for module: $module\\n" ) ;
   }
}

Fragment "preparations" {
/*
 * Nothing special - no preparations necessary
 */
}

Fragment "call_initialise" {
   fprintf( test__logf, "\\n$title\\n" ) ;
   initialise() ;
}

Fragment "run_test" {
   run_module() ;
   check_result() ;
}

Fragment "end_test_suite" {
   summary() ;
   exit( 0 ) ;

\}
}

Fragment "initialise" {
void initialise( void )
\{
}

#
# Works for all types, except when pointers are involved
# TODO: other variables than scalar
#
Fragment "copy_variable" {
   test__copy( (void *) &($varname), (void *) &($varcopy),
               "$vartype", sizeof($varname) ) ;
}

Fragment "user_initialisation" {
/* No extra initialisation */
}

Fragment "end_initialise" {
return ;
\}
}

Fragment "run_module" {
void run_module( void )
\{
   test__failed = 0 ;
}

Fragment "call_module" {
   ===> Error: call to module not defined!
}

Fragment "end_run_module" {
   return ;
\}
}

Fragment "check_result" {
void check_result( void )
\{
   test__input  = 1 ;
   test__output = 1 ;
   test__error  = 0 ;
}

#
# TODO: Compensate for lack of overloading mechanism!
#
Fragment "should_be_equal" {
   if ( ! test__equal( (void *)(&$varname), (void *)(&$varcopy),
                       "$vartype", sizeof($varname) ) )
   {
      fprintf( test__logf, "Input parameter $varname has changed!\\n" ) ;
      test__input = 0 ;
   }
}

Fragment "should_differ" {
   if ( test__equal( (void *)(&$varname), (void *)(&$varcopy),
                       "$vartype", sizeof($varname) ) )
   {
      fprintf( test__logf, "Output parameter $varname has NOT changed!\\n" ) ;
      test__output = 0 ;
   }
}

Fragment "error_if_differs" {
   if ( !test__equal( (void *)(&$varname), (void *)(&$varcopy),
                       "$vartype", sizeof($varname) ) )
   {
      fprintf( test__logf, "Error parameter $varname has been set\\n" ) ;
      test__error = 1 ;
   }
}

Fragment "end_check_result" {
/* The test failed, if:
   - any of the input parameters has changed
   - any of the output parameters has NOT changed, even though
     there was no error
*/
   if ( !test__input )
   {
      test__failed = 1 ;
   }
   if ( ! test__output && ! test__error )
   {
      test__failed = 1 ;
   }
   if ( test__failed )
   {
      fprintf( test__logf, "The current test has failed!\\n" ) ;
   }
   else
   {
      test__success ++ ;
   }

   return ;
\}
}

Fragment "epilogue" {
/*
 * No cleaning up required
 */
}

Fragment "summary" {

void print_stats( int path_count, int path_success, int path_io,
                  int no_success )
{
fprintf( test__logf,"\\n\\n\\
--------------------------------------------------------\\n\\
Summary:\\n\\
-------\\n" ) ;

fprintf( test__logf, "Total number of test paths:        %d\\n", path_count   ) ;
fprintf( test__logf, "Successfully constructed:          %d\\n", path_success ) ;
fprintf( test__logf, "Number of paths controlled by I/O: %d\\n", path_io      ) ;
fprintf( test__logf, "Number of successful tests:        %d\\n", no_success   ) ;

if ( path_count > 0 )
{
   if ( path_success <= path_count / 2 )
   {
      fprintf( test__logf, "Only a small percentage accounted for: %d%c\\n",
            (int) (( 100.0 * path_success ) / path_count ), '%' ) ;
   }
   if ( path_io      >  path_count / 2 )
   {
      fprintf( test__logf, "Most paths involve I/O: %d%c\\n",
         (int) (( 100.0 * path_success ) / path_count ), '%' ) ;
   }
}
else
{
   fprintf( test__logf, "No automatic test paths defined\\n" ) ;
}

fprintf( test__logf, "\\nNumber of successful tests:        %d\\n",
   no_success ) ;
return ;
}

void summary( void )
{
   print_stats( $pathcount, $path_success, $path_io, test__success ) ;
   return ;
}
}
