# DOC
#
# codejava.tcl - Code fragments to be inserted into the output
#
# The test program will be written as fragments of java code.
# These fragments are defined here. Individual modules may however
# want to overwrite the defaults given in this script.
#
# Note:
# These fragments may contain references to variables, such as
# the names of the dummy parameters and their types.
#
# Note 2:
# This is a very preliminary implementation for Java.
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
import java.io.* ;

}

Fragment "headers" {
/*
 * User-specific header files come here, as well as definitions
 * and so on
 */
}

Fragment "initdecl" {
/* Start of declarations */

public class test_$module \{
/* TestMake variables */
static PrintWriter test__logf    ;
static int         test__success ;
static boolean     test__input   ;
static boolean     test__output  ;
static boolean     test__error   ;
static boolean     test__failed  ;

}

Fragment "user_declarations" {
/* No extra declarations */
}

#
# TODO: also strings, arrays, pointers
# TODO: can we avoid static in an easy way?
#
Fragment "declaration" {
   static $vartype $varname ;
}

Fragment "dummy_declaration" {
/* Externally defined: $varname */
}

Fragment "init_preparations" {
/* Main program */
public static void main( String argv\[\] )
\{
   test__success = 0 ;
   try {
      test__logf = new PrintWriter( new FileWriter( "test_$module.log" ) ) ;
   }
   catch ( IOException e )
   {
      System.err.println( "Could not open log file!" ) ;
      System.exit( 1 ) ;
   }

   test__logf.println( "Test program for module: $module" ) ;
}

Fragment "preparations" {
/*
 * Nothing special - no preparations necessary
 */
}

Fragment "call_initialise" {
   test__logf.println( "\\n$title" ) ;
   initialise() ;
}

Fragment "run_test" {
   run_module() ;
   check_result() ;
}

Fragment "end_test_suite" {
   summary() ;
   test__logf.close() ;
   System.exit( 0 ) ;

\}
}

Fragment "initialise" {
static void initialise( )
\{
}

#
# Works for all types, except when pointers are involved
# TODO: other variables than scalar
#
Fragment "copy_variable" {
   $varcopy = Testcopy.Copy($varname) ;
}

Fragment "user_initialisation" {
/* No extra initialisation */
}

Fragment "end_initialise" {
return ;
\}
}

Fragment "run_module" {
static void run_module( )
\{
}

Fragment "call_module" {
   ===> Error: call to module not defined!
}

Fragment "end_run_module" {
   return ;
\}
}

Fragment "check_result" {
static void check_result( )
\{
   test__input  = true  ;
   test__output = true  ;
   test__error  = false ;
   test__failed = false ;
}

#
# TODO: Compensate for lack of overloading mechanism!
#
Fragment "should_be_equal" {
   if ( ! Testcmp.Equal($varname, $varcopy) )
   {
      test__logf.println( "Input parameter $varname has changed!" ) ;
      test__input = false ;
   }
}

Fragment "should_differ" {
   if ( Testcmp.Equal($varname, $varcopy) )
   {
      test__logf.println( "Output parameter $varname has NOT changed!" ) ;
      test__output = false ;
   }
}

Fragment "error_if_differs" {
   if ( !Testcmp.Equal($varname,$varcopy) )
   {
      test__logf.println( "Error parameter $varname has been set" ) ;
      test__error = true ;
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
      test__failed = true ;
   }
   if ( ! test__output && ! test__error )
   {
      test__failed = true ;
   }
   if ( test__failed )
   {
      test__logf.println( "The current test has failed!" ) ;
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

static void print_stats( int path_count, int path_success, int path_io,
                  int no_success )
{
test__logf.println("\\n" +
"--------------------------------------------------------\\n" +
"Summary:\\n" +
"-------" ) ;

test__logf.println( "Total number of test paths:        " + path_count ) ;
test__logf.println( "Successfully constructed:          " + path_success ) ;
test__logf.println( "Number of paths controlled by I/O: " + path_io      ) ;
test__logf.println( "Number of successful tests:        " + no_success   ) ;

if ( path_count > 0 )
{
   if ( path_success <= path_count / 2 )
   {
      test__logf.println( "Only a small percentage accounted for: " +
            (int) (( 100.0 * path_success ) / path_count ) + "%" ) ;
   }
   if ( path_io      >  path_count / 2 )
   {
      test__logf.println( "Most paths involve I/O: " +
         (int) (( 100.0 * path_success ) / path_count ) + "%" ) ;
   }
}
else
{
   test__logf.println( "No automatic test paths defined" ) ;
}

test__logf.println( "\\nNumber of successful tests:        " + no_success ) ;
return ;
}

static void summary( )
{
   print_stats( $pathcount, $path_success, $path_io, test__success ) ;
   return ;
}
/* End of class text */
\}
}
