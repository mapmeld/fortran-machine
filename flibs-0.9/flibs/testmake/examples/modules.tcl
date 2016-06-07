# DOC
#
# modules.tcl - Description of the various modules to be tested
#
# Each module should be described by the following items:
# - The calling sequence for the module, basically the call to be
#   made. Any preparations etc. are taken care of automatically.
# - The arguments:
#   - For pure input variables: the type is required and input values
#     (for testing purposes: the values after the test are compared with
#     the input values)
#   - For pure output variables: the type is required and output values
#     are reported
#   - For input/output variables: the type is required, as well as input
#     values
#   - Error flags are a special type of output variable. Their output
#     should be interpreted.
# - Possible return type (for functions) must be given
# - The standard initialisation sequence consists of setting all the
#   variables to their standard values. This is automatically generated.
# - The standard output reporting is taken care based on the available
#   information.
# - For each module a series of conditions will have to be satisfied for
#   the module execution to take a certain path through the code. These
#   conditions have to be defined (both the values that render them
#   false as values that render them true).
# - Any special tests that must be done in addition to the ones
#   generated from the analysis by the McCabe toolset.
#
# Overview:
# Module <name> {
#
#    Declaration {
#       <code for additional declarations, such as parameters or auxiliary variables>
#
#
#    Input <variable> <type> {
#       <code to set the variable>
#    }
#    Output <variable> <type> {
#       <code to set the initial values for the variable - needed for checking >
#    }
#    In/out <variable> <type> {
#       <code to set the initial values for the variable>
#    }
#    Error <variable> <type> {
#       <code to check the error conditions>
#    }
#
#    Call {
#       <code to call the module>
#    }
#
#    Testcase "title" {
#       <code to set the input for the test case>
#    }
#
#    Condition "expression" TRUE {
#       <code that makes the expression true>
#    }
#
#    Condition "expression" FALSE {
#       <code that makes the expression true>
#    }
# }
#
# Notes:
# - A pass through the analysis will generate a template for the
#   particular module.
# - A more sophisticated approach than the default reporting is
#   being thought of
# - The type must be a proper Fortran 90 declaration
#
# ENDDOC
#
# Example for "getcco"
#
Module "getcco" {
#Module ODS_DELWAQ_UNF_CCO

   Output "DATA" "REAL*4, DIMENSION(1:10000)" {
      DATA = 1.0
   } { ! Just an extra check for "DATA" }
   Input "FILNAM" "CHARACTER(LEN=256), DIMENSION(1:3)" {
      FILNAM(1) = 'test.lga'
      FILNAM(2) = 'test.cco'
      FILNAM(3) = ' '
   }
   Error "IERROR" "INTEGER*4" {
      IERROR = 0
   }
   Output "INDLOC" "INTEGER*4" {
      INDLOC = 0
   }
   Input "IPCODE" "INTEGER*4" {
      IPCODE = 1
   }
   Input "ITYPE" "INTEGER*4" {
      ITYPE = 1
   }
   Input "MAXDIM" "INTEGER*4" {
      MAXDIM = 10000
   }
   Input "TIME" "REAL*8" {
      TIME = 1.0D00
   }
   Input "VALMIS" "REAL*4" {
      VALMIS = -999.0
   }
   Condition " NODATA .GT. MAXDIM  " TRUE {
      MAXDIM = 1
   }
   Condition " NODATA .GT. MAXDIM  " FALSE {
      MAXDIM = 10000
   }
   Condition " IPCODE .EQ. 2  " TRUE {
      IPCODE = 2
   } { call test_failed( .not. (ipcode .eq. 2), "IPCODE not equal 2" ) }
   Condition " IPCODE .EQ. 2  " FALSE {
      IPCODE = 1
   }
   Testcase "Extra test" {
      ! Just a test
   } { ! Just an extra check }
   Call {
      CALL ODS_DELWAQ_UNF_CCO(                        &
         FILNAM , ITYPE  , IPCODE , TIME   , INDLOC , &
         VALMIS , MAXDIM , DATA   , IERROR            )
   }
}

#
# Test to see if more than one module is accepted
#
Module AA {
   Output "DATA_AA" "REAL*4, DIMENSION(1:10000)" { AA }
   Call {
      call aa
   }
}
Module BB {
   Output "DATA_BB" "REAL*4, DIMENSION(1:10000)" { BB }
   Call {
      call bb
   }
}

#
# Module to test the C implementation
#
Module CC {
   Input "u" "int" { u = 0 ; }

   Output "v" "int" { v = 0 ; }
   Call {
      v = 1 ;
      u = 2 ;
   }
   Testcase "One single test" {
      v = 2 ;
   }
}

#
# Module to test the Java implementation
#
Module JJ {
   Input "u" "int" { u = 0 ; }

   Output "v" "int" { v = 0 ; }

   Input "str" "String" { str = "Aha" ; }
   Input "buff" "StringBuffer" { buff = new StringBuffer("Aha") ; }
   Input "array" "float[]" {
      array = new float[10] ;
      for (int i = 0; i < 10; i ++)
      {
         array[i] = (float) i ;
      }
   }

   Call {
      v = 1 ;
      u = 2 ;
      str = "Oops" ; /* This should have no effect on the copy */
      buff.setCharAt( 0, 'a' ) ; /* Problem: no error detected! */
      array[1] = 0.0f ;        /* Problem: ditto! */
   }
   Testcase "One single test" {
      v = 2 ;
   }
}
