# DOC
#
# codef90.tcl - Code fragments to be inserted into the output
#
# The test program will be written as fragments of Fortran 90 code.
# These fragments are defined here. Individual modules may however
# want to overwrite the defaults given in this script.
#
# Note:
# These fragments may contain references to variables, such as
# the names of the dummy parameters and their types.
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
program test_$module
   use test_facilities
}

Fragment "headers" {
   !
   ! "Use" statements come here
   !
}

Fragment "initdecl" {
   !
   ! Force explicit declarations
   !
   implicit none
}

Fragment "user_declarations" {
   ! No extra declarations
}

Fragment "declaration" {
   $vartype :: $varname
}

Fragment "dummy_declaration" {
   ! Externally defined: $varname
}

Fragment "init_preparations" {
   test__success = 0
   call test_print_open( 'test_$module.log', '$module' )
}

Fragment "preparations" {
   !
   ! Nothing special - no preparations necessary
   !
}

Fragment "call_initialise" {
   call test_print_text( ' ' )
   call test_print_text( '$title' )
   call initialise
}

Fragment "run_test" {
   call run_module
   call check_result
}

Fragment "end_test_suite" {
   call summary
   stop
contains
}

Fragment "initialise" {
subroutine initialise
}

#
# Works for all types, except when pointers are involved
#
Fragment "copy_variable" {
   $varcopy = $varname
}

Fragment "user_initialisation" {
   ! No extra initialisation
}

Fragment "end_initialise" {
end subroutine initialise
}

Fragment "run_module" {
subroutine run_module
}

Fragment "call_module" {
   ===> Error: call to module not defined!
}

Fragment "end_run_module" {
   return
end subroutine run_module
}

Fragment "check_result" {
subroutine check_result
   test__input  = .true.
   test__output = .true.
   test__error  = .false.
   test__failed = .false.
}

#
# Trust Fortran 90's overloading mechanism!
#
Fragment "should_be_equal" {
   if ( .not. test_equals( $varname, $varcopy ) ) then
      write( test__lun, * ) "Input parameter $varname has changed!"
      test__input = .false.
   endif
}

Fragment "should_differ" {
   if ( test_equals( $varname, $varcopy ) ) then
      write( test__lun, * ) "Output parameter $varname has NOT changed!"
      test__output = .false.
   endif
}

Fragment "error_if_differs" {
   if ( .not. test_equals( $varname, $varcopy ) ) then
      write( test__lun, * ) "Error parameter $varname has been set"
      test__error = .true.
   endif
}

Fragment "end_check_result" {
   !
   ! The test failed, if:
   ! - any of the input parameters has changed
   ! - any of the output parameters has NOT changed, even though
   !   there was no error
   if ( .not. test__input ) then
      test__failed = .true.
   endif
   if ( .not. test__output .and. .not. test__error ) then
      test__failed = .true.
   endif
   if ( test__failed ) then
      write( test__lun, * ) "The current test has failed!"
   else
      test__success = test__success + 1
   endif

   return
end subroutine check_result
}

Fragment "epilogue" {
   !
   ! No cleaning up required
   !
}

Fragment "summary" {
subroutine summary
   call print_stats( $pathcount, $path_success, $path_io, test__success )
   return
end subroutine summary

subroutine print_stats( path_count, path_success, path_io, no_success )
   integer, intent(in)   :: path_count
   integer, intent(in)   :: path_success
   integer, intent(in)   :: path_io
   integer, intent(in)   :: no_success

   write(test__lun,*) ' '
   write(test__lun,*) &
      '--------------------------------------------------------'
   write(test__lun,*) 'Summary:'
   write(test__lun,*) '-------'
   write(test__lun,*) 'Total number of test paths:        ', path_count
   write(test__lun,*) 'Successfully constructed:          ', path_success
   write(test__lun,*) 'Number of paths controlled by I/O: ', path_io
   write(test__lun,*) 'Number of successful tests:        ', no_success
   if ( path_count .gt. 0 ) then
      if ( path_success .le. path_count / 2 ) then
         write(test__lun,*) 'Only a small percentage accounted for: ', &
            int(( 100.0 * path_success ) / path_count ), '%'
      endif
      if ( path_io      .gt. path_count / 2 ) then
         write(test__lun,*) 'Most paths involve I/O: ', &
            int(( 100.0 * path_success ) / path_count ), '%'
      endif
   else
      write(test__lun,*) 'No automatic test paths defined'
   endif

   write(test__lun,*) ' '
   write(test__lun,*) 'Number of successful tests:        ', no_success
end subroutine print_stats

end program test_$module
}
