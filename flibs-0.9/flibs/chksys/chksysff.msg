@warning
File size should not exceed 800 lines!
@INTRODUCTION
CHKSYSFF: A program to determine the properties of the run-time
          environment for FORTRAN 90 programs

The program presents information about:
- The numerical properties of the processor/compiler
- What character set is used?
- Manipulations with pointers and allocatable arrays
- Whether array assignments are properly implemented in the compiler
- Issues concerning the random number generation

Note:
The program is accompanied by a more comprehensive program written
in FORTRAN 77. As this language allows more variations in the
precise interpretation of the standard, it does concentrate on
various uncertainties in the behaviour of the run-time environment.
However, even a relatively new language like FORTRAN 90 has non-standard
extensions.

@LIST-KINDS
Integer and real kinds
----------------------
FORTRAN 90 has a number of portable features regarding the specification
of integers and reals of various lengths. To use these features you need
to know the "kinds" - numbers that indicates the kind of data you want.

The program tries to determine which integers and reals are supported by
the current run-time environment.

@CHAR-PROPERTIES
Ordinary characters
-------------------
The character set used by FORTRAN 90 consists of most characters you
find on ordinary QWERTY keyboards and none other. The language provides
standard ways of using different types of character sets besides the
ordinary, but there is no way (like for integers and reals) to find
out which are supported in the current environment. Instead, you will
have to consult the compiler's manual for that.

However, the default character set is supposed to have the following
properties:
- The functions ICHAR() and CHAR() are each others' inverse:
     CHAR(ICHAR(c) = c   for any character
- The functions IACHAR() and ACHAR() that give the ASCII code for the
  characters are likewise related:
     ACHAR(IACHAR(c) = c   for any character

The program checks these relations for the FORTRAN 90 character set.

@RANDOM-NUMBERS
Random number generator
-----------------------
The language standard defines a random number generator that is supposed
to have a uniform distribution. Unfortunately, there are no guarantees
on the quality of the random number generator:
- What is the length of the pseudo-random number sequence?
- Does the generator truly have a uniform distribution?

This program is not capable of answering these and similar questions.
However, it does provide you with information on the quality:
- It draws 10000 samples and determines the distribution over ten bins
  of equal size.
- It displays the number of samples in each bin, the average of the
  samples and the standard deviation.
- It estimates via a simple test, if the distribution is roughly
  uniform.

@RANDOM-NUMBERS-SUCCESS
One or more cases out of range. The random number generator fails the
uniformity test!

@RANDOM-NUMBERS-FAILURE
All classes within range. The random number generator seems uniform enough.

@UNDEFINED-POINTERS
Pointers: uninitialised
-----------------------
Pointers are a special type of data in FORTRAN 90. They can be used and
abused, as pointers in any language. What seems to be a very rare
feature indeed is the ability in FORTRAN 90 to check the status of
the pointers. This would enable you to make robust programs.

There is a slight complication, though, as uninitialised pointers
have an undefined status. This is checked for the present run-time
environment in this part of the program. Other properties and
pecularities are checked in a subsequent test.

@UNDEFINED-POINTERS-STATE
Checking the state of uninitialised pointers:
@UNDEFINED-POINTERS-TRUE
   Undefined status for pointers: may be TRUE!
   - you have to be careful!

@UNDEFINED-POINTERS-FALSE
   Undefined status for pointers: seems systematically FALSE
   - you have to be careful though. This test is NOT rigid

@UNDEFINED-ALLOCATED-STATE
Checking the state of uninitialised allocatable arrays:
@UNDEFINED-ALLOCATED-TRUE
   Undefined status for allocatable arrays: may be TRUE!
   - you have to be careful!

@UNDEFINED-ALLOCATED-FALSE
   Undefined status for allocatable arrays: seems systematically FALSE
   - you have to be careful though. This test is NOT rigid

@UNDEFINED-POINTERS-ASSOC
Checking the association of pointers to uninitialised allocatable arrays:
@UNDEFINED-ASSOC-SUCCESS
   Succeeded in associating a pointer to an uninitialised allocatable array'
   - you have to be careful!

@UNDEFINED-ASSOC-FAILURE
   Failed to associate a pointer to an uninitialised allocatable array'
   - this might be robust!

@ALLOCATED-POINTERS
Manipulating allocated memory
-----------------------------
FORTRAN 90 allows two ways of allocating and deallocating memory:
- Via ALLOCATABLE arrays
- Via POINTER variables

You may combine these methods, but be careful: it may render the
variables useless or you get run-time errors.

Consequently, this test may cause the program to crash.

@ALLOCATED-POINTERS-SKIPPED
Note:
Test is skipped.

@ALLOCATED-POINTERS-STEP1
Step 1:
- Allocate some memory via an allocatable array
- Deallocate it via a pointer to that memory
@ALLOCATED-STEP1-SUCCESS
   Memory freed via pointer - no problem reported
   Note: ALLOCATED() on allocatable array indicates:
@ALLOCATED-STEP1-WARNING
      Memory still allocated according to target

@ALLOCATED-STEP1-NOALLOC
      Memory no longer allocated according to target

@ALLOCATED-STEP1-FAILURE
   Memory could not be freed via pointer

@ALLOCATED-POINTERS-STEP2
Step 2:
- Allocate some memory via an allocatable array
- Let a pointer point to that memory
- Deallocate the memory.
- What is the status of the pointer?

Note:
Because of the first test, the original allocatable array may have
become useless. This second test uses a second allocatable array.

@ALLOCATED-STEP2-STATUS
Memory to which pointer points has been freed. Check the status of
the pointer:
@ALLOCATED-STEP2-REASSOCIATE
Trying to reassociate the pointer:
@ALLOCATED-POINTERS-STEP3
Step 3:
- Try to free the allocated memory twice

@ALLOCATED-STEP3-SUCCESS
   Memory freed twice - no problem reported

@ALLOCATED-STEP3-FAILURE
   Memory could not be freed again

@ARRAY-ASSIGNMENT
Array assignments
-----------------
One of the nice features of FORTRAN 90 is that it allows array
manipulations as statements. For instance:
   REAL , DIMENSION(1:10) :: A
   REAL , DIMENSION(1:10) :: B

   A = B

means that each element of B is copied into the corresponding element
of A.

This is just a simple example. You can also shift the elements of the
same array:

   A(2:10) = A(1:9)

or turn the array around:

   A(10:1:-1) = A

Whether such statements are indeed useful, depends on your application,
but they can be done!

This part of the CHKSYSFF program determines if for a number of cases,
the results are indeed as expected. It is not a definitive test and may
give more insight in what the statements mean than whether the compiler
is capable of handling them, but it seems a good exercise anyway.

@end-of-file
