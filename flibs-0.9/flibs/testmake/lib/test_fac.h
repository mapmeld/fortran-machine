/* DOC

   test_fac.h - Header file for the test facilities in TestMake (C)

   Copyright (C) 2002 Arjen Markus

   Arjen Markus

   General information:
   This header file defines the auxiliary functions used in the TestMake
   utility. It is needed for the proper compilation.

   ENDDOC
*/

/* Generated routines */
void initialise(   void ) ;
void summary(      void ) ;
void check_result( void ) ;
void run_module(   void ) ;

/* Alias for memory allocation */
#define malloc test__malloc

/* Private functions */
void test__copy(  void **a, void **b, char *type, size_t size ) ;
int  test__equal( void **a, void **b, char *type, size_t size ) ;

