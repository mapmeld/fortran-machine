/* DOC

   Testcmp.java - Class of functions that compare two data items

   Arjen Markus

   General information:
   This class defines comparison functions that are used by the test
   programs generated with TestMake. Because it is impossible (?) to
   create a generic class, one may have to enhance this class, when
   a new class must be supported.

   Notes:
   - We require exact equality.
   - All methods must be defined as "public static boolean"

   ENDDOC
*/

public class Testcmp {

   public static boolean Equal( float a, float b )
   {
      return ( a == b ) ;
   }
   public static boolean Equal( int a, int b )
   {
      return ( a == b ) ;
   }
   public static boolean Equal( short a, short b )
   {
      return ( a == b ) ;
   }
   public static boolean Equal( double a, double b )
   {
      return ( a == b ) ;
   }
   public static boolean Equal( long a, long b )
   {
      return ( a == b ) ;
   }

   public static boolean Equal( boolean a, boolean b )
   {
      return ( (a&&b) || (!a&&!b) ) ;
   }
   public static boolean Equal( String a, String b )
   {
      return (a.compareTo(b) == 0) ;
   }

   public static boolean Equal( StringBuffer a, StringBuffer b )
   {
      return (a.toString().compareTo(b.toString()) == 0) ;
   }

   /* Equal arrays */
   public static boolean Equal( float[] a, float[] b )
   {
      int i ;
      if ( a.length != b.length )
      {
         return false ;
      }
      else
      {
         for ( i = 0 ; i < a.length ; i ++ )
         {
            if ( a[i] != b[i] )
            {
               return false ;
            }
         }
      }
      return true ;
   }
}
