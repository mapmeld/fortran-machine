/* DOC

   Testcopy.java - Class of functions that copy a data item

   Arjen Markus

   General information:
   This class defines copy functions that are used by the test
   programs generated with TestMake. These functions are used to
   avoid problems with references, rather than actual data that
   are being compared.

   Notes:
   - All methods must be defined as "public static boolean"

   ENDDOC
*/

public class Testcopy {

   public static float Copy( float a )
   {
      return a ;
   }
   public static double Copy( double a )
   {
      return a ;
   }
   public static int Copy( int a )
   {
      return a ;
   }
   public static short Copy( short a )
   {
      return a ;
   }
   public static long Copy( long a )
   {
      return a ;
   }
   public static boolean Copy( boolean a )
   {
      return a ;
   }
   public static String Copy( String a )
   {
      return a ; /* This works because strings are immutable */
   }

   public static StringBuffer Copy( StringBuffer a )
   {
      return (new StringBuffer()).append( a ) ;
   }

   /* Arrays */
   public static float[] Copy( float[] a )
   {
      int     i ;
      float[] b ;
      b = new float[a.length] ;
      for ( i = 0 ; i < a.length ; i ++ )
      {
         b[i] = a[i] ;
      }
      return b ;
   }
}
