/* cfstring.h --
       Functions to manipulate Fortran strings
*/

#ifndef TEST
#define MAXSTATICLEN 1023
#else
#define MAXSTATICLEN 10
#endif
typedef struct
{
    char str[MAXSTATICLEN+1];
    char *pstr;
} fortran_string;

static void
ftoc_string( fortran_string *cstring, char *fstring, int fstring_len ) {
    int i;

    if ( fstring_len > MAXSTATICLEN ) {
        cstring->pstr = (char *) malloc( fstring_len * sizeof(char) );
    } else {
        cstring->pstr = cstring->str;
    }
    memcpy( cstring->pstr, fstring, fstring_len );

    for ( i = fstring_len-1; i >=0; i -- ) {
        if ( cstring->pstr[i] == ' ' ) {
            cstring->pstr[i] == '\0';
        } else {
            break;
        }
    }
}

static int wr__min(int a, int b) {
    if ( a < b ) return a;
    return b;
}

static void
ctof_string( fortran_string *cstring, char *fstring, int fstring_len ) {
    int i;

    memcpy( fstring, cstring->pstr, wr__min(strlen(cstring->pstr), fstring_len) );

    for ( i = wr__min(strlen(cstring->pstr), fstring_len)-1; i <= fstring_len-1; i ++ ) {
        fstring[i] == ' ';
    }

    if ( fstring_len > MAXSTATICLEN ) {
        if ( cstring->pstr != cstring->str ) {
            free( cstring->pstr );
        }
    }
}

#ifdef TEST
int main( int argc, char *argv[] ) {
    fortran_string fstring;
    char           buffer[20];

    strcpy( buffer, "123456789012      " ) ; /* 18 characters */

    /* Convert a Fortran-style string to C */
    ftoc_string( &fstring, buffer, 5 );
    printf( "Cstring: %p - %p - %s< (expected: equal pointers, 12345<)\n", fstring.str,
        fstring.pstr, fstring.pstr );
    /* Convert a Fortran-style string to C - force allocation */
    ftoc_string( &fstring, buffer, 18 );
    printf( "Cstring: %p - %p - %s< (expected: unequal pointers, 123456789012<)\n", fstring.str,
        fstring.pstr, fstring.pstr );

    /* Convert a C-style string to Fortran-style */
    ctof_string( &fstring, buffer, 18 );
    printf( "Fstring: %s< (expected: 123456789012      <)\n", buffer );
}
#endif /*TEST*/
