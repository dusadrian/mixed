#include <R.h>
#include <Rinternals.h>
#include <stdbool.h>
#include <R_ext/Rdynload.h>


// copied from: https://gist.github.com/wch/3280369#file-unlockenvironment-r
#define FRAME_LOCK_MASK (1<<14)
#define FRAME_IS_LOCKED(e) (ENVFLAGS(e) & FRAME_LOCK_MASK)
#define UNLOCK_FRAME(e) SET_ENVFLAGS(e, ENVFLAGS(e) & (~ FRAME_LOCK_MASK))

SEXP _unlockEnvironment(SEXP env) {
    UNLOCK_FRAME(env);
    
    SEXP result = PROTECT( Rf_allocVector(LGLSXP, 1) );
    LOGICAL(result)[0] = FRAME_IS_LOCKED(env) == 0;
    UNPROTECT(1);
    return result;
}



// adapted from https://stackoverflow.com/questions/11815894/how-to-read-write-arbitrary-bits-in-c-c
#define bit_value(data, y) ((data >> y) & 1)
#define set_bit(data, y) data |= (1 << y)
#define clear_bit(data, y)  data &= ~(1 << y)

/*

https://stackoverflow.com/questions/51684861/how-does-r-represent-na-internally
https://github.com/wch/r-source/blob/HEAD/src/include/R_ext/Arith.h#L41-L45
https://github.com/wch/r-source/blob/HEAD/src/main/arithmetic.c#L112-L120
http://www.cs.toronto.edu/~radford/ftp/fltcompress.pdf

https://stackoverflow.com/questions/23212538/float-and-double-significand-numbers-mantissa-pov
https://github.com/wch/r-source/blob/HEAD/src/main/arithmetic.c#L112-L120
https://graphics.stanford.edu/~seander/bithacks.html


THE FOLLOWING FUNCTIONS ARE ADAPTED FROM PACKAGE HAVEN

IEEE 754 defines binary64 as
* 1  bit : sign
* 11 bits: exponent
* 52 bits: significand

R stores the value "1954" in the last 32 bits: this payload marks
the value as a NA, not a regular NaN.

(Note that this discussion like most discussion of FP on the web, assumes
a big-endian architecture - in little endian the sign bit is the last
bit)
*/



typedef union {
    double value;
    char byte[8];
} ieee_double;


#ifdef WORDS_BIGENDIAN
// First two bytes are sign & exponent
// Last four bytes (that is, 32 bits) are 1954
const int TAG_BYTE = 3;
#else
const int TAG_BYTE = 4;
#endif


const int WILDCARD = 45; // "-"


Rboolean isASCII(unsigned char ch) {
    /*
    unsigned char is a "byte", but bytes can have different sizes on different
    platforms: some have 9-bit, 32-bit, or 64-bit bytes
    (https://isocpp.org/wiki/faq/intrinsic-types#bits-per-byte)
    Not only can a single character be stored in a variable number of bytes but
    it can be displayed in 1, 2 or even 0 columns.
    https://developer.r-project.org/Encodings_and_R.html
    */
    
    int bit = 0;
    unsigned int a;
    // printf("char:%hhu\n", ch);

    // for (int bit = 15; bit >= 0; bit--) {
    //     printf("%d", bit_value(ch, bit));
    // }
    // printf("\n");

    for(a = 0; ch; ch >>=1) {
        bit++;
        a += ch & 1;
    }

    return(bit < 8);
}


SEXP _tag(SEXP x) {

    if (TYPEOF(x) != STRSXP) {
        Rf_errorcall(R_NilValue, "`x` must be a character vector");
    }

    int n = Rf_length(x);
    SEXP out = PROTECT(Rf_allocVector(REALSXP, n));

    for (int i = 0; i < n; ++i) {
        
        ieee_double y;
        y.value = NA_REAL;
        
        int nchars = Rf_length(STRING_ELT(x, i));

        Rboolean ascii = TRUE;
        int c = 0;
        while (c < nchars && ascii) {
            ascii = isASCII(CHAR(STRING_ELT(x, i))[c]);
            c++;
        }

        if (!ascii) {
            Rf_errorcall(R_NilValue, "Only ASCII characters can be tagged.");
        }

        int number = 0;
        Rboolean numeric = TRUE;
        Rboolean firstminus = CHAR(STRING_ELT(x, i))[0] == WILDCARD;
        Rboolean thirdminus = nchars > 2 && CHAR(STRING_ELT(x, i))[2] == WILDCARD;

        // transform string into the corresponding number, if numeric
        for (int c = firstminus; c < nchars; c++) {
            int charc = CHAR(STRING_ELT(x, i))[c] - '0';
            if (charc >= 0 && charc <= 9 && numeric) {
                number = number * 10 + charc;
            }
            else {
                numeric = FALSE;
                if (nchars > 2 + firstminus + thirdminus) {
                    nchars = 2 + firstminus + thirdminus;
                }
            }
        }

        if (number > 32767) {
             Rf_errorcall(R_NilValue, "Number(s) too large, use the R function tag().");
        }

        int bytepos = TAG_BYTE;
        int bytepos2 = bytepos + ((bytepos == 3) ? -1 : 1);

        if (numeric) {
            y.value = -1 * NA_REAL; // signal numeric using the sign bit

            // transform the number into its binary representation
            // the number does not exceed 32767, to fit in the available 15 bits
            int binary[15]; 
            for (int bit = 0; bit < 15; bit++) {
                binary[bit] = 0;
            }

            int bit = 0;
            while (number > 0) {
                // storing remainder in binary array
                binary[bit] = number % 2;
                number = number / 2;
                bit++;
            }

            for (int bit = 0; bit < 8; bit++) { // store the first 8 bits of the number
                if (binary[bit] == 1) {
                    set_bit(y.byte[bytepos], bit);
                }
            }

            for (int bit = 0; bit < 7; bit++) { // store the last 7 bits of the number
                if (binary[bit + 8] == 1) {
                    set_bit(y.byte[bytepos2], bit);
                }
            }
        }
        else {
            for (int c = firstminus; c < nchars; c++) {
                if (c == 2 && thirdminus) {
                    set_bit(y.byte[TAG_BYTE], 7);
                }
                else {
                    y.byte[bytepos] = CHAR(STRING_ELT(x, i))[c];
                    bytepos += (bytepos == 3) ? -1 : 1;
                }
            }
        }

        if (firstminus) {
            // any entry from the ASCII table can be represented in 7 bits
            // the 8th (or in this case, the 16th) is available as a sign bit (1 means negative)
            set_bit(y.byte[bytepos2], 7);
        }
        
        REAL(out)[i] = y.value;
    }

    UNPROTECT(1);
    return(out);
}




SEXP _extract_tag (double xi) {
    SEXP out = PROTECT(Rf_allocVector(STRSXP, 1));
    
    SET_STRING_ELT(out, 0, NA_STRING);

    if (isnan(xi)) {
        char tag[8];
        ieee_double y;
        y.value = xi;

        int bytepos = TAG_BYTE;
        int bytepos2 = bytepos + ((bytepos == 3) ? -1 : 1);
        
        Rboolean numeric = signbit(xi);
        Rboolean firstminus = bit_value(y.byte[bytepos2], 7) == 1;

        // for (int i = 0; i < 2; i++) {
        //     if (i == 0) {
        //         for (int j = 7; j >= 0; j--) {
        //             printf("%d", bit_value(y.byte[bytepos], j));
        //         }
        //     }
        //     else {
        //         for (int j = 7; j >= 0; j--) {
        //             printf("%d", bit_value(y.byte[bytepos2], j));
        //         }
        //     }
        //     printf("\n");
        // }

        if (numeric) {
            int number = 0;
            int power = 1;

            for (int bit = 0; bit < 8; bit++) {
                number += bit_value(y.byte[bytepos], bit) * power;
                power *= 2;
            }

            for (int bit = 0; bit < 7; bit++) {
                number += bit_value(y.byte[bytepos2], bit) * power;
                power *= 2;
            }
            
            if (firstminus) {
                number *= -1;
            }
            
            sprintf(tag, "%d", number);
            SET_STRING_ELT(out, 0,  Rf_mkCharLenCE(tag, strlen(tag), CE_UTF8));
        }
        else {
            Rboolean thirdminus = bit_value(y.byte[bytepos], 7) == 1;

            clear_bit(y.byte[bytepos], 7);
            clear_bit(y.byte[bytepos2], 7);

            int pos = 0;
            
            if (firstminus) {
                tag[pos] = WILDCARD;
                pos++;
            }
            
            if (y.byte[TAG_BYTE] == '\0') {
                if (pos == 0) {
                    SET_STRING_ELT(out, 0, NA_STRING);
                }
                else {
                    SET_STRING_ELT(out, 0, Rf_mkCharLenCE(tag, pos, CE_UTF8));
                }
            }
            else {
                tag[pos] = y.byte[TAG_BYTE];
                pos++;

                if (thirdminus) {
                    tag[pos] = WILDCARD;
                    pos++;
                }

                if (y.byte[bytepos2] != '\0') {
                    tag[pos] = y.byte[bytepos2];
                    pos++;
                }
                
                SET_STRING_ELT(out, 0, Rf_mkCharLenCE(tag, pos, CE_UTF8));
            }
        }
    }

    UNPROTECT(1);
    return out;
}






SEXP _has_tag(SEXP x, SEXP tag_) {
    int n = Rf_length(x);
    SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));

    for (int i = 0; i < n; ++i) {
        LOGICAL(out)[i] = 1; // initialize
    }
    
    if (TYPEOF(x) != REALSXP) {
        for (int i = 0; i < n; ++i) {
            LOGICAL(out)[i] = 0;
        }
    }
    else {
        for (int i = 0; i < n; ++i) {
            SEXP tag = _extract_tag(REAL(x)[i]);

            if (STRING_ELT(tag, 0) == NA_STRING) {
                LOGICAL(out)[i] = FALSE;
            }
            else if (TYPEOF(tag_) == STRSXP) {
                const char value_tag = *CHAR(STRING_ELT(tag, 0));
                const char value_tag_ = *CHAR(STRING_ELT(tag_, 0));
                LOGICAL(out)[i] = value_tag == value_tag_;
            }
            else {
                LOGICAL(out)[i] = CHAR(STRING_ELT(tag, 0))[0] != '\0';
            }
        }
    }
    
    UNPROTECT(1);
    return out;
}





SEXP _get_tag(SEXP x) {

    int n = Rf_length(x);
    SEXP out = PROTECT(Rf_allocVector(STRSXP, n));

    for (int i = 0; i < n; ++i) {
        
        SEXP tag = _extract_tag(REAL(x)[i]);
        SET_STRING_ELT(out, i, STRING_ELT(tag, 0));
        
    }

    UNPROTECT(1);
    return out;
}



/*

Representations:
NA_real_              ---------------- 16 available bits to play with
bin: 0111111111110000 0000000000000000 00000000000000000000100000000000
hex: 7ff00000000007a2

negative NA_real_ (using the sign bit)
bin: 1111111111110000000000000000000000000000000000000000100000000000
hex: fff00000000007a2



ASCII table
https://www.cs.cmu.edu/~pattis/15-1XX/common/handouts/ascii.html
Dec  = Decimal Value
Char = Character

'5' has the int value 53
if we write '5'-'0' it evaluates to 53-48, or the int 5
if we write char c = 'B'+32; then c stores 'b'


Dec  Char                           Dec  Char     Dec  Char     Dec  Char
---------                           ---------     ---------     ----------
  0  NUL (null)                      32  SPACE     64  @         96  `
  1  SOH (start of heading)          33  !         65  A         97  a
  2  STX (start of text)             34  "         66  B         98  b
  3  ETX (end of text)               35  #         67  C         99  c
  4  EOT (end of transmission)       36  $         68  D        100  d
  5  ENQ (enquiry)                   37  %         69  E        101  e
  6  ACK (acknowledge)               38  &         70  F        102  f
  7  BEL (bell)                      39  '         71  G        103  g
  8  BS  (backspace)                 40  (         72  H        104  h
  9  TAB (horizontal tab)            41  )         73  I        105  i
 10  LF  (NL line feed, new line)    42  *         74  J        106  j
 11  VT  (vertical tab)              43  +         75  K        107  k
 12  FF  (NP form feed, new page)    44  ,         76  L        108  l
 13  CR  (carriage return)           45  -         77  M        109  m
 14  SO  (shift out)                 46  .         78  N        110  n
 15  SI  (shift in)                  47  /         79  O        111  o
 16  DLE (data link escape)          48  0         80  P        112  p
 17  DC1 (device control 1)          49  1         81  Q        113  q
 18  DC2 (device control 2)          50  2         82  R        114  r
 19  DC3 (device control 3)          51  3         83  S        115  s
 20  DC4 (device control 4)          52  4         84  T        116  t
 21  NAK (negative acknowledge)      53  5         85  U        117  u
 22  SYN (synchronous idle)          54  6         86  V        118  v
 23  ETB (end of trans. block)       55  7         87  W        119  w
 24  CAN (cancel)                    56  8         88  X        120  x
 25  EM  (end of medium)             57  9         89  Y        121  y
 26  SUB (substitute)                58  :         90  Z        122  z
 27  ESC (escape)                    59  ;         91  [        123  {
 28  FS  (file separator)            60  <         92  \        124  |
 29  GS  (group separator)           61  =         93  ]        125  }
 30  RS  (record separator)          62  >         94  ^        126  ~
 31  US  (unit separator)            63  ?         95  _        127  DEL
*/
