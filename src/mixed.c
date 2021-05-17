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
#define bit_value(number, bit) ((number>>bit) & 1)
#define set_bit(number, bit) number |= (1 << bit)


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
    char byte[16];
} ieee_double;


#ifdef WORDS_BIGENDIAN
// First two bytes are sign & exponent
// Last four bytes (that is, 32 bits) are 1954
const int TAG_BYTE = 3;
#else
const int TAG_BYTE = 4;
#endif


SEXP _tag(SEXP x) {
    int n = Rf_length(x);
    SEXP out = PROTECT(Rf_allocVector(REALSXP, n));

    for (int i = 0; i < n; ++i) {
        
        int nchars = Rf_length(STRING_ELT(x, i));
        int number = 0;
        Rboolean numeric = TRUE;
        Rboolean firstminus = CHAR(STRING_ELT(x, i))[0] == CHAR(mkChar("-"))[0];

        // int test;
        // sscanf(CHAR(STRING_ELT(x, i)), "%d", &test);
        // // printf("%d\n", test);

        // test if string is numeric, and if so transform it into the corresponding number
        for (int c = firstminus; c < nchars; c++) {
            int charc = CHAR(STRING_ELT(x, i))[c] - '0';
            if (charc >= 0 && charc <= 9 && numeric) {
                number = number * 10 + charc;
            }
            else {
                numeric = FALSE;
                firstminus = FALSE;
                if (nchars > 2) {
                    nchars = 2;
                }
            }
        }

        if (number > 32767) {
             Rf_errorcall(R_NilValue, "Number(s) too large, use the R function tag().");
        }
        
        ieee_double y;
        y.value = NA_REAL;

        if (firstminus) {
            y.value = -1 * NA_REAL; // set the sign bit
        }

        int bytepos = TAG_BYTE;
        int bytepos2 = bytepos + ((bytepos == 3) ? -1 : 1);

        if (numeric) {
            // any entry from the ASCII table can be represented in 7 bits
            // the 8th is therefore available as a flag
            set_bit(y.byte[bytepos2], 7);

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
            for (int c = 0; c < nchars; c++) {
                y.byte[bytepos] = CHAR(STRING_ELT(x, i))[c + firstminus];
                bytepos += (bytepos == 3) ? -1 : 1;
            }
        }

        
        REAL(out)[i] = y.value;
    }

    UNPROTECT(1);
    return(out);
}





SEXP _has_tag(SEXP x, SEXP tag_) {
    int n = Rf_length(x);
    SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));

    if (TYPEOF(x) != REALSXP) {
        for (int i = 0; i < n; ++i) {
            LOGICAL(out)[i] = 0;
        }
    }
    else {
        for (int i = 0; i < n; ++i) {
            double xi = REAL(x)[i];
            LOGICAL(out)[i] = 1;

            if (!isnan(xi)) {
                LOGICAL(out)[i] = 0;
            }
            else {
                
                ieee_double y;
                y.value = xi;
                int bytepos = TAG_BYTE;
                int bytepos2 = bytepos + ((bytepos == 3) ? -1 : 1);
                Rboolean xi_numeric = bit_value(y.byte[bytepos2], 7) == 1;

                char tag = y.byte[TAG_BYTE];
                
                if (TYPEOF(tag_) == NILSXP) {
                    if (!xi_numeric && (tag == '\0')) {
                        LOGICAL(out)[i] = 0;
                    }
                }
                
                else {
                    if (TYPEOF(tag_) == STRSXP) {
                        if (Rf_length(tag_) != 1) {
                            Rf_errorcall(R_NilValue, "`tag` should be a vector of length 1");
                        }
                        
                        Rboolean tag_minus = CHAR(STRING_ELT(tag_, 0))[0] == CHAR(mkChar("-"))[0];
                        Rboolean xi_minus = signbit(xi);
                        
                        LOGICAL(out)[i] = 1 * ((1 * tag_minus + 1 * xi_minus) != 1);

                        if (LOGICAL(out)[i]) {
                            int nchars = Rf_length(STRING_ELT(tag_, 0));
                            int tag_number = 0;
                            Rboolean tag_numeric = TRUE;

                            for (int c = tag_minus; c < nchars; c++) {
                                int charc = CHAR(STRING_ELT(tag_, 0))[c] - '0';
                                if (charc >= 0 && charc <= 9 && tag_numeric) {
                                    tag_number = tag_number * 10 + charc;
                                }
                                else {
                                    tag_numeric = FALSE;
                                }
                            }

                            
                            if (tag_number > 32767) {
                                Rf_errorcall(R_NilValue, "`tag` number too large, use the R function has_tag()");
                            }

                            LOGICAL(out)[i] = 1 * ((1 * xi_numeric + 1 * tag_numeric) != 1);

                            if (LOGICAL(out)[i]) {
                                Rboolean test = TRUE;

                                if (xi_numeric) {
                                    int xi_number = 0;
                                    int power = 1;

                                    for (int bit = 0; bit < 8; bit++) {
                                        xi_number += bit_value(y.byte[bytepos], bit) * power;
                                        power *= 2;
                                    }

                                    for (int bit = 0; bit < 7; bit++) {
                                        xi_number += bit_value(y.byte[bytepos2], bit) * power;
                                        power *= 2;
                                    }

                                    test = xi_number == tag_number;
                                }
                                else {
                                    test = test && y.byte[bytepos] == CHAR(STRING_ELT(tag_, 0))[0];
                                    if (nchars > 1) {
                                        test = test && y.byte[bytepos2] == CHAR(STRING_ELT(tag_, 0))[1];
                                    }
                                }

                                if (test) {
                                    LOGICAL(out)[i] = 1;
                                }
                            }
                        }
                    }
                    else {
                        LOGICAL(out)[i] = 0;
                    }
                }
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
        double xi = REAL(x)[i];

        if (!isnan(xi)) {
            SET_STRING_ELT(out, i, NA_STRING);
        }
        else {
            ieee_double y;
            y.value = xi;

            int bytepos = TAG_BYTE;
            int bytepos2 = bytepos + ((bytepos == 3) ? -1 : 1);

            Rboolean numeric = bit_value(y.byte[bytepos2], 7) == 1;
            
            char tag[24];

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

                Rboolean minus = signbit(xi);
                if (minus) {
                    number *= -1;
                }
                
                sprintf(tag, "%d", number);
                SET_STRING_ELT(out, i,  Rf_mkCharLenCE(tag, strlen(tag), CE_UTF8));
                
            }
            else {
                
                tag[0] = y.byte[TAG_BYTE];

                if (tag[0] == '\0') {
                    SET_STRING_ELT(out, i, NA_STRING);
                }
                else {
                    tag[1] = y.byte[bytepos2];
                    SET_STRING_ELT(out, i,  Rf_mkCharLenCE(tag, ((tag[1] == '\0') ? 1 : 2), CE_UTF8));
                }
            }
        }
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
