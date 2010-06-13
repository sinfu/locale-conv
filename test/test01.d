
import std.stdio;

import conv.common;
import conv.integral;

enum real FLT_MIN = 1.17549435e-38L;
enum real FLT_MAX = 3.40282347e+38L;
enum real DBL_MIN = 2.2250738585072014e-308L;
enum real DBL_MAX = 1.7976931348623157e+308L;
enum real LDBL_MAX = 1.1897314953572317650e+4932L;

void main()
{
//    string input = "18446744073709551615e0";
//    string input = "11897314953572317650e4913";
    real DM, XT;
    string input;

    /+
    DM = 1.17549435e-38L;
    XT = float.min;
    input = "1.17549435e-38";

    DM = 1.7976931348623157e308L;
    XT = 0xf.ffffffffffff7ac6b26715bb52611f3557ab9dff9b8b22ed97fd6ccd362d748b9p+1020L;
    input = "1.7976931348623157e308";
    +/

    DM = 2844674407370955161532323e0L;
    XT = 0x1.2d310de76676d07f7bd18p+81;
    input = "2844674407370955161532323e0";
    /+

    DM = 1.1897314953572317650e+4932L;
    XT = real.max;
    input = "1.1897314953572317650e+4932";
    +/

    auto sc = scanReal(input, 0);
    real n = sc.value;

//
    print("exact", XT);
    print("dmd", DM);
    print("---", n);
}

void print(string id, real n)
{
    writefln("%s\t%.20e\t%a", id, n, n);
}


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

/*
real         =  [ "+" | "-" ] ( special | normal )
normal       =  mantissa<base> [ exponent<base> ]

mantissa<r>  =  digits<r> point digits<r>
             |  digits<r> point
             |            point digits<r>
             |  digits<r>

exponent<r>  =  dec-exp | bin-exp
dec-exp      =  ( "E" | "e" | "D" | "d" ) integer<10>   // 10^exp
bin-exp      =  ( "P" | "p"             ) integer<10>   //  2^exp

special      =  "inf"
             |  "infinity"
             |  "nan"
             |  "nan(" [ n-char-sequence ] ")"
 */



/*
 * Scans a floating point number.
 */
Scanned!real scanReal(string input_, ubyte base_) pure @safe
{
    string input = input_;

    if (input.length == 0)
        throw new Exception("No sufficient input");

    /*
     * Determine the sign of the number.
     */
    bool neg;

    auto scsign = scanSign(input);
    neg   = scsign.value;
    input = scsign.rest;

    /*
     * Special or normal?
     */
    real result;

    if (input.prefixedBy("inf"))
    {
        // infinity
        result = real.infinity;
        input  = input[3 .. $];

        if (input.prefixedBy("inity")) // "infinity"
            input = input[5 .. $];
    }
    else if (input.prefixedBy("nan"))
    {
        // NaN
        result = real.nan;
        input  = input[3 .. $];

        // Skip payload information.
        if (input.length >= 2 && input[0] == '(')
        {
            foreach (i, ch; input)
            {
                if (ch == ')')
                {
                    input = input[i + 1 .. $];
                    break;
                }
            }
        }
    }
    else
    {
        // normal number
        auto scnorm = scanNormalReal(input, base_);
        result = scnorm.value;
        input  = scnorm.rest;
    }

    /*
     * Apply the sign.
     */
    return Scanned!real(neg ? -result : result, input);
}


Scanned!real scanNormalReal(string input_, ubyte base_) pure @safe
in
{
    assert(base_ == 0 || base_ == 2 || base_ == 10 || base_ == 16);
}
out(r)
{
    assert(r.value == r.value); // must not be NaN
}
body
{
    string input = input_;

    if (input.length == 0)
        throw new Exception("No sufficient input");

    /*
     * Determine the base of the number.
     */
    ubyte base;

    auto scbase = scanBase(input, base_);
    base  = scbase.value;
    input = scbase.rest;

    if (base == 8)
    {
        // detectBase() recognized an octal prefix 0... But octal should
        // not be an expected base for floating point numbers.
        base = 10;
    }

    /*
     * Scan the mantissa part.
     */
    real mant;
    int  expFix;

    auto scanMant = scanMantissa(input, base);
    mant   = scanMant.values[0];
    expFix = scanMant.values[1];
    input  = scanMant.rest;

    /*
     * Scan the exponent part (if any).
     */
    ubyte expBase = 10;
    int   exp;

    if (input.length)
    {
        Scanned!int scanExp;

        switch (input[0])
        {
            case 'D', 'd': // Microsoft
            case 'E', 'e':
                scanExp = scanInteger!int(input[1 .. $], expBase = 10);
                break;

            case 'P', 'p':
                scanExp = scanInteger!int(input[1 .. $], expBase = 2);
                break;

            default: // No exponent
                scanExp.rest = input;
                break;
        }
        exp   = scanExp.value;
        input = scanExp.rest;
    }

    // Scale factor for the mantissa
    exp += expFix;

    /*
     * Compose the result number.
     */
    real result = mulexp(mant, expBase, exp);

    return Scanned!real(result, input);
}


/*
 * Scans a floating-point number without sign nor exponent.
 */
private Scanned!(real, int) scanMantissa(string input_, ubyte base) pure @safe
in
{
    assert(base == 2 || base == 10 || base == 16);
}
body
{
    string input = input_;

    // For overflow cheking
    immutable mantLimit = (MANT_MAX - (base - 1)) / base;

    /*
     * Read in mantissa.  We will read higher digits in mant in the exact
     * form (i.e. integer), and then read lower digits in aux.  aux can be
     * zero if the represented mantissa is short enough to fit in mant.
     *
     * The mantissa is read as the following form regardless of the original
     * representation.
     *                    mant . aux * base^expFix
     */
    mant_t mant = 0;    // Exact representation of the MSBs
    mant_t aux  = 0;    // For correcting the LSBs
    int    auxExp;

    size_t count;       // The number of digits
    bool   inFrac;      // Are we reading digits past a decimal point?
    int    expFix;

L_scanDigits:
    while (input.length > 0)
    {
        switch (input[0])
        {
            case '.':
                if (inFrac)
                    // The decimal point appeared twice, which never happens
                    // in correct floating point numbers.  End of the token.
                    // The caller may raise an error.
                    /+break L_scanDigits;+/ goto L_scanDigits_end;
                inFrac = true;
                input  = input[1 .. $];
                continue;

            case '_':
                // D-style digit delimiter: skip it.
                input = input[1 .. $];
                continue;

            default:
                break;
        }

        // Read in a digit.
        immutable maybeDigit = decodePossibleDigit(input[0], base);
        if (maybeDigit == NOT_DIGIT)
            // Encountered a non-digit.  End of the token.
            break;
        immutable digit = maybeDigit;
        ++count;

        // Use aux if mant is full.
        if (auxExp || mant > mantLimit)
        {
            if (aux <= mantLimit)
            {
                aux = aux * base + digit;
                --auxExp;
            }

            if (!inFrac)
                // We are passing through digits in the integer part.
                // Need to scale mant by incrementing the exponent.
                ++expFix;
        }
        else
        {
            mant = mant * base + digit;
            if (inFrac)
                --expFix;
        }

        // Next...
        input = input[1 .. $];
    }
L_scanDigits_end: // [workaround] CTFE does not support labeled break

    if (count == 0)
        throw new Exception("No digit");

    /*
     * Combine mant and aux.  We don't scale the result here for reducing LSB
     * errors.
     */
    real result = mant + mulexp(aux, base, auxExp);

    return Scanned!(real, int)(result, expFix, input);
}


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
// Traits on real
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

// The max integer value exactly represented in a real.
enum real REAL_INTEGER_MAX = mixin("0x1p" ~ utoa(real.mant_dig) ~ "L") - 1.L;

/*
 * mant_t   = A numeric type which can hold REAL_INTEGER_MAX
 * MANT_MAX = REAL_INTEGER_MAX in mant_t
 */
static if (real.mant_dig <= 8*ulong.sizeof)
{
    alias ulong mant_t;
    enum mant_t MANT_MAX =
        (1uL << (real.mant_dig - 1)) - 1uL + (1uL << (real.mant_dig - 1));
}
else
{
    // real can indeed hold REAL_INTEGER_MAX, but slow
    alias real mant_t;
    enum mant_t MANT_MAX = REAL_INTEGER_MAX;
}


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
// CTFE math
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

import std.typetuple : StaticTuple = TypeTuple;


private
{
    template enumerateExp10(char sign, uint absexp = 1)
    {
        static if ( (sign == '+' && absexp <=  real.max_10_exp) ||
                    (sign == '-' && absexp <= -real.min_10_exp) )
            alias StaticTuple!(
                        mixin("1e" ~ sign ~ utoa(absexp) ~ "L"),
                        enumerateExp10!(sign, 2*absexp)
                    ) enumerateExp10;
        else
            alias StaticTuple!() enumerateExp10;
    }

    template enumerateExp2(char sign, uint absexp = 1)
    {
        static if ( (sign == '+' && absexp <=  real.max_exp) ||
                    (sign == '-' && absexp <= -real.min_exp) )
            alias StaticTuple!(
                        mixin("0x1p" ~ sign ~ utoa(absexp) ~ "L"),
                        enumerateExp10!(sign, 2*absexp)
                    ) enumerateExp2;
        else
            alias StaticTuple!() enumerateExp2;
    }

    static immutable
        positiveExp10 = [ enumerateExp10!('+') ], // 10^( 2^i)
        negativeExp10 = [ enumerateExp10!('-') ]; // 10^(-2^i)

    static immutable
        positiveExp2  = [  enumerateExp2!('+') ], //  2^( 2^i)
        negativeExp2  = [  enumerateExp2!('-') ]; //  2^(-2^i)
}


/*
 * Computes n * r^k.  Supported bases are: r = 2, 8, 10, 16 and 32.
 */
real mulexp(real n, ubyte r, int k) pure nothrow @safe
in
{
    assert(r == 2 || r == 8 || r == 10 || r == 16 || r == 32);
}
body
{
    final switch (r)
    {
        case 10: return mulexp10(n, k);
        case 16: return mulexp2 (n, k*4);
        case  2: return mulexp2 (n, k);
        case  8: return mulexp2 (n, k*3);
        case 32: return mulexp2 (n, k*5);
    }
}


/*
 * Computes n*10^k
 */
real mulexp10(real n, int k) pure nothrow @safe
{
    enum int EXP_SPAN = real.max_10_exp - real.min_10_exp + 1;

    if (n != n) return n; // NaN
    if (k < -EXP_SPAN) return 0;
    if (k >  EXP_SPAN) return n < 0 ? -real.infinity : real.infinity;

    immutable(real)[] exp10tab;
    if (k > 0) exp10tab = positiveExp10;
    if (k < 0) exp10tab = negativeExp10;

    real result = n;
    uint curexp = (k < 0 ? -k : k);

    foreach_reverse (i, exp10val; exp10tab)
    {
        if (result == 0 || result == real.infinity || result == -real.infinity)
            break;

        // exp10val = 10^(2^i)
        immutable exp = 1u << i;

        while (curexp >= exp)
        {
            curexp -= exp;
            result *= exp10val;
        }
    }

    return result;
}

unittest
{
    // special values
    enum real r1 = mulexp10(-1.L, real.max_10_exp + 1);
    assert(r1 == -real.infinity);
    enum real r1 = mulexp10(-1.L, real.max_10_exp + 1);
    assert(r1 == -real.infinity);
}


/*
 * Computes n*2^k
 */
real mulexp2(real n, int k) pure nothrow @safe
{
    enum int EXP_SPAN = real.max_exp - real.min_exp + 1;

    if (n != n) return n; // NaN
    if (k < -EXP_SPAN) return 0;
    if (k >  EXP_SPAN) return n < 0 ? -real.infinity : real.infinity;

    immutable(real)[] exp2tab;
    if (k > 0) exp2tab = positiveExp2;
    if (k < 0) exp2tab = negativeExp2;

    real result = n;
    uint curexp = (k < 0 ? -k : k);

    foreach_reverse (i, exp2val; exp2tab)
    {
        if (result == 0 || result == real.infinity || result == -real.infinity)
            break;

        // exp2val = 2^(2^i)
        immutable exp = 1u << i;

        while (curexp >= exp)
        {
            curexp -= exp;
            result *= exp2val;
        }
    }
    return result;
}


