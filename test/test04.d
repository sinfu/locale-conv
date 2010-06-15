/*
 * without locale
 */

version = use_bigint_method;

import std.conv;
import std.math;
import std.stdio;
import std.traits;
import std.typetuple;

import core.stdc.stdint;

void main()
{
    real dmd, n;
    string str;
    real exact;

    str = "1.234";
    dmd = 1.234L;
    exact = 0x1.3be76c8b4395810624dd2f1a9fbe76c8b4395810624dd2f1a9fbe76c8b43958p+0L;
    n = toReal(str);
    writefln("%.20e", n);
    writefln("%.20e", dmd);
    writefln("%.20e", exact);
    writeln("--------------------");

    str = "3.3621031431120935063e-4932";
    dmd = 3.3621031431120935063e-4932L;
    exact = real.min_normal;
    n = toReal(str);
    writefln("%.20e", n);
    writefln("%.20e", dmd);
    writefln("%.20e", exact);
    assert(!isSubnormal(n));
    writeln("--------------------");

    str = "1.1897314953572317650e+4932";
    dmd = 1.1897314953572317650e+4932L;
    exact = real.max;
    n = toReal(str);
    writefln("%.20e", n);
    writefln("%.20e", dmd);
    writefln("%.20e", exact);
    writeln("--------------------");

    str = "1.999999999999999999999999999999999999999999999999999999999999999999999999999999999";
    dmd =  1.999999999999999999999999999999999999999999999999999999999999999999999999999999999L;
    exact = 2.L;
    n = toReal(str);
    writefln("%.20e", n);
    writefln("%.20e", dmd);
    writefln("%.20e", exact);
}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

real toReal(String)(String str)// pure @safe
    if (isSomeString!(String))
{
    /+@@@ BUG @@@ auto+/typeof(str.idup) input = str[];
    real result;

    if (input.length == 0)
        throw new ConvError("empty");

    /* Read a sign if any. */
    bool negate;

    negate = input[0] == '-';
    if (negate || input[0] == '+')
        input = input[1 .. $];

version (use_bigint_method)
{
    /*
     * Read mantissa digits.  We use a bigint-like way to deal with long
     * mantissa without dropping precision.
     */
    uintmax_t mantHi, mantLo;
    bool overprec;
    bool inFrac;
    int  expFix;
    uint ndigits;

L_scanMantissa:
    for (; input.length; input = input[1 .. $])
    {
        switch (input[0])
        {
            case '.':
                assert(!inFrac);
                inFrac = true;
                continue;

            case 'E', 'e':
                //break L_scanMantissa; @@@ CTFE
                goto L_scanMantissa_end;

            default:
                break;
        }
        auto digit = input[0] - '0';

        ++ndigits;
        if (inFrac && !overprec)
            --expFix;

        if (ndigits <= (mantLo.sizeof * 2))
        {
            mantLo = mantLo*10 + digit;
        }
        else if (!overprec)
        {
            enum HBIT = 4*uintmax_t.sizeof;
            enum HALF = (cast(uintmax_t) 1u << HBIT) - 1u;

            auto tmpL = (mantLo  & HALF) * 10 + digit;
            auto tmpM = (mantLo >> HBIT) * 10 + (tmpL >> HBIT);
            auto tmpH =  mantHi          * 10;

            mantLo =        (tmpM << HBIT) + (tmpL & HALF);
            mantHi = tmpH + (tmpM >> HBIT);

            if (mantHi >= (mantHi.max / 10))
                overprec = true;
        }
    }
L_scanMantissa_end:
    // compose the mantissa part
    result = mantHi * 0x1.p64 + mantLo;
}
else
{
    /*
     * Read in mantissa.  We will read higher digits in mant in the exact
     * form (i.e. integer), and then read lower digits in aux.  aux can be
     * zero if the represented mantissa is short enough to fit in mant.
     *
     * The mantissa is read as the following form regardless of the original
     * representation.
     *                    mant . aux * base^mantExp
     */
    alias ulong mant_t;
    enum mant_t MANT_LIMIT = mant_t.max/10;

    mant_t mant = 0;    // Exact representation of the MSBs
    mant_t aux  = 0;    // For correcting the LSBs
    int    auxExp;

    bool   inFrac;      // Are we reading digits past a decimal point?
    int    mantExp;

L_scanDigits:
    for (; input.length != 0; input = input[1 .. $])
    {
        switch (input[0])
        {
            case '.':
                inFrac = true;
                continue;

            case 'E', 'e':
                //break L_scanDigits_end;
                goto L_scanDigits_end;

            default:
                break;
        }

        // Read in a digit.
        auto digit = input[0] - '0';

        // Use aux if mant is full.
        if (auxExp || mant >= MANT_LIMIT)
        {
            if (aux < MANT_LIMIT)
            {
                aux = aux * 10 + digit;
                --auxExp;
            }

            if (!inFrac)
                // We are passing through digits in the integer part.
                // Need to scale mant by incrementing the exponent.
                ++mantExp;
        }
        else
        {
            mant = mant * 10 + digit;
            if (inFrac)
                --mantExp;
        }
    }
L_scanDigits_end: // [workaround] CTFE does not support labeled break
    result = mant + ldexp10(aux, auxExp);
    int expFix = mantExp;
}

    /*
     * Read an exponent if any.  We should return infinity for huge exponents,
     * so we won't use ready-made integer scanner which throws an exception on
     * an integer overflow.
     */
    int  exp;
    bool expOverflow;

    if (input.length)
    {
        input = input[1 .. $];

        //
        bool negateExp;

        negateExp = input[0] == '-';
        if (negateExp || input[0] == '+')
            input = input[1 .. $];

        //
        for (; input.length; input = input[1 .. $])
        {
            auto digit = input[0] - '0';

            exp = exp*10 + digit;

            if (exp > 100000)
            {
                expOverflow = true;
                break;
            }
        }

        //
        if (negateExp)
            exp = -exp;
    }
    exp += expFix;

    //
    if (expOverflow)
        result = (exp > 0) ? real.infinity : 0;
    else
        result = ldexp10(result, exp);

    return negate ? -result : result;
}


/*
 */
real ldexp10(real x, int n) pure nothrow @safe
{
    //return mulexp10(x, n);
    if (n < real.min_10_exp)
        return ldexp10(x * mulexp10(1.L, real.min_10_exp), n - real.min_10_exp);
    else if (n > real.max_10_exp)
        return ldexp10(x * mulexp10(1.L, real.max_10_exp), n - real.max_10_exp);
    else
        return x * mulexp10(1.L, n);
}


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

/*
 * Returns $(D true) if two characters $(D a) and $(D b) are the same ASCII
 * letter except for their cases.
 */
bool sameAsciiAlpha(dchar a, dchar b) pure nothrow @safe
{
    return toAsciiLower(a) == toAsciiLower(b);
}

/*
 * .
 */
dchar toAsciiLower(dchar ch) pure nothrow @safe
{
    if ('A' <= ch && ch <= 'Z')
        return ch + ('a' - 'A');
    else
        return ch;
}


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
// CTFE math
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

private
{
    template enumerateExp10(char sign, uint absexp = 1)
    {
        static if ( (sign == '+' && absexp <=  real.max_10_exp) ||
                    (sign == '-' && absexp <= -real.min_10_exp) )
            alias TypeTuple!(
                        mixin("1e" ~ sign ~ to!string(absexp) ~ "L"),
                        enumerateExp10!(sign, 2*absexp)
                    ) enumerateExp10;
        else
            alias TypeTuple!() enumerateExp10;
    }

    template enumerateExp2(char sign, uint absexp = 1)
    {
        static if ( (sign == '+' && absexp <=  real.max_exp) ||
                    (sign == '-' && absexp <= -real.min_exp) )
            alias TypeTuple!(
                        mixin("0x1p" ~ sign ~ to!string(absexp) ~ "L"),
                        enumerateExp10!(sign, 2*absexp)
                    ) enumerateExp2;
        else
            alias TypeTuple!() enumerateExp2;
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
    enum real r2 = mulexp10(-1.L, real.max_10_exp + 1);
    assert(r2 == -real.infinity);
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


