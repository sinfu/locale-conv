module conv.floatingpoint;

import conv.common;
import conv.integral;


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
// CTFE - Floating Point to String
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://



//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
// CTFE - Converting String to Floating Point
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

Scanned!real scanReal(string input_, ubyte base) pure @safe
in
{
    assert(2 <= base && base <= MAX_BASE);
}
body
{
    immutable preMulLimit = limit / base;
    string    input       = input_;
    size_t    count;

    while (input.length != 0)
    {
        // Read in a digit
        immutable maybeDigit = decodePossibleDigit(input[0], base);

        if (maybeDigit == NOT_DIGIT)
            break; // end of the token
        immutable digit = maybeDigit;

        // Accumulate
        if (result > preMulLimit - digit)
        {
            overflow = true;
            break;
        }
        result *= cast(const Num) base;
        result += cast(const Num) digit;
        ++count;

        // Consume the digit character and delimiters (if any)
        input = input[1 .. $].consumeChars(DELIM);
    }

    return ScanDigitsStatus!Num(result, input, overflow, count);
}






/*
 * input        =  [ "+" | "-" ] ( bin | hex | dec )
 *
 * bin          =  "0b" mantissa<2>  bin-exp?
 * hex          =  "0x" mantissa<16> bin-exp?
 * dec          =       mantissa<10> dec-exp?
 *
 * mantissa<r>  =  digits<r> "." digits<r>
 *              |            "." digits<r>
 *              |  digits<r> "."?
 *
 * dec-exp      =  "e" [ "+" | "-" ] digits<10>
 * bin-exp      =  "p" [ "+" | "-" ] digits<10>
 */
Scanned!real scan_real(string str) pure @safe
in
{
    assert(str.length > 0);
}
body
{
    string input = str;

    /*
     * Determine the sign.
     */
    bool signed;

    if (input.length) switch (input[0])
    {
        case '+': signed = false; input = input[1 .. $]; break;
        case '-': signed =  true; input = input[1 .. $]; break;
        default : break;
    }

    if (input.length == 0)
        throw new Exception("No sufficient input");

    /*
     * Determine the radix to use in convertion.  If it's not supplied by
     * the caller, 
     */
    immutable ubyte radix = 10;//(base == 0 ? detectRadix(input) : base);

    /+
    if (radix == 2)
    {
        input = input.withoutNumeralPrefix('B');
        input = input.consumeChars(DELIM);
    }
    else if (radix == 16)
    {
        input = input.withoutNumeralPrefix('X');
        input = input.consumeChars(DELIM);
    }
    +/

    /*
     * Read in
     */
    mant_t mantHigh, mantLow;
    int    highExp, fixExp;

    foreach (i, ch; input)
    {
        switch (ch)
        {
            case '.':
                continue;
            case 'e', 'E':
                break;
        }

        if (input[0] == '.')
        {
            dotpos = ndigits;
        }
        else
        {
            // Read in a digit
            immutable maybeDigit = decodePossibleDigit(input[0], radix);

            if (maybeDigit == NOT_DIGIT)
                break; // end of the token

            digits[ndigits++] = input[0];
        }

        //
        input = consumeChars(input[1 .. $], DELIM);
    }
    while (input.length != 0);

    immutable int dotexp =
        dotpos == size_t.max ? 0 : dotpos - ndigits;

    // exponent

    int exp;

    if (input.length) switch (input[0])
    {
        case 'e', 'E':
            input = input[1 .. $];
            exp = scan_integer!int(input, 10);
            break;

        case 'p', 'P':
            input = input[1 .. $];
            exp = scan_integer!int(input, 10);
            break;

        default:
            break;
    }

    exp += dotexp;

    //
    real uresult = 1;

    uresult = scan_integer!ulong(digits[0 .. ndigits].idup);

    //
    uresult *= pow10(exp);

    immutable result = (signed ? -uresult : uresult);
    return Scanned!real(result, input);
}


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

enum real REAL_INTEGER_MAX = mixin("0x1p" ~ utoa(real.mant_dig) ~ "L") - 1.L;

static if (real.mant_dig <= 8*ulong.sizeof)
{
    alias ulong mant_t;
    enum mant_t MANT_MAX = cast(ulong) REAL_INTEGER_MAX;
}
else
{
    // real can hold up to mant_dig-bit integer value
    alias real mant_t;
    enum mant_t MANT_MAX = REAL_INTEGER_MAX;
}


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
        negativeExp10 = [ enumerateExp10!('-') ], // 10^(-2^i)
        positiveExp2  = [  enumerateExp2!('+') ], //  2^( 2^i)
        negativeExp2  = [  enumerateExp2!('-') ]; //  2^(-2^i)
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


