/*
 * CTFE-pure-safe string and number convertion
 */

import std.conv;
import std.traits;

import core.stdc.locale;
import core.stdc.stdint;


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
// Locale
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

enum ubyte DIGIT_GROUP_STOP = ubyte.max;

struct NumericConfig
{
    string             radixPoint;
    string             digitGroupSeparator;
    immutable(ubyte)[] digitGrouping = [ DIGIT_GROUP_STOP ];

    invariant()
    {
        assert(radixPoint.length > 0);
        assert(digitGrouping.length > 0);
        foreach (groupSize; digitGrouping)
            assert(groupSize > 0);
    }
    bool validate() const pure nothrow @safe { return true; }
        // for invoking invariant()
}

immutable NumericConfig
    NUMERIC_SI = NumericConfig(",", " ", [ 3 ]),
    NUMERIC_C  = NumericConfig("."),
    NUMERIC_D  = NumericConfig(".", "_");

alias NUMERIC_C DEFAULT_NUMERIC_CONFIG;


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
// Basics
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

immutable   DIGITS    = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
enum ubyte  MAX_BASE  = DIGITS.length;
enum        NOT_DIGIT = cast(immutable) ubyte.max;

private
{
    typedef ubyte DigitTableEntry = NOT_DIGIT;

    static immutable DigitTableEntry[0x80] DIGIT_TABLE =
    [
        '0':  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
        'A': 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23,
             24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35,
        'a': 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23,
             24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35,
    ];
}


/*
 * Determines if the character $(D ch) represents an ASCII digit in _base
 * $(D base), and returns the represented number if it's a digit.
 *
 * Note that the returned value may not be a valid number, i.e. $(D NOT_DIGIT).
 * The caller must check the returned value and use it as a number only if it
 * is not $(D NOT_DIGIT).
 *
 * Params:
 *   ch   = unicode point which possibly represents a digit
 *   base = numeric _base between 2 and 36
 *
 * Returns:
 *   $(D NOT_DIGIT) or the number represented by $(D ch).
 */
ubyte decodePossibleDigit(dchar ch, ubyte base) pure nothrow @safe
in
{
    assert(2 <= base && base <= 36);
}
out(r)
{
    assert(r < base || r == NOT_DIGIT);
}
body
{
    if (ch < 0x80)
    {
        immutable value = DIGIT_TABLE[ch];

        if (value < base)
            return value;
    }
    return NOT_DIGIT;
}

unittest
{
    assert(decodePossibleDigit('0', 10) == 0);
    assert(decodePossibleDigit('5', 10) == 5);
    assert(decodePossibleDigit('9', 10) == 9);
    assert(decodePossibleDigit('A', 10) == NOT_DIGIT);
    assert(decodePossibleDigit('a', 11) == 10);
    assert(decodePossibleDigit('D', 16) == 13);
}


/*
 * Returns $(D true) if two characters $(D a) and $(D b) are the same ASCII
 * alphabet except for their cases.
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
// Integer --> String
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

/**
 *
 */
immutable(Char)[] signedToString(Char)(
        intmax_t num, ubyte base, NumericConfig config) pure @safe
    if (isSomeChar!(Char))
{
    if (num >= 0)
        return commonIntegerToString!(Char)(num, base, config, false);
    else
        return commonIntegerToString!(Char)(-num, base, config, true);
}

/**
 *
 */
immutable(Char)[] unsignedToString(Char)(
        uintmax_t num, ubyte base, NumericConfig config) pure @safe
    if (isSomeChar!(Char))
{
    return commonIntegerToString!(Char)(num, base, config, false);
}


//----------------------------------------------------------------------------//

/*
 *
 */
immutable(Char)[] commonIntegerToString(Char)(
        uintmax_t num, ubyte base, NumericConfig config, bool negative) pure @safe
in
{
    static assert(isSomeChar!(Char));

    assert(2 <= base && base <= MAX_BASE);
    assert(config.validate);
}
body
{
    Char[] buffer;
    size_t pos;

    /* Digit grouping (generalized thousands separator). */
    immutable string  groupSep = config.digitGroupSeparator;
    immutable ubyte[] grouping = config.digitGrouping;

    bool   repeatGroup; // true to repeatedly use the last group size
    ubyte  groupSize;   // cached size of the current group
    ubyte  groupPlace;  // current place in the current group
    size_t groupIndex;  // index of the current group in $grouping

    // initial group
    groupSize  = grouping[groupIndex = 0];
    groupPlace = 0;

    /* Start the itoa work. */
    uintmax_t taper = num;

    pos = buffer.length = 6 * (4 + groupSep.length);

    do
    {
        if (pos < (2 + groupSep.length))
        {
            buffer.length *= 2;
        }

        /* Put next digit. */
        auto digit = taper % base;
        taper /= base;
        buffer[--pos] = DIGITS[cast(size_t) digit]; // ASCII char

        /* Deal with digit grouping. */
        assert(groupPlace <= groupSize);

        if (groupSize != DIGIT_GROUP_STOP && ++groupPlace == groupSize)
        {
            // write out the digit group separator
            static if (is(Char == char))
            {
                buffer[pos - groupSep.length .. pos] = groupSep;
                pos -= groupSep.length;
            }
            else
            {
                // needs UTF convertion
                foreach (i, Char ch; groupSep)
                    buffer[--pos] = ch;
            }

            // move to the next group
            if (!repeatGroup)
            {
                if (++groupIndex == grouping.length)
                    // The group list ended without STOP value.  Then we
                    // should repeatedly use the last group size for sub-
                    // sequent digit groups.
                    repeatGroup = true;
                else
                    // Get the next group size from the group list.  The
                    // 'size' may be a stop value.
                    groupSize = grouping[groupIndex];
            }

            // reset the in-group place counter
            groupPlace = 0;
        }
    }
    while (taper != 0);

    /* Deal with the negative sign. */
    if (negative)
        buffer[--pos] = '-';

    return cast(immutable) buffer[pos .. $]; // assumeUnique
}

__EOF__

immutable(Char)[] commonIntegerToString(Char)(
        uintmax_t num, ubyte base, NumericConfig config, bool negative) pure @safe
{
    mixin ReverseStringBuilder!(Char) builder;
    mixin DigitGroupWriter!(builder) groupWriter;

    builder.init();
    groupWriter.init(config);

    /* Start the itoa work. */
    uintmax_t taper = num;
    do
    {
        /*  */
        auto digit = taper % base;
        taper /= base;
        builder.put( DIGITS[cast(size_t) digit] );

        groupWriter.
    }
    while (taper != 0);

    if (negative)
        builder.put('-');

    return builder.finish();
}


//----------------------------------------------------------------------------//

template ReverseStringBuilder(Char)
{
    Char[] buffer;
    size_t at;

    void init()
    {
        buffer.length = at = 128;
    }

    void put(string str)
    {
        foreach (Char ch; str)
            buffer[--at] = ch;
    }

    void put(Char ch)
    {
        buffer[--at] = ch;
    }

    immutable(Char)[] finish()
    {
        return cast(immutable) buffer[at .. $];
    }
}


__EOF__
template ReverseStringBuilder(Char)
{
    Char[] buffer;
    size_t at;

    void init()
    {
        buffer.length = at = 128;
    }

    void put(string str)
    {
        foreach (Char ch; str)
            buffer[--at] = ch;
    }

    void put(Char ch)
    {
        buffer[--at] = ch;
    }

    immutable(Char)[] finish()
    {
        return cast(immutable) buffer[at .. $];
    }
}

template writeDigitGroup()
{
}


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
// String --> Integer
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://



//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
// Traits on real
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

// The max integer value exactly representable in real.
enum real REAL_INTEGER_MAX = mixin("0x1p" ~ to!string(real.mant_dig) ~ "L") - 1.L;

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


__EOF__
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
// Grammar
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
/++

----------------------------------------
integer<r>    =  [ sign ] unsigned<r>

unsigned<0>   =  bin | oct | hex | digits<10>
unsigned<2>   =  bin             | digits<2>
unsigned<8>   =        oct       | digits<8>
unsigned<16>  =              hex | digits<10>
unsigned<r>   =                    digits<r>

bin           =  "0b"              digits<2>
oct           =  "0"               digits<8>
hex           =  "0x"              digits<16>

digits<r>     =  digit<r> ( separator* digit<r> )*
sign          =  "+" | "-"
----------------------------------------

----------------------------------------
real<r>      =  [ sign ] ( special | normal<r> )
normal<r>    =  mantissa<r> [ exponent<r> ]

mantissa<r>  =  digits<r> point digits<r>
             |  digits<r> point
             |            point digits<r>
             |  digits<r>

exponent<r>  =  ten-exp | two-exp
ten-exp      =  ( "d" | "e" ) [ sign ] digit<10>+   // 10^exp
two-exp      =          "p"   [ sign ] digit<10>+   //  2^exp

special      =  "inf"
             |  "infinity"
             |  "nan"
             |  "nan(" [ n-char-sequence ] ")"
----------------------------------------

