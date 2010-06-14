
import std.algorithm;
import std.array;
import std.contracts;
import std.conv;
import std.range;
import std.traits;
import std.typetuple;

import core.stdc.locale;
import core.stdc.stdint;

void main()
{
}


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
// Locale
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

enum ubyte DIGIT_GROUP_STOP = ubyte.max;

struct NumericLocale
{
    string             radixPoint;
    string             digitGroupSeparator;
    immutable(ubyte)[] digitGrouping;
}

immutable NumericLocale
    NUMERIC_SI = NumericLocale(",", " ", [ 3 ]),
    NUMERIC_C  = NumericLocale("."),
    NUMERIC_D  = NumericLocale(".", "_");


NumericLocale currentNumericLocale()
{
    NumericLocale numeric;
    auto lconv = localeconv();

    numeric.radixPoint          = mbsToUTF8(lconv.decimal_point);
    numeric.digitGroupSeparator = mbsToUTF8(lconv.thousands_sep);
    numeric.digitGrouping       = parseDigitGrouping(lconv.grouping);

    return numeric;
}

private immutable(ubyte)[] parseDigitGrouping(in char* grouping)
{
    ubyte[16] result;
    size_t    used;

    while (*grouping != 0)
    {
        ubyte pos = *grouping;

        if (pos >= byte.max)
            pos = DIGIT_GROUP_STOP;
        result[used++] = pos;
    }
    return result[0 .. used].idup;
}

private string mbsToUTF8(in char* mbs)
{
    return to!string(mbs); // TODO
}


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


bool sameAsciiAlpha(dchar a, dchar b) pure nothrow @safe
{
    return toAsciiLower(a) == toAsciiLower(b);
}

dchar toAsciiLower(dchar ch) pure nothrow @safe
{
    if ('A' <= ch && ch <= 'Z')
        return ch + ('a' - 'A');
    else
        return ch;
}


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
// Common scanners
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

class ScanException : Exception
{
    this(string msg)
    {
        super(msg);
    }
}

class OverflowException : Exception
{
    this(string msg)
    {
        super(msg);
    }
}


/**
 * Returns $(D true) if $(D R) is an input range with character elements.
 */
template isTextInput(R)
{
    enum bool isTextInput =
        isInputRange!(R) && isSomeChar!(ElementType!R);
}

template isBacktrackableString(R)
{
    enum bool isBacktrackableString =
        isForwardRange!(R) && isTextInput!(R);
}


//----------------------------------------------------------------------------//

/**
 * Scans a numeral sign at the beginning of $(D input) if any.
 *
 * Params:
 *   input = nonempty text input range.
 *
 * Returns:
 *   $(D true) if $(D input) starts with a negative sign, or $(D false)
 *   otherwise.
 */
bool scanSign(Input)(ref Input input)
    if (isTextInput!(Input))
in
{
    assert(!input.empty);
}
body
{
    bool signed;

    switch (input.front)
    {
        case '+': signed = false; input.popFront; break;
        case '-': signed =  true; input.popFront; break;
        default : break;
    }
    return signed;
}

unittest
{
    string s;

    // negative sign
    s = "-1234";
    assert(scanSign(s) == true);
    assert(s == "1234");
    // positive sign
    s = "+1234";
    assert(scanSign(s) == false);
    assert(s == "1234");
    // no sign
    s = "1234";
    assert(scanSign(s) == false);
    assert(s == "1234");
    // excess signs
    s = "-+1234";
    assert(scanSign(s) == true);
    assert(s == "+1234");
    // CTFE
    enum sgn = { string s = "-0";
                 auto r = scanSign(s);
                 assert(s == "0");
                 return r; }();
    assert(sgn == true);
}


/**
 * Scans a base prefix at the beginning of $(D input) if any, and returns the
 * base of the represented number.
 *
 * Params:
 *   input  = nonempty forward range with character elements.
 *   assump = specify an assumed base of the number if it's known, or specify
 *            zero to determine the base by reading a possible base prefix.
 *
 * Returns:
 *   $(D assump) if $(D assump) is nonzero, or returns a determined base.
 */
ubyte scanBase(Input)(ref Input input, ubyte assump = 0)
    if (isTextInput!(Input) && isForwardRange!(Input))
in
{
    assert(!input.empty);
    assert(assump == 0 || (2 <= assump && assump <= MAX_BASE));
}
out(r)
{
    assert(2 <= r && r <= MAX_BASE);
}
body
{
    auto  save = input.save;
    ubyte base = (assump == 0) ? 10 : assump;

    if (skipOver(input, '0') && !input.empty)
    {
        size_t skip;

        switch (input.front)
        {
            case 'b', 'B': base =  2; ++skip; break;
            case 'x', 'X': base = 16; ++skip; break;

            default:
                if (decodePossibleDigit(input.front, 8) != NOT_DIGIT)
                    base = 8;
                break;
        }

        if (assump == 0 || base == assump)
        {
            // consume the base prefix if the base was unknown or the
            // base prefix is the assumed one
            popFrontN(input, skip);
        }
        else
        {
            // otherwise, revert everything
            input = save;
            base  = assump;
        }
    }
    else
    {
        // restore the '0' which we might skip above
        input = save;
    }
    return base;
}

unittest
{
    string s;

    /* without assumption */
    {
        // binary
        s = "0b11011";
        assert(scanBase(s) == 2);
        assert(s == "11011");
        // hexadecimal
        s = "0xfeed";
        assert(scanBase(s) == 16);
        assert(s == "feed");
        // octal
        s = "01644";
        assert(scanBase(s) == 8);
        assert(s == "1644");
        // decimal
        s = "987";
        assert(scanBase(s) == 10);
        assert(s == "987");
        // prefer decimal for zero
        s = "0";
        assert(scanBase(s) == 10);
        assert(s == "0");
    }
    /* with matched assumption */
    {
        // binary
        s = "0b11011";
        assert(scanBase(s, cast(ubyte) 2) == 2);
        assert(s == "11011");
        // hexadecimal
        s = "0xfeed";
        assert(scanBase(s, cast(ubyte) 16) == 16);
        assert(s == "feed");
        // octal
        s = "01644";
        assert(scanBase(s, cast(ubyte) 8) == 8);
        assert(s == "1644");
    }
    /* with unmatched assumption */
    {
        s = "0b11011";
        assert(scanBase(s, cast(ubyte) 16) == 16);
        assert(s == "0b11011");
    }
    /* CTFE */
    {
        enum r = { string s = "0xbebadadaba";
                   auto base = scanBase(s);
                   assert(s == "bebadadaba");
                   return base; }();
        assert(r == 16);
    }
}


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
// Integer
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

/**
 * Scans an integer value at the beginning of $(D input).
 *
 * Params:
 *   input   = nonempty forward range with character elements.
 *   base    = numeral _base of the represented integer.  If this parameter
 *             is zero, the _base is detected by reading a _base prefix from
 *             $(D input).  Decimal is used if no _base prefix is found.
 *   numeric = $(D NumericLocale) object specifying a digit group
 *             separator to allow in $(D input).
 *
 * Throws:
 * $(UL
 *   $(LI Enforcement fails if $(D input) is empty.)
 *   $(LI Enforcement fails if $(D base) is not a valid _base.)
 *   $(LI $(D ScanException) if $(D input) does not starts with any integer.)
 *   $(LI $(D OverflowException) on an integer overflow.  Then $(D input)
 *        points to the exact digit where the integer overflow occured.)
 * )
 */
Integer scanInteger(Integer = intmax_t, Input)(
        ref Input input, ubyte base = 0,
        NumericLocale numeric = NUMERIC_C)
    if (isTextInput!(Input) && isForwardRange!(Input))
{
    enforce(!input.empty,
            "empty input is passed to scanInteger()");
    enforce(base == 0 || (2 <= base && base <= MAX_BASE),
            "invalid base is passed to scanInteger()");

    // just to shorten the identifier
    immutable groupSep = numeric.digitGroupSeparator;

    /* Scan preceding sign and base prefix if any. */
    bool negate;

    negate = scanSign(input);
    if (input.empty)
        throw new ScanException("input contains no integer; only a sign exists");

    base = scanBase(input, base);
    if (input.empty)
        throw new ScanException("input contains only a base prefix");

    /* We will parse subsequent digits as an absolute value. */
    Unsigned!Integer uresult = 0;

    // absolute negative limit is one larger than positive one
    immutable absLimit = cast(Unsigned!Integer) Integer.max +
            ((isSigned!(Integer) && negate) ? 1u : 0u);

    if (decodePossibleDigit(input.front, base) == NOT_DIGIT)
        throw new ScanException("input contains no digit");
    do
    {
        // check if the front character is a digit
        immutable maybeDigit = decodePossibleDigit(input.front, base);
        if (maybeDigit == NOT_DIGIT)
            break;
        immutable digit = maybeDigit;

        auto tmp = uresult;
        tmp *= base;
        tmp += digit;
        if (tmp < uresult || tmp > absLimit)
            throw new OverflowException("integer oveflow on parsing integer");
        uresult = tmp;

        // consume the digit and subsequent separators if any
        input.popFront;
        if (groupSep.length)
            while (skipOver(input, groupSep)) continue;
    }
    while (!input.empty);

    assert(uresult <= absLimit);
    return negate ? -uresult : uresult;
}

unittest
{
    string s;

    /* default */
    {
        s = "123456789"; // decimal
        assert(scanInteger(s) == 123456789 && s.empty);
        s = "0xface2face"; // hexadecimal
        assert(scanInteger(s) == 0xface2face && s.empty);
        s = "0b11011"; // binary
        assert(scanInteger(s) == 0b11011 && s.empty);
        s = "0644"; // octal
        assert(scanInteger(s) == 0644 && s.empty);
        s = "-100"; // sign (-)
        assert(scanInteger(s) == -100 && s.empty);
        s = "+100"; // sign (+)
        assert(scanInteger(s) == 100 && s.empty);
    }
    /* garbage at the end */
    {
        s = "1234LL";
        assert(scanInteger(s) == 1234 && s == "LL");
        s = "1234ab";
        assert(scanInteger(s) == 1234 && s == "ab");
        s = "0xbeefstake";
        assert(scanInteger!ushort(s) == 0xbeef && s == "stake");
    }
    /* specifying base */
    {
        s = "cabada";
        assert(scanInteger(s, cast(ubyte) 16) == 0xcabada && s.empty);
        s = "0xcabada";
        assert(scanInteger(s, cast(ubyte) 10) == 0 && s == "xcabada");
    }
    /* smaller integer types */
    {
        s = "30";
        assert(scanInteger!byte(s) == 30);
        s = "40";
        assert(scanInteger!ubyte(s) == 40);
    }
    /* negative sign to unsigned type */
    {
        s = "-64";
        assert(scanInteger!ubyte(s) == 192);
        s = "-1";
        assert(scanInteger!uintmax_t(s) == uintmax_t.max);
    }
    /* overflow */
    {
        // unsigned
        s = "65535";
        assert(scanInteger!ushort(s) == 65535);
        try
        {
            s = "65536";
            scanInteger!ushort(s);
            assert(0);
        }
        catch (OverflowException e) {}
        // signed
        s = "32767";
        assert(scanInteger!short(s) == 32767);
        s = "-32768";
        assert(scanInteger!short(s) == -32768);
        try
        {
            s = "32768";
            scanInteger!short(s);
            assert(0);
        }
        catch (OverflowException e) {}
    }
    /* custom locale */
    {
        auto myLocale = NumericLocale(".", "::");
        s = "123::456::789.0";
        assert(scanInteger(s, cast(ubyte) 10, myLocale) == 123456789);
        assert(s == ".0");
    }
    /* CTFE */
    {
        enum n = { string s = "-32768#";
                   auto r = scanInteger!short(s);
                   assert(s == "#");
                   return r; }();
        assert(n == -32768);
    }
}


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
// Floating point number
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

/**
 * Scans a floating-point number at the beginning of $(D input).
 *
 * Params:
 *   input   = nonempty forward range with character elements.
 *   base    = numeral _base of the represented floating-point number.  If this
 *             parameter is zero, the _base is detected by reading a _base
 *             prefix from $(D input).  Decimal is used if no _base prefix is
 *             found.
 *   numeric = $(D NumericLocale) object specifying a radix point and a digit
 *             group separator used in $(D input).
 *
 * Throws:
 * $(UL
 *   $(LI Enforcement fails if $(D input) is empty.)
 *   $(LI Enforcement fails if $(D base) is not a valid _base.)
 *   $(LI $(D ScanException) if $(D input) does not starts with any number.)
 * )
 */
real scanReal(Input)(ref Input input, ubyte base = 0,
        NumericLocale numeric = NUMERIC_C)
{
    enforce(!input.empty,
            "Attempted to scan a floating point number from an empty source");
    enforce(base == 0 || (2 <= base && base <= MAX_BASE),
            "Invalid numeral base is specified for scanning a floating point number");

    /* Determine the sign of the represented number. */
    bool negate = scanSign(input);
    if (input.empty)
        throw new ScanException("input contains no floating point number; only a sign exists");

    /* Special or normal? */
    real result;

    if (skipOver!sameAsciiAlpha(input, "inf"))
    {
        result = real.infinity;
        skipOver!sameAsciiAlpha(input, "inity"); // inf'inity
    }
    else if (skipOver!sameAsciiAlpha(input, "nan"))
    {
        result = real.nan;

        // just skip n-char-sequence (TODO)
        if (input.startsWith('('))
        {
            auto save = input.save;
            if (! skipOver!("a != b")(input, ')'))
                input = save;
        }
    }
    else /* normal number */
    {
        result = scanNormalReal(input, base, numeric);
    }

    return negate ? -result : result;
}


/*
 *
 */
private real scanNormalReal(Input)(ref Input input, ubyte base, NumericLocale numeric)
out(r)
{
    assert(r == r); // must not be NaN
}
body
{
    enforce(!input.empty,
            "empty");

    /* Determine the base of the represented number. */
    base = scanBase(input, base);
    if (input.empty)
        throw new ScanException("input contains only a base prefix");

    if (base == 8)
    {
        // scanBase() recognized an octal prefix 0... But octal should
        // not be an expected base for floating point numbers.
        base = 10;
    }

    /* Scan the mantissa part. */
    real mant;
    int  mantExp;

    auto scmant = scanMantissa(input, base, numeric);
    mant    = scmant.mantissa;
    mantExp = scmant.scale;

    /* Scan the exponent part if any. */
    ubyte expBase = base;
    int   exp;

    if (!input.empty) switch (input.front)
    {
        case 'D', 'd': // Microsoft
        case 'E', 'e':
            input.popFront;
            exp = scanInteger!int(input, expBase = 10);
            break;

        case 'P', 'p':
            input.popFront;
            exp = scanInteger!int(input, expBase = 2);
            break;

        default: // No exponent
            break;
    }

    // scale factor for the mantissa
    if (base == expBase)
        exp += mantExp;
    else
        mant = mulexp(mant, base, mantExp);

    /* Compose the result. */
    return mulexp(mant, expBase, exp);
}


/*
 * Scans a floating-point number without sign nor exponent.
 */
private struct ScanMantissaResult
{
    real mantissa;
    int  scale;
}
private ScanMantissaResult scanMantissa(Input)(
        ref Input input, ubyte base, NumericLocale numeric)
    if (isBacktrackableString!(Input))
{
    immutable point    = numeric.radixPoint;
    immutable groupSep = numeric.digitGroupSeparator;

    // For overflow cheking
    immutable mantLimit = (MANT_MAX - (base - 1)) / base;

    /*
     * Read in mantissa.  We will read higher digits in mant in the exact
     * form (i.e. integer), and then read lower digits in aux.  aux can be
     * zero if the represented mantissa is short enough to fit in mant.
     *
     * The mantissa is read as the following form regardless of the original
     * representation.
     *                    mant . aux * base^mantExp
     */
    mant_t mant = 0;    // Exact representation of the MSBs
    mant_t aux  = 0;    // For correcting the LSBs
    int    auxExp;

    size_t count;       // The number of digits
    bool   inFrac;      // Are we reading digits past a decimal point?
    int    mantExp;

L_scanDigits:
    while (!input.empty)
    {
        if (startsWith(input, point))
        {
            if (inFrac)
                // The decimal point appeared twice, which never happens
                // in correct floating point numbers.  End of the token.
                // The caller may raise an error.
                /+break L_scanDigits;+/ goto L_scanDigits_end;
            inFrac = true;
            skipOver(input, point);
            continue;
        }

        // Read in a digit.
        immutable maybeDigit = decodePossibleDigit(input.front, base);
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
                ++mantExp;
        }
        else
        {
            mant = mant * base + digit;
            if (inFrac)
                --mantExp;
        }

        // consume the digit and subsequent separators if any
        input.popFront;
        if (groupSep.length)
            while (skipOver(input, groupSep)) continue;
    }
L_scanDigits_end: // [workaround] CTFE does not support labeled break

    if (count == 0)
        throw new ScanException("No digit");

    /*
     * Combine mant and aux.  We don't scale the result here for reducing LSB
     * errors.
     */
    mant += mulexp(aux, base, auxExp);
    return ScanMantissaResult(mant, mantExp);
}


//----------------------------------------------------------------------------//
// Traits on real

// The max integer value exactly represented in a real.
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


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
// Simplified interface
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

Number toNumber(Number, String)(String numstr, ubyte base = 0)
    if (isIntegral!(Number) && isBacktrackableString!(String))
{
    return scanInteger!Integer(text, base, NUMERIC_C);
}

Number toNumber(Number, String)(String numstr, ubyte base = 0)
    if (isFloatingPoint!(Number) && isBacktrackableString!(String))
{
    return scanReal(text, base, NUMERIC_C);
}


__EOF__
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
// Grammer
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

++/
