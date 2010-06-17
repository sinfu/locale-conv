
import std.conv : ConvError;
import std.math;
import std.stdint;
import std.stdio;
import std.traits;
import std.typetuple;

extern(C) int printf(immutable char*, ...) pure nothrow @safe; // fake

void main()
{
    /+
    auto eps = to!real("1.0842021724855044340E-19");
    writefln("%a", eps);
    writefln("%a", 1.0842021724855044340E-19L);
    writefln("%a", real.epsilon);

    auto min = to!real("3.3621031431120935063E-4932");
    writefln("%a", min);
    writefln("%a", 3.3621031431120935063E-4932L);
    writefln("%a", real.min_normal);

    auto max = to!real("1.1897314953572317650E+4932");
    writefln("%a", max);
    writefln("%a", 1.1897314953572317650E+4932L);
    writefln("%a", real.max);
    +/
}


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
// std.conv compatible interface
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

/*
 * String to integer.
 */
Target to(Target, Source)(Source s) pure @safe
    if (isSomeImmutableString!(Source) && isIntegral!(Target))
{
    auto r = parseIntegral!(Target, Source)(s, 0);
    if (r.rest.length > 0)
        throw new ConvError("source string contains non-digit characters at the end");
    return r.value;
}

Target to(Target, Source)(Source s, uint radix) pure @safe
    if (isSomeImmutableString!(Source) && isIntegral!(Target))
{
    if (radix < 2 || MAX_RADIX < radix)
        throw new ConvError(to!string(radix) ~ " is illegal for integral radix");
    auto r = parseIntegral!(Target, Source)(s, radix);
    if (r.rest.length > 0)
        throw new ConvError("source string contains non-digit characters at the end");
    return r.value;
}


/*
 * Integer to string.
 */
Target to(Target, Source)(Source s) pure @safe
    if (isIntegral!(Source) && isSomeString!(Target))
{
    return to!Target(s, 10u);
}

Target to(Target, Source)(Source s, uint radix) pure @safe
    if (isIntegral!(Source) && isSomeString!(Target))
{
    if (radix < 2 || MAX_RADIX < radix)
        throw new ConvError(to!string(radix) ~ " is illegal for integral radix");

    if (isSigned!(Source) && radix == 10)
        return signedIntegerToString!Target(s);
    else
        return unsignedIntegerToString!Target(s, radix);
}


/*
 * String to floating-point number.
 */
Target to(Target, Source)(Source s) pure @safe
    if (isSomeImmutableString!(Source) && isFloatingPoint!(Target))
{
    auto r = parseReal!(Source)(s, 0);
    if (r.rest.length > 0)
        throw new ConvError("source string contains non-digit characters at the end");
    return r.value;
}

Target to(Target, Source)(Source s, uint radix) pure @safe
    if (isSomeImmutableString!(Source) && isFloatingPoint!(Target))
{
    if (radix != 10 && radix != 16)
        throw new ConvError(to!string(radix) ~ " is illegal for radix of"
                ~ " a floating point number");
    auto r = parseReal!(Source)(s, radix);
    if (r.rest.length > 0)
        throw new ConvError("source string contains non-digit characters at the end");
    return r.value;
}


/*
 * Floating-point number to string.
 */
Target to(Target, Source)(Source s) pure @safe
    if (isFloatingPoint!(Source) && isSomeString!(Target))
{
    return to!(Target, Source)(s, 10u);
}

Target to(Target, Source)(Source s, uint radix) pure @safe
    if (isFloatingPoint!(Source) && isSomeString!(Target))
{
    if (radix != 10 && radix != 16)
        throw new ConvError(to!string(radix) ~ " is illegal for radix of"
                ~ " a floating point number");
    return realToString!Target(s, radix);
}


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
// parser utilities
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

// workaround @@@BUG4177@@@
extern immutable bool __ctfe;

/*
 * Returns true if T is some string type with immutable elements.
 */
template isSomeImmutableString(T)
{
    static if (isSomeString!(T))
        enum bool isSomeImmutableString = is(typeof(T.init[0]) == immutable);
    else
        enum bool isSomeImmutableString = false;
}

/*
 * Returns T without head const/immutable.
 */
template Mutable(T)
{
         static if (is(T U ==     const U)) alias U Mutable;
    else static if (is(T U == immutable U)) alias U Mutable;
    else                                    alias T Mutable;
}

/*
 * Maximum allowed numeral radix.
 */
enum uint MAX_RADIX = 36;


/*
 * For 'pure' parsers to return result with the rest of a source string.
 */
struct Parsed(String, T)
{
    String rest;
    T      value;
}

// for returning multiple results
struct Parsed(String, T...)
{
    String rest;
    T      values;
}


/*
 * pure, CTFE-safe, case-insensitive startsWith for immutable strings
 */
bool pure_istartsWith(Char1, Char2)(
        immutable(Char1)[] str, immutable(Char2)[] prefix) pure @safe
    if (isSomeChar!(Char1) && isSomeChar!(Char2))
{
    if (str.length >= prefix.length)
    {
        foreach (i, Char1 pch; prefix)
        {
            Mutable!Char1 ch1 = str[i];
            Mutable!Char1 ch2 = pch;

            if ('A' <= ch1 && ch1 <= 'Z') ch1 += 'a' - 'A';
            if ('A' <= ch2 && ch2 <= 'Z') ch2 += 'a' - 'A';

            if (ch1 != ch2)
                return false;
        }
        return true;
    }
    return false;
}

unittest
{
    assert(pure_istartsWith("abcdef", "abc"c));
    assert(pure_istartsWith("abcdef", "Abc"w));
    assert(pure_istartsWith("abcdef", "aBC"d));
    assert(!pure_istartsWith("abcdef", "def"c));
    assert(!pure_istartsWith("abcdef", "Def"w));
    assert(!pure_istartsWith("abcdef", "dEF"d));
    assert(!pure_istartsWith("abc", "abcdef"));
}


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
// string to integer
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

/*
 * Parses an integer at the beggining of $(D source) represented in the
 * specified radix.
 *
 * Params:
 *   source = string which starts with a validly formatted integral number.
 *   radix_ = specify the radix of the number, or zero to determine it by
 *            reading a radix prefix if any.
 */
Parsed!(Source, Integer) parseIntegral(Integer, Source)(
        Source source, uint radix_ = 0 ) pure @safe
    if (isSomeImmutableString!(Source) && isIntegral!(Integer))
in
{
    assert(radix_ == 0 || (2 <= radix_ && radix_ <= MAX_RADIX));
}
body
{
    alias typeof(Source.init[0]) Char;
    Char[] rest = source;

    if (rest.length == 0)
        throw new ConvError("source string is empty");

    /*
     * Scan a sign if any.  We allow a negative sign for unsigned and/or
     * non-decimal source (be liberal in parsing input).
     */
    bool negate;

    negate = (rest[0] == '-');
    if (negate || rest[0] == '+')
        rest = rest[1 .. $];

    if (rest.length == 0)
        throw new ConvError("source string represents no integer, only a sign");

    /*
     * Detect the radix by scanning a radix prefix if it's not specified.
     */
    uint radix = (radix_ == 0 ? 10 : radix_);

    if (radix_ == 0 && rest.length >= 2 && rest[0] == '0')
    {
        size_t skip;

        switch (rest[1])
        {
            case 'X', 'x': radix = 16; skip = 2; break;
            case 'B', 'b': radix =  2; skip = 2; break;
            default: break;
        }
        if (rest.length == skip)
            throw new ConvError("source string represents no integer," ~
                    " only a radix prefix");
        rest = rest[skip .. $];
    }
    assert(2 <= radix && radix <= MAX_RADIX);
    assert(rest.length > 0);

    /*
     * Scan the represented integer as an absolute value.
     */
    Unsigned!Integer uresult = 0;
    uint ndigits;

    immutable absLimit = cast(Unsigned!Integer) Integer.max
        + ((isSigned!(Integer) && negate) ? 1u : 0u);

    for (; rest.length > 0; rest = rest[1 .. $])
    {
        Char ch = rest[0];
        uint digit = MAX_RADIX;

        if ('0' <= ch && ch <= '9')
            digit = ch - '0';
        else if (ch >= 'a')
            digit = ch - 'a' + 10;
        else if (ch >= 'A')
            digit = ch - 'A' + 10;

        if (digit >= radix)
            break; // not a digit in the specified radix; end

        // accumulate the digit in uresult after checking overflow
        ++ndigits;

        auto tmp = uresult;
        tmp *= cast(Integer) radix;
        tmp += cast(Integer) digit;
        if (tmp < uresult || (isSigned!(Integer) && tmp > absLimit))
            throw new ConvError("integer overflow parsing an integer of type "
                    ~ Integer.stringof);
        uresult = tmp;
    }
    assert(uresult <= absLimit);

    if (ndigits == 0)
        throw new ConvError("source string does not contain any digit");

    return Parsed!(Source, Integer)(rest, negate ? -uresult : uresult);
}

unittest
{
    // basic unsigned cases
    {
        auto dec = parseIntegral!uint("123456789", 10);
        assert(dec.value == 123456789);
        assert(dec.rest == "");

        auto bin = parseIntegral!uint("1001011010100101", 2);
        assert(bin.value == 0b1001011010100101);
        assert(bin.rest == "");

        auto hex = parseIntegral!uint("DeAdBeEf", 16);
        assert(hex.value == 0xdeadbeef);
        assert(hex.rest == "");

        auto zzz = parseIntegral!uint("Za80Fx", 36);
        assert(zzz.value == 36*(36*(36*(36*(36*35 + 10) + 8) + 0) + 15) + 33);
        assert(zzz.rest == "");

        auto max = parseIntegral!uint("4294967295", 10);
        assert(max.value == uint.max);
        assert(max.rest == "");

        auto nil = parseIntegral!uint("0", 10);
        assert(nil.value == 0);
        assert(nil.rest == "");

        auto sgn = parseIntegral!uint("+65536", 10);
        assert(sgn.value == 65536);
        assert(sgn.rest == "");
    }
    // basic signed cases
    {
        auto dec = parseIntegral!int("-123456789", 10);
        assert(dec.value == -123456789);
        assert(dec.rest == "");

        auto hex = parseIntegral!int("-abc", 16);
        assert(hex.value == -0xabc);
        assert(hex.rest == "");

        auto max = parseIntegral!int("2147483647", 10);
        assert(max.value == int.max);
        assert(max.rest == "");

        auto min = parseIntegral!int("-2147483648", 10);
        assert(min.value == int.min);
        assert(min.rest == "");

        auto uns = parseIntegral!uint("-1", 10);
        assert(uns.value == uint.max);
        assert(uns.rest == "");
    }
    // smaller integer types
    {
        auto u8 = parseIntegral!ubyte("255", 10);
        assert(u8.value == 255);
        assert(u8.rest == "");

        auto s8max = parseIntegral!byte("127", 10);
        assert(s8max.value == 127);
        assert(s8max.rest == "");

        auto s8min = parseIntegral!byte("-128", 10);
        assert(s8min.value == -128);
        assert(s8min.rest == "");

        auto u16 = parseIntegral!ushort("65535", 10);
        assert(u16.value == 65535);
        assert(u16.rest == "");

        auto s16max = parseIntegral!short("32767", 10);
        assert(s16max.value == 32767);
        assert(s16max.rest == "");

        auto s16min = parseIntegral!short("-32768", 10);
        assert(s16min.value == -32768);
        assert(s16min.rest == "");
    }
    // larger integer types
    {
        auto u64max = parseIntegral!ulong("18446744073709551615", 10);
        assert(u64max.value == ulong.max);
        assert(u64max.rest == "");

        auto s64max = parseIntegral!long("+9223372036854775807", 10);
        assert(s64max.value == long.max);
        assert(u64max.rest == "");

        auto s64min = parseIntegral!long("-9223372036854775808", 10);
        assert(s64min.value == long.min);
        assert(u64max.rest == "");
    }
    // non-digit at the end
    {
        auto r1 = parseIntegral!int("9876###", 10);
        assert(r1.value == 9876);
        assert(r1.rest == "###");

        auto r2 = parseIntegral!int("9876\uff10", 10);
        assert(r2.value == 9876);
        assert(r2.rest == "\uff10");

        auto r3 = parseIntegral!int("123", 2);
        assert(r3.value == 1);
        assert(r3.rest == "23");
    }
    // detecting radix
    {
        auto dec = parseIntegral!int("1234abc");
        assert(dec.value == 1234);
        assert(dec.rest == "abc");

        auto bin = parseIntegral!int("0b1101234");
        assert(bin.value == 0b1101);
        assert(bin.rest == "234");

        auto hex = parseIntegral!int("0x08fAGhi");
        assert(hex.value == 0x08fa);
        assert(hex.rest == "Ghi");
    }
    // various string types
    {
        auto ws = parseIntegral!uint("321\uff10"w);
        assert(ws.value == 321);
        assert(ws.rest == "\uff10");

        auto ds = parseIntegral!uint("321\uff10"d);
        assert(ds.value == 321);
        assert(ds.rest == "\uff10");
    }
    // error (no digit)
    {
        try { parseIntegral!uint(""); assert(0); } catch (ConvError e) {}
        try { parseIntegral!uint("+"); assert(0); } catch (ConvError e) {}
        try { parseIntegral!uint("-"); assert(0); } catch (ConvError e) {}
        try { parseIntegral!uint("0x"); assert(0); } catch (ConvError e) {}
        try { parseIntegral!uint("-0x"); assert(0); } catch (ConvError e) {}
        try { parseIntegral!uint("\uff10"); assert(0); } catch (ConvError e) {}
    }
    // error (overflow)
    {
        try { parseIntegral!ushort("65536"); assert(0); } catch (ConvError e) {}
        try { parseIntegral!short("32768"); assert(0); } catch (ConvError e) {}
        try { parseIntegral!short("-32769"); assert(0); } catch (ConvError e) {}
        try { parseIntegral!ubyte("0x100"); assert(0); } catch (ConvError e) {}
        try { parseIntegral!byte("0x80"); assert(0); } catch (ConvError e) {}
        try { parseIntegral!byte("-0x81"); assert(0); } catch (ConvError e) {}
    }
    // CTFE
    {
        enum r = parseIntegral!short("-32768#");
        assert(r.value == -32768);
        assert(r.rest == "#");
    }
}


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
// integer to string
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

/*
 * Converts unsigned integer $(D num) to a string of type $(D String) in the
 * specified radix.
 */
String unsignedIntegerToString(String)(uintmax_t num, uint radix) pure @safe
    if (isSomeString!(String))
in
{
    assert(2 <= radix && radix <= MAX_RADIX);
}
body
{
    alias typeof(String.init[0]) Char;

    static if (is(Char == immutable))
    {
        static immutable Char[MAX_RADIX] DIGITS
            = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        if (num < radix)
            // optimize for tiny values
            return DIGITS[cast(const size_t) num .. cast(const size_t) num + 1];
    }

    Mutable!Char[uintmax_t.sizeof * 8] buffer = void;
    size_t    pos = buffer.length;
    uintmax_t cur = num;

    do
    {
        auto dig = cast(ubyte) (cur % radix);
        cur /= radix;
        buffer[--pos] = cast(Char) (dig < 10 ? '0' + dig : 'A' + dig - 10);
    }
    while (cur != 0);

    if (num < 0)
        buffer[--pos] = '-';

    static if (is(Char == immutable))
        return buffer[pos .. $].idup;
    else
        return buffer[pos .. $].dup;
}

unittest
{
    {
        string s;

        s = unsignedIntegerToString!string(123454321, 10);
        assert(s == "123454321");
        s = unsignedIntegerToString!string(0b1001011010100101, 2);
        assert(s == "1001011010100101");
        s = unsignedIntegerToString!string(0xbeef, 16);
        assert(s == "BEEF");
        s = unsignedIntegerToString!string(2133486141, 36);
        assert(s == "ZA80FX");

        s = unsignedIntegerToString!string(0, 10);
        assert(s == "0");
        s = unsignedIntegerToString!string(ulong.max, 10);
        assert(s == "18446744073709551615");
        s = unsignedIntegerToString!string(ulong.max, 16);
        assert(s == "FFFFFFFFFFFFFFFF");
        s = unsignedIntegerToString!string(long.max, 10);
        assert(s == "9223372036854775807");
    }
    // various string types
    {
        wstring w;
        dstring d;
        const( char)[] cs;
        const(wchar)[] cw;
        const(dchar)[] cd;
         char[] ms;
        wchar[] mw;
        dchar[] md;

        w = unsignedIntegerToString!wstring(0x1234ABCD, 16);
        assert(w == "1234ABCD");
        d = unsignedIntegerToString!dstring(0x1234ABCD, 16);
        assert(d == "1234ABCD");

        cs = unsignedIntegerToString!(const char[])(0x1234ABCD, 16);
        assert(cs == "1234ABCD");
        cw = unsignedIntegerToString!(const wchar[])(0x1234ABCD, 16);
        assert(cw == "1234ABCD");
        cd = unsignedIntegerToString!(const dchar[])(0x1234ABCD, 16);
        assert(cd == "1234ABCD");

        ms = unsignedIntegerToString!(char[])(0x1234ABCD, 16);
        assert(ms == "1234ABCD");
        mw = unsignedIntegerToString!(wchar[])(0x1234ABCD, 16);
        assert(mw == "1234ABCD");
        md = unsignedIntegerToString!(dchar[])(0x1234ABCD, 16);
        assert(md == "1234ABCD");
    }
    // CTFE
    {
        enum s = unsignedIntegerToString!string(0x1234ABCD, 16);
        assert(s == "1234ABCD");
    }
}


/*
 * Converts a signed integer $(D num) to a string of the type $(D String) in
 * the decimal radix with a possible negative sign.
 */
String signedIntegerToString(String)(intmax_t num) pure @safe
    if (isSomeString!(String))
{
    alias typeof(String.init[0]) Char;

    Mutable!Char[intmax_t.sizeof*5/2 + 2] buffer = void;
    size_t    pos = buffer.length;
    uintmax_t cur = (num < 0 ? -num : num);

    assert(cur >= 0);
    do
    {
        auto dig = cur % 10;
        cur /= 10;
        buffer[--pos] = cast(Char) ('0' + dig);
    }
    while (cur != 0);

    if (num < 0)
        buffer[--pos] = '-';

    static if (is(Char == immutable))
        return buffer[pos .. $].idup;
    else
        return buffer[pos .. $].dup;
}

unittest
{
    {
        string s;

        s = signedIntegerToString!string(123454321);
        assert(s == "123454321");
        s = signedIntegerToString!string(-123454321);
        assert(s == "-123454321");
        s = signedIntegerToString!string(0);
        assert(s == "0");
        s = signedIntegerToString!string(long.max);
        assert(s == "9223372036854775807");
        s = signedIntegerToString!string(long.min);
        assert(s == "-9223372036854775808");
    }
    // various string types
    {
        wstring w;
        dstring d;
        const( char)[] cs;
        const(wchar)[] cw;
        const(dchar)[] cd;
         char[] ms;
        wchar[] mw;
        dchar[] md;

        w = signedIntegerToString!wstring(-123456789);
        assert(w == "-123456789");
        d = signedIntegerToString!dstring(-123456789);
        assert(d == "-123456789");

        cs = signedIntegerToString!(const char[])(-1234567890);
        assert(cs == "-1234567890");
        cw = signedIntegerToString!(const wchar[])(-1234567890);
        assert(cw == "-1234567890");
        cd = signedIntegerToString!(const dchar[])(-1234567890);
        assert(cd == "-1234567890");

        ms = signedIntegerToString!(char[])(-1234567890);
        assert(ms == "-1234567890");
        mw = signedIntegerToString!(wchar[])(-1234567890);
        assert(mw == "-1234567890");
        md = signedIntegerToString!(dchar[])(-1234567890);
        assert(md == "-1234567890");
    }
    // CTFE
    {
        enum s = signedIntegerToString!string(-1234567890);
        assert(s == "-1234567890");
    }
}


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
// string to floating-point
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

/*
 * Parses a floating point number represented in _radix $(D radix)
 * mantissa digits.  The _radix must be 2, 10 or 16.
 *
 * Params:
 *   source = nonempty string which begins with a valid floating point
 *            number representation, "nan" or "infinity".
 *   radix  = the numeral _radix to use, or specify zero to determine it
 *            by parsing a preceding _radix prefix if any.
 *
 * Returns:
 *   The represented floating point number.
 *
 * Throws:
 *   $(D ConvError) if the source represents no floating-point number in
 *   the specified radix.
 */
Parsed!(Source, real) parseReal(Source)(Source source, uint radix = 0) pure @safe
    if (isSomeImmutableString!(Source))
in
{
    assert(radix == 0 || radix == 2 || radix == 10 || radix == 16);
}
body
{
    alias typeof(Source.init[0]) Char;
    Char[] rest = source;

    if (rest.length == 0)
        throw new ConvError("source string is empty");

    /*
     * Scan a sign if any.
     */
    bool negate;

    negate = (rest[0] == '-');
    if (negate || rest[0] == '+')
        rest = rest[1 .. $];

    if (rest.length == 0)
        throw new ConvError("source string represents no floating point"
                ~ " number, only a sign");

    /*
     * Special or normal?
     */
    real result;

    if (rest.pure_istartsWith("inf"))
    {
        // infinity
        result = real.infinity;
        rest   = rest[3 .. $];

        if (rest.pure_istartsWith("inity")) // inf'inity
            rest = rest[5 .. $];
    }
    else if (rest.pure_istartsWith("nan"))
    {
        // NaN
        result = real.nan;
        rest   = rest[3 .. $];

        // expect NaN payload to be a hexadecimal integer string
        // XXX: should we use stdc.math.nan()?
        if (rest.length >= 2 && rest[0] == '(')
        {
            ulong payload;

            auto tokint = parseIntegral!ulong(rest[1 .. $], 16);
            payload = tokint.value;
            rest    = tokint.rest;

            if (!rest.length || rest[0] != ')')
                throw new ConvError("NaN payload is not closed");
            rest = rest[1 .. $];

            /+ @@@ NaN's impure @@@ 
            if (!__ctfe || HAVE_BUILTIN_NAN)
                result = NaN(payload);
            +/
        }
    }
    else
    {
        // number
        auto tokreal = parseNumericReal(rest, radix);
        result = tokreal.value;
        rest   = tokreal.rest;
    }

    // apply the sign
    return Parsed!(Source, real)(rest, negate ? -result : result);
}

unittest
{
    static bool feq(real a, real b)
    {
        return feqrel(a, b) >= real.mant_dig - 1;
    }

    // normal numbers
    {
        auto tok1 = parseReal("+1.0e0");
        assert(tok1.value == 1);
        assert(tok1.rest == "");

        auto tok2 = parseReal("-2.5e1");
        assert(tok2.value == -25);
        assert(tok2.rest == "");
    }
    // binary and hexadecimal
    {
        auto bin1 = parseReal("+1011.01001110p4", 2);
        assert(bin1.value == 0xb.4ep4);
        assert(bin1.rest == "");

        auto bin2 = parseReal("-0b1011.01001110p-4");
        assert(bin2.value == -0xb.4ep-4);
        assert(bin2.rest == "");

        auto bin3 = parseReal("0b.0100");
        assert(bin3.value == 0x0.4p0);
        assert(bin3.rest == "");

        auto hex1 = parseReal("a.bcdefp+5", 16);
        assert(hex1.value == 0xa.bcdefp5);
        assert(hex1.rest == "");

        auto hex2 = parseReal("0xfed.cbap-5");
        assert(hex2.value == 0xfed.cbap-5);
        assert(hex2.rest == "");

        auto hex3 = parseReal("0x.ffff");
        assert(hex3.value == 0x0.ffffp0);
        assert(hex3.rest == "");
    }
    // non-digit at the end
    {
        auto tok1 = parseReal("1234.5LL");
        assert(tok1.value == 1234.5);
        assert(tok1.rest == "LL");

        auto tok2 = parseReal("1.0110234", 2);
        assert(tok2.value == 0x1.6p0);
        assert(tok2.rest == "234");

        auto tok3 = parseReal("1234p5");
        assert(tok3.value == 1234);
        assert(tok3.rest == "p5");
    }
    // wide chars
    {
        auto tok1 = parseReal!wstring("123.\uff10");
        assert(tok1.value == 123);
        assert(tok1.rest == "\uff10");

        auto tok2 = parseReal!dstring("abc.\uff10", 16);
        assert(tok2.value == 0xabc.p0L);
        assert(tok2.rest == "\uff10");
    }
    // special values
    {
        auto inf1 = parseReal("inf");
        assert(isInfinity(inf1.value));
        auto inf2 = parseReal("+INF");
        assert(isInfinity(inf2.value));
        auto inf3 = parseReal("-InFiNiTy");
        assert(isInfinity(inf3.value));
        auto nan1 = parseReal("nan");
        assert(isNaN(nan1.value));
        auto nan2 = parseReal("-NaN");
        assert(isNaN(nan2.value));
        auto nan3 = parseReal("+nAn(DaBe5a)");
        assert(isNaN(nan3.value));
    }
    // limits
    static if (real.mant_dig == 64)
    {{
        auto eps = parseReal("1.0842021724855044340E-19");
        assert(feq(eps.value, real.epsilon));
        assert(eps.rest == "");
    }}
    static if (real.mant_dig == 64 && real.min_10_exp == -4931)
    {{
        auto min = parseReal("3.3621031431120935063E-4932");
        assert(feq(min.value, real.min_normal));
        assert(min.rest == "");

        auto sub = parseReal("1e-4949");
        assert(isSubnormal(sub.value));
        assert(sub.rest == "");
    }}
    static if (real.mant_dig == 64 && real.max_10_exp == 4932)
    {{
        auto max = parseReal("1.1897314953572317650E+4932");
        assert(feq(max.value, real.max));
        assert(max.rest == "");
    }}
    // very long mantissa/exponent
    {
        auto longmant = parseReal(
                "999999999999999999999999999999999999999999999999999999999"
                ~ "999999999999999999999999999.999999999999999999999999999"
                ~ "999999999999999999999999999999999999999999999999999999#");
        assert(feq(longmant.value, 1e84));
        assert(longmant.rest == "#");

        auto huge = parseReal(
                "-1.e99999999999999999999999999999999999999999999999999999#");
        assert(huge.value < 0 && isInfinity(huge.value));
        assert(huge.rest == "#");

        auto tiny = parseReal(
                "0x1.p-999999999999999999999999999999999999999999999999999#");
        assert(tiny.value == 0);
        assert(tiny.rest == "#");
    }
    // error
    {
        try { parseReal(""); assert(0); } catch (ConvError e) {}
        try { parseReal("zz"); assert(0); } catch (ConvError e) {}
        try { parseReal("-"); assert(0); } catch (ConvError e) {}
        try { parseReal("-.e"); assert(0); } catch (ConvError e) {}
        try { parseReal(".e5"); assert(0); } catch (ConvError e) {}
        try { parseReal("0x"); assert(0); } catch (ConvError e) {}
        try { parseReal("0b."); assert(0); } catch (ConvError e) {}
        try { parseReal("A", 2); assert(0); } catch (ConvError e) {}
        try { parseReal("1.e"); assert(0); } catch (ConvError e) {}
        try { parseReal("1.e+"); assert(0); } catch (ConvError e) {}
        try { parseReal("0x0.p"); assert(0); } catch (ConvError e) {}
    }
    // CTFE
    {
        enum tok = parseReal("1234.5678e90LL");
        assert(feq(tok.value, 1234.5678e90));
        assert(tok.rest == "LL");

        enum inf = parseReal("inFiniTyness");
        assert(isInfinity(inf.value));
        assert(inf.rest == "ness");
    }
}


/*
 * Parses a floating point number represented in radix $(D radix_)
 * mantissa digits.  The radix must be 2, 10 or 16.
 *
 * Params:
 *   source = nonempty string which begins with a valid floating point
 *            number representation.
 *   radix_ = the numeral radix to use, or specify zero to determine it
 *            by parsing a preceding radix prefix if any.
 *
 * Returns:
 *   The represented floating point number.  Infinite value is returned
 *   if the represented number is too large to fit in $(D real); zero if
 *   too small.
 *
 * Throws:
 *   $(D ConvError) if the source represents no floating-point number in
 *   the specified radix.
 */
private Parsed!(Source, real) parseNumericReal(Source)(
        Source source, uint radix_ ) pure @safe
    if (isSomeImmutableString!(Source))
in
{
    assert(source.length > 0);
    assert(radix_ == 0 || radix_ == 2 || radix_ == 10 || radix_ == 16);
}
out(r)
{
    assert(r.value == r.value); // not NaN
}
body
{
    alias typeof(source.init[0]) Char;
    Char[] rest = source;
    real   result;

    /*
     * Detect the radix by scanning a radix prefix if it's not specified.
     */
    uint radix = (radix_ == 0 ? 10 : radix_);

    if (radix_ == 0 && rest.length >= 2 && rest[0] == '0')
    {
        size_t skip;

        switch (rest[1])
        {
            case 'X', 'x': radix = 16; skip = 2; break;
            case 'B', 'b': radix =  2; skip = 2; break;
            default: break;
        }
        if (rest.length == skip)
            throw new ConvError("source string represents no"
                    ~ " floating-point number, only a radix prefix");
        rest = rest[skip .. $];
    }

    assert(radix == 2 || radix == 10 || radix == 16);
    assert(rest.length > 0);

    /*
     * Read in a mantissa using parseMantissa().  The returned mantissa
     * value is scaled with radix^-expFix.
     */
    real mant;      // scaled mantissa value
    int  expFix;    // scale factor in log_radix

    auto tokmant = parseMantissa!(Source)(rest, radix);
    mant   = tokmant.values[0];
    expFix = tokmant.values[1];
    rest   = tokmant.rest;

    /*
     * Read in an exponent if any.
     */
    int  exp;
    uint expRadix;

    // Set the numeral radix for the exponent part.  Here we also scale
    // expFix (whose base is radix, not expRadix) with appropriate factor.
    final switch (radix)
    {
        case 10: expRadix = 10;              break;
        case 16: expRadix =  2; expFix *= 4; break;
        case  2: expRadix =  2;              break;
    }

    if (rest.length > 0) switch (rest[0])
    {
        case 'E', 'e': // use 'e' exponent only if the expected radix is 10
            if (expRadix == 10)
            {
                auto tokexp = parseExponent!(Source)(rest[1 .. $]);
                exp  = tokexp.value;
                rest = tokexp.rest;
            }
            break;

        case 'P', 'p': // use 'p' exponent only if the expected radix is 2
            if (expRadix == 2)
            {
                auto tokexp = parseExponent!(Source)(rest[1 .. $]);
                exp  = tokexp.value;
                rest = tokexp.rest;
            }
            break;

        default:
            break;
    }

    // load the exponent
    switch (exp)
    {
        case int.max: result = real.infinity; break;
        case int.min: result =             0; break;

        default:
            result = ldexpN(mant, expRadix, exp + expFix);
            break;
    }

    return Parsed!(Source, real)(rest, result);
}

unittest
{
    // detect radix
    {
        auto dec = parseNumericReal("01234.5", 0);
        assert(dec.value == 1234.5);
        assert(dec.rest == "");

        auto hex = parseNumericReal("0x0", 0);
        assert(hex.value == 0);
        assert(hex.rest == "");

        auto bin = parseNumericReal("0b1", 0);
        assert(bin.value == 1);
        assert(bin.rest == "");
    }
    // exponent
    {
        auto dec1 = parseNumericReal("1.0e10", 10);
        assert(dec1.value == 1.0e10);
        assert(dec1.rest == "");

        auto dec2 = parseNumericReal("1.E10", 10);
        assert(dec2.value == 1.e10);
        assert(dec2.rest == "");

        auto dec3 = parseNumericReal(".1e10", 10);
        assert(dec3.value == .1e10);
        assert(dec3.rest == "");

        auto hex = parseNumericReal("1.ap5", 16);
        assert(hex.value == 0x1.ap5);
        assert(hex.rest == "");

        auto bin = parseNumericReal("1.1P5", 2);
        assert(bin.value == 0x1.8p5);
        assert(bin.rest == "");
    }
}


/*
 * Parses a floating point number without sign nor exponent part as a
 * pair of the value and its scale exponent.
 *
 * Params:
 *   source = nonempty string which starts with a validly formatted
 *            floating-point number in _radix $(D radix).
 *   radix  = numeral radix to use in parsing digits
 *
 * Returns:
 *   .values[0] = the mantissa scaled by radix^exp
 *   .values[1] = exp, or the scale factor of the mantissa in log_radix
 */
private Parsed!(Source, real, int) parseMantissa(Source)(
        Source source, uint radix) pure @safe
    if (isSomeImmutableString!(Source))
in
{
    assert(2 <= radix && radix <= MAX_RADIX);
}
out(r)
{
    assert(r.values[0] == r.values[0]); // the result never be a NaN
}
body
{
    alias typeof(source.init[0]) Char;
    Char[] rest = source;

    /*
     * Read in mantissa.  We will read higher digits in mant in the exact
     * form (i.e. integer), and then read lower digits in aux.  aux will be
     * zero if the represented mantissa is short enough to fit in mant.
     *
     * The mantissa is read as the following form regardless of the original
     * representation.
     *                    mant . aux * base^expFix
     */
    mant_t mant = 0;    // exact representation of the MSBs
    mant_t aux  = 0;    // used for correcting the LSBs
    int    auxExp;

    size_t ndigits;     // the number of digits
    bool   inFrac;      // are we reading digits past a decimal point?
    int    expFix;

    immutable mantLimit = (MANT_MAX - (radix - 1)) / radix;

    for (; rest.length > 0; rest = rest[1 .. $])
    {
        Char ch = rest[0];

        if (ch == '.')
        {
            if (inFrac)
                // The radix point appeared twice, which never happens
                // for correct floating point numbers.  End of the token.
                break;
            inFrac = true;
            continue;
        }

        // Read in a digit.
        uint digit = MAX_RADIX;

        if ('0' <= ch && ch <= '9')
            digit = ch - '0';
        else if (ch >= 'a')
            digit = ch - 'a' + 10;
        else if (ch >= 'A')
            digit = ch - 'A' + 10;

        if (digit >= radix)
            break; // not a digit in the specified radix; end
        ++ndigits;

        // Use aux if mant is full.
        if (auxExp || mant > mantLimit)
        {
            if (aux <= mantLimit)
            {
                aux = aux * radix + digit;
                --auxExp;
            }

            if (!inFrac)
                // we are passing through digits in the integer part,
                // so need to upscale mant.
                ++expFix;
        }
        else
        {
            mant = mant * radix + digit;
            if (inFrac)
                // we are reading the fractional part as an integer,
                // so need to downscale mant.
                --expFix;
        }
    }

    if (ndigits == 0)
        throw new ConvError("source string does not contain any digit in mantissa");

    /*
     * Combine mant and aux.  We don't scale the mantissa here (with expFix)
     * for reducing LSB errors.
     */
    real result;

    result  = mant;
    result += ldexpN(aux, radix, auxExp);

    return Parsed!(Source, real, int)(rest, result, expFix);
}

unittest
{
    auto dec = parseMantissa("123.456e4", 10);
    assert(dec.values[0] == 123456.L);
    assert(dec.values[1] == -3);
    assert(dec.rest == "e4");

    auto hex = parseMantissa("abc.98765p", 16);
    assert(hex.values[0] == 0xabc98765.p0L);
    assert(hex.values[1] == -5);
    assert(hex.rest == "p");

    auto bin = parseMantissa("0.1101e", 2);
    assert(bin.values[0] == 0xd.p0L);
    assert(bin.values[1] == -4);
    assert(bin.rest == "e");
}


/*
 * Parses a decimal integer as a floating-point exponent.
 *
 * Params:
 *   source = nonempty string which starts with a signed decimal integer.
 *
 * Returns:
 *   The value of the exponent as an exp_t value.  exp_t.max is returned
 *   for positive huge exponent, exp_t.min for negative huge exponent.
 */
private Parsed!(Source, exp_t) parseExponent(Source)(Source source) pure @safe
{
    alias typeof(source.init[0]) Char;
    Char[] rest = source;

    if (source.length == 0)
        throw new ConvError("source string has an exponent part, but"
                " nothing follows");

    /*
     * Scan the sign of an exponent if any.
     */
    bool negate;

    negate = rest[0] == '-';
    if (negate || rest[0] == '+')
        rest = rest[1 .. $];

    /*
     * Scan an exponent as an absolute value.  We will pass through all
     * digits even if the exponent is too huge to fit in exp_t.
     */
    exp_t exp;
    bool  huge;
    uint  ndigits;

    enum absLimit = real.max_exp + real.mant_dig;

    for (; rest.length; rest = rest[1 .. $])
    {
        Char ch = rest[0];

        // Read in a digit.
        uint digit;

        if (ch < '0' || '9' < ch)
            break; // not a decimal digit
        digit = ch - '0';
        ++ndigits;

        if (!huge)
        {
            exp = exp*10 + digit;
            if (exp > absLimit)
                huge = true;
        }
    }

    if (ndigits == 0)
        throw new ConvError("source string has an exponent, but no digits");

    if (huge)
        exp = negate ? exp.min : exp.max;
    else
        exp = negate ? -exp : exp;

    return Parsed!(Source, exp_t)(rest, exp);
}

unittest
{
    // common cases
    {
        auto zero = parseExponent("0");
        assert(zero.value == 0);
        assert(zero.rest == "");

        auto ex = parseExponent("123");
        assert(ex.value == 123);
        assert(ex.rest == "");

        auto dmax = parseExponent("-1021");
        assert(dmax.value == -1021);
        assert(dmax.rest == "");

        auto dmin = parseExponent("+1024");
        assert(dmin.value == 1024);
        assert(dmin.rest == "");
    }
    // nondigit at the end
    {
        auto ex = parseExponent("1024LL");
        assert(ex.value == 1024);
        assert(ex.rest == "LL");
    }
    // huge exponent
    static if (EXP_SPAN < 100000)
    {
        auto pos = parseExponent("100000");
        assert(pos.value == exp_t.max);

        auto neg = parseExponent("-100000");
        assert(neg.value == exp_t.min);
    }
}


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
// traits on real
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

// The max integer value exactly represented in a real.
enum real REAL_INTEGER_MAX = mixin("0x1p" ~ to!string(real.mant_dig) ~ "L") - 1.L;

/*
 * mant_t   = numeric type which can hold REAL_INTEGER_MAX
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
    // real can hold REAL_INTEGER_MAX, perhaps slower than integer types
    alias real mant_t;
    enum mant_t MANT_MAX = REAL_INTEGER_MAX;
}


/*
 * exp_t       = signed integral type which can hold an exponent of real
 * EXP_SPAN    = positive exponent value such that multiplying 2^EXP_SPAN
 *               to any finite number (including subnormal ones) gives infinity
 * EXP_10_SPAN = same as EXP_SPAN, except for its base being 10
 */
alias int exp_t;

enum
{
    EXP_SPAN    = real.max_exp - (real.min_exp - real.mant_dig),
    EXP_10_SPAN = (real.max_10_exp + 1) - (real.min_10_exp - (real.dig + 1)),
}

unittest
{
    assert(exp_t.min < -EXP_SPAN && EXP_SPAN < exp_t.max);

    /* EXP_SPAN */
    const real tiny = ldexp(real.min_normal, -real.mant_dig + 1);
    assert(tiny > 0);

    assert(ldexp2(tiny, EXP_SPAN - 1) < real.infinity);
    assert(ldexp10(tiny, EXP_10_SPAN - 1) < real.infinity);
    assert(isInfinity(ldexp2(tiny, EXP_SPAN)));
    assert(isInfinity(ldexp10(tiny, EXP_10_SPAN)));
}



//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
// CTFE math
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

// these may be built in the compiler or CTFE safe
private enum : bool
{
    HAVE_BUILTIN_NAN   = __traits(compiles, expectConstant!(   NaN(0uL   ) )),
    HAVE_BUILTIN_LDEXP = __traits(compiles, expectConstant!( ldexp(1.L, 0) )),
}
private template expectConstant(alias v) {}


private
{
/++ @@@BUG4298@@@

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
        static if ( (sign == '+' && absexp <  real.max_exp                ) ||
                    (sign == '-' && absexp < -real.min_exp + real.mant_dig) )
            alias TypeTuple!(
                        mixin("0x1p" ~ sign ~ to!string(absexp) ~ "L"),
                        enumerateExp10!(sign, 2*absexp)
                    ) enumerateExp2;
        else
            alias TypeTuple!() enumerateExp2;
    }

    static immutable
        positiveExp10 = [ enumerateExp10!'+' ], // 10^( 2^i)
        negativeExp10 = [ enumerateExp10!'-' ]; // 10^(-2^i)

    static immutable
        positiveExp2  = [  enumerateExp2!'+' ], //  2^( 2^i)
        negativeExp2  = [  enumerateExp2!'-' ]; //  2^(-2^i)

++/

    static assert(real.min_exp < -8192 && 8192 < real.max_exp);

    static immutable real[13] positiveExp10 = [
            1e+1L, 1e+2L, 1e+4L, 1e+8L, 1e+16L, 1e+32L, 1e+64L,
            1e+128L, 1e+256L, 1e+512L, 1e+1024L, 1e+2048L, 1e+4096L ];

    static immutable real[13] negativeExp10 = [
            1e-1L, 1e-2L, 1e-4L, 1e-8L, 1e-16L, 1e-32L, 1e-64L,
            1e-128L, 1e-256L, 1e-512L, 1e-1024L, 1e-2048L, 1e-4096L ];

    static immutable real[14] positiveExp2 = [
            0x1p+1L, 0x1p+2L, 0x1p+4L, 0x1p+8L, 0x1p+16L, 0x1p+32L, 0x1p+64L,
            0x1p+128L, 0x1p+256L, 0x1p+512L, 0x1p+1024L, 0x1p+2048L, 0x1p+4096L,
            0x1p+8192L ];

    static immutable real[15] negativeExp2 = [
            0x1p-1L, 0x1p-2L, 0x1p-4L, 0x1p-8L, 0x1p-16L, 0x1p-32L, 0x1p-64L,
            0x1p-128L, 0x1p-256L, 0x1p-512L, 0x1p-1024L, 0x1p-2048L, 0x1p-4096L,
            0x1p-8192L, /+subnormal+/ 0x1p-16384L ];
}


/*
 * Computes x * r^n.  Supported bases are: r = 2, 10 and 16.
 */
real ldexpN(real x, uint r, int n) pure nothrow @safe
in
{
    assert(r == 2 || r == 10 || r == 16);
}
body
{
    final switch (r)
    {
        case  2: return ldexp2 (x, n);
        case 10: return ldexp10(x, n);
        case 16: return ldexp2 (x, n*4);
    }
}


/*
 * Computes x * 2^k.
 */
real ldexp2(real x, int k) pure nothrow @safe
{
    if (!__ctfe || HAVE_BUILTIN_LDEXP)
        return std.math.ldexp(x, k);

    /*
     * Handle obvious cases.
     */
    if (x != x) return x; // NaN
    if (x == 0) return x;
    if (x == real.infinity || x == -real.infinity) return x;
    if (k < -EXP_SPAN) return 0;
    if (k >  EXP_SPAN) return x < 0 ? -real.infinity : real.infinity;

    /*
     * We can precisely multiply 2^? for multiple times.
     */
    immutable(real)[] exp2tab;
    if (k > 0) exp2tab = positiveExp2;
    if (k < 0) exp2tab = negativeExp2;

    real result = x;
    uint curexp = (k < 0 ? -k : k);

    foreach_reverse (i, exp2val; exp2tab)
    {
        if (result == 0 || result == real.infinity || result == -real.infinity)
            break; // prevent NaN

        // exp2val = 2^(2^i)
        immutable exp = 1u << i;

        while (curexp >= exp)
        {
            curexp -= exp;
            result *= exp2val;
        }
    }
    assert(result == result);

    return result;
}

unittest
{
    {
        assert(ldexp2(1,  512) == 0x1.p+512);
        assert(ldexp2(1, -512) == 0x1.p-512);
        assert(ldexp2(0x1.234p+80, -100) == 0x1.234p-20);
        assert(ldexp2(0x1.876p-80,   99) == 0x1.876p+19);
        assert(ldexp2(-10,  4) == -160);
        assert(ldexp2(-10, -2) == -2.5);
    }
    // obvious cases
    {
        assert(ldexp2(0, exp_t.max) == 0);
        assert(ldexp2(real.infinity, exp_t.min) == real.infinity);
        assert(ldexp2(1, exp_t.max) == real.infinity);
        assert(ldexp2(1, exp_t.min) == 0);
        assert(isNaN(ldexp2(real.nan, 1)));
        assert(isInfinity(ldexp2( real.infinity, -10000)));
        assert(isInfinity(ldexp2(-real.infinity, -10000)));
    }
    // CTFE
    {
        enum n = ldexp2(0x1.p-1073L, 2096);
        assert(n == 0x1.p1023L);
    }
}


/*
 * Computes x * 10^n.
 */
real ldexp10(real x, exp_t k) pure nothrow @safe
{
    /*
     * Handle obvious cases.
     */
    if (x != x) return x; // NaN
    if (x == 0) return x;
    if (x == real.infinity || x == -real.infinity) return x;
    if (k < -EXP_10_SPAN) return 0;
    if (k >  EXP_10_SPAN) return x < 0 ? -real.infinity : real.infinity;

    /*
     * Reduce exponent so we can safely compute 10^k.
     */
    if (k < real.min_10_exp)
        return ldexp10(ldexp10(x, real.min_10_exp), k - real.min_10_exp);
    if (k > real.max_10_exp)
        return ldexp10(ldexp10(x, real.max_10_exp), k - real.max_10_exp);

    assert(real.min_10_exp <= k && k <= real.max_10_exp);

    /*
     * Compute 10^k and multiply it to x.  This gives a bit better result
     * than repeatedly multiplying 10^? to x (like done in ldexp2).
     */
    immutable(real)[] exp10tab;
    if (k > 0) exp10tab = positiveExp10;
    if (k < 0) exp10tab = negativeExp10;

    real scale  = 1;
    uint curexp = (k < 0 ? -k : k);

    foreach_reverse (i, exp10val; exp10tab)
    {
        // exp10val = 10^(2^i)
        immutable exp = 1u << i;

        if (curexp >= exp)
        {
            curexp -= exp;
            scale  *= exp10val;
        }
        assert(scale != 0 && scale < real.infinity);
    }
    assert(curexp == 0);
    assert(scale == scale);

    return x * scale;
}

unittest
{
    {
        assert(ldexp10(1,  10) == 1e+10);
        assert(ldexp10(1, -10) == 1e-10);
        assert(ldexp10(-7, 4) == -70000);
        assert(ldexp10(-200, -2) == -2);
        assert(feqrel!real(ldexp10( 1.7,  293),  1.7e+293) >= real.mant_dig - 1);
        assert(feqrel!real(ldexp10(-1.7, -293), -1.7e-293) >= real.mant_dig - 1);
    }
    // obvious cases
    {
        assert(ldexp10(0, exp_t.max) == 0);
        assert(ldexp10(real.infinity, exp_t.min) == real.infinity);
        assert(ldexp10(1, exp_t.max) == real.infinity);
        assert(ldexp10(1, exp_t.min) == 0);
        assert(isNaN(ldexp10(real.nan, 1)));
        assert(isInfinity(ldexp10( real.infinity, -10000)));
        assert(isInfinity(ldexp10(-real.infinity, -10000)));
    }
    // CTFE
    {
        enum n = ldexp10(1.234, real.max_10_exp);
        enum p = mixin("1.234e" ~ to!string(real.max_10_exp) ~ "L");
        assert(feqrel!real(n, p) >= real.mant_dig - 1);
    }
}

