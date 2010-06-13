module conv.integral;

import core.stdc.stdint : uintmax_t, intmax_t;
import std.traits       : isSigned, Unsigned;
import conv.common;


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
// CTFE - Converting Integer to String
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

string utoa(uintmax_t num, ubyte base = 10) pure @trusted
in
{
    assert(2 <= base && base <= MAX_BASE);
}
body
{
    enum size_t BUF = 8*uintmax_t.sizeof;

    char[BUF] buffer = void;
    size_t    pos    = buffer.length;

    //
    uintmax_t x = num;

    while (x != 0)
    {
        immutable k = cast(ubyte) (x % base); // trusted
        x /= base;
        buffer[--pos] = DIGITS[k];
    }

    return buffer[pos .. $].idup;
}

string itoa(intmax_t num, ubyte base = 10) pure @trusted
in
{
    assert(2 <= base && base <= MAX_BASE);
}
body
{
    if (num >= 0)
    {
        immutable unum = cast(uintmax_t) num; // trusted
        return utoa(unum);
    }
    else
    {
        immutable unum = cast(uintmax_t) -(num + 1) + 1u; // trusted
        return "-" ~ utoa(unum);
    }
}


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
// CTFE - Converting String to Integer
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

/*
 *
 *
 */
Int atoi(Int = int)(string numstr, ubyte base = 0) pure @safe
in
{
    assert(base == 0 || (2 <= base && base <= MAX_BASE));
}
body
{
    auto scan = scanInteger!(Int)(numstr, base);

    if (scan.rest.length)
        throw new Exception("");
    return scan.value;
}

unittest
{
    // decimal (CTFE)
    {
        enum int v1 = atoi("123456789");
        assert(v1 == 123456789);
    }
    // octal (CTFE)
    {
        // auto detection
        enum int v1 = atoi("01740");
        assert(v1 == 01740);

        // explicit
        enum int v2 = atoi!int("1740", 8);
        assert(v2 == 01740);
        enum int v3 = atoi!int("0001740", 8);
        assert(v3 == 01740);

        // large
        enum long v4 = atoi!long("777777777777777", 8);
        assert(v4 == 0777777777777777);

        enum bool f1 = __traits(compiles, eval!(atoi("", 8)));
        assert(!f1); // empty
        enum bool f2 = __traits(compiles, eval!(atoi("0x012", 8)));
        assert(!f2); // not digit
        enum bool f3 = __traits(compiles, eval!(atoi!ubyte("77777", 8)));
        assert(!f3); // overflow
    }
    // binary (CTFE)
    {
        //enum int v1 = atoi("");
    }
    // hexadecimal (CTFE)
    {
    }
}


/*
 * input      =  [ "+" | "-" ] any<base>
 *
 * any<0>     =  bin | oct | hex | any<10>
 * any<2>     =  bin | digits<2>
 * any<16>    =  hex | digits<16>
 * any<r>     =  digits<r>
 *
 * bin        =  "0b" delim* digits<2>
 * oct        =  "0"  delim* digits<8>?
 * hex        =  "0x" delim* digits<16>
 *
 * digit<r>   =  "0" | "1" | ... | "z"
 * digits<r>  =  ( digit<r> delim* )+
 */
Scanned!Int scanInteger(Int)(string input_, ubyte base_ = 0) pure @safe
in
{
    assert(base_ == 0 || (2 <= base_ && base_ <= MAX_BASE));
}
body
{
    string input = input_;

    /*
     * Determine the sign.
     */
    bool signed;

    auto scsign = scanSign(input);
    signed = scsign.value;
    input  = scsign.rest;

    if (input.length == 0)
        throw new Exception("No sufficient input");

    /*
     * Determine the base to use in convertion if it's not supplied by
     * the caller.
     */
    immutable ubyte base = (base_ == 0 ? detectBase(input) : base_);

    if (base == 2)
    {
        input = input.withoutNumeralPrefix('B');
        input = input.consumeChars(DELIM);
    }
    else if (base == 16)
    {
        input = input.withoutNumeralPrefix('X');
        input = input.consumeChars(DELIM);
    }

    /*
     * Read in the represented integer as its absolute value.
     */
    alias Unsigned!Int Uint;
    Uint uresult = void;

    auto excess = (isSigned!(Int) && signed) ? 1u : 0u;
    auto limit = cast(Uint) (Int.max + excess);

    immutable scand = scanDigits!Uint(input, base, limit);
    uresult = scand.value;
    input   = scand.rest;

    return Scanned!Int(signed ? -uresult : uresult, input);
}

unittest
{
    // common case
    Scanned!int r1 = scanInteger!int("314159265");
    assert(r1.value == 314159265);
    assert(r1.rest.length == 0);

    // hexadecimal
    auto r2_a = scanInteger!uint("0xDeadBeef");
    auto r2_b = scanInteger!uint("0xDeadBeef", 16);
    auto r2_c = scanInteger!uint("DeadBeef", 16);
    assert(r2_a.value == 0xDeadBeef);
    assert(r2_b.value == 0xDeadBeef);
    assert(r2_c.value == 0xDeadBeef);
    assert(r2_a.rest == "");
    assert(r2_b.rest == "");
    assert(r2_c.rest == "");

    // non-digits at the end
    auto r3 = scanInteger!ulong("123456789uL");
    assert(r3.value == 123456789uL);
    assert(r3.rest  == "uL");

    // using delimiters
    auto r4 = scanInteger!uint("123_456_789__uL");
    assert(r4.value == 123456789);
    assert(r4.rest  == "uL");

    // detecting integer overflows
    try
    {
        scanInteger!short("32767");
        scanInteger!short("-32768");
    }
    catch { assert(0); }

    try { scanInteger!short("32768"); assert(0); } catch {}
    try { scanInteger!short("-32769"); assert(0); } catch {}

    // CTFE
    enum Scanned!ubyte r5 = scanInteger!ubyte("0b1010_0110//");
    assert(r5.value == 0b10100110);
    assert(r5.rest  == "//");
}


