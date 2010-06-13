module conv.common;

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

/*
 * For 'pure' lexer
 */
struct Scanned(T)
{
    T      value;
    string rest;
}

struct Scanned(TT...)
    if (TT.length >= 2)
{
    TT     values;
    string rest;
}


/*
 * For unittests.  The argument will be evaluated at compile time.
 */
version (unittest) template eval(alias value)
{
    enum eval = value;
}



//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
// Constants
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

enum char DELIM = '_';

enum NOT_DIGIT = cast(immutable) ubyte.max;

immutable DIGITS = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

enum ubyte MAX_BASE = DIGITS.length;

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


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
// Lexer utilities [pure safe]
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

char toAsciiLower(char ch) pure nothrow @safe
{
    if ('A' <= ch && ch <= 'Z')
        return cast(char) (ch + 'a' - 'A');
    else
        return ch;
}

char toAsciiUpper(char ch) pure nothrow @safe
{
    if ('a' <= ch && ch <= 'z')
        return cast(char) (ch - ('a' - 'A'));
    else
        return ch;
}

string skipSpaces(string s) pure nothrow @safe
{
    foreach (i, ch; s)
    {
        if (ch != ' ')
            return s[i .. $];
    }
    return null;
}

bool startsWith(string str, string prefix) pure nothrow @safe
{
    if (str.length >= prefix.length)
        return str[0 .. prefix.length] == prefix;
    else
        return false;
}



bool isDigit(dchar ch, ubyte base) pure nothrow @safe
in
{
    assert(2 <= base && base <= MAX_BASE);
}
body
{
    if (base <= 10 || ch <= '9')
    {
        return '0' <= ch && ch <= ('0' + base);
    }
    else
    {
        assert(base > 10 && ch > '9');
        return ('A' <= ch && ch <= ('A' + base)) ||
               ('a' <= ch && ch <= ('a' + base));
    }
}


/*
 *
 */
bool prefixedBy(string str, string prefix) pure nothrow @safe
{
    if (str.length > prefix.length)
    {
        foreach (i, pch; prefix)
        {
            if (str[i] != toAsciiLower(pch) && str[i] != toAsciiUpper(pch))
                return false;
        }
        return true;
    }
    return false;
}


/*
 *
 */
ubyte decodePossibleDigit(char ch, ubyte radix) pure nothrow @trusted
in
{
    assert(2 <= radix && radix <= 36);
}
out(r)
{
    assert(r < radix || r == NOT_DIGIT);
}
body
{
    if (ch < 0x80)
    {
        ubyte digit = DIGIT_TABLE[ch];

        if (digit < radix)
            return digit;
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
 *
 */
ubyte detectBase(string numstr) pure nothrow @safe
in
{
    assert(numstr.length > 0);
}
body
{
    ubyte base = 10;

    if (numstr[0] == '0')
    {
        base = 8;

        if (numstr.length > 1) switch (numstr[1])
        {
            case 'b', 'B': base =  2; break;
            case 'x', 'X': base = 16; break;
            default: break;
        }
    }
    return base;
}

unittest
{
    assert(detectBase("0b") == 2);
    assert(detectBase("0B") == 2);
    assert(detectBase("0x") == 16);
    assert(detectBase("0X") == 16);
    assert(detectBase("0") == 8);
    assert(detectBase("01") == 8);
    assert(detectBase("1") == 10);
}


/*
 * Removes a leading numeral prefix (e.g. "0x") from numstr.
 */
string withoutNumeralPrefix(string numstr, char ch) pure nothrow @safe
in
{
    assert('A' <= ch && ch <= 'Z');
}
body
{
    if (numstr.length >= 2 && numstr[0] == '0')
    {
        if (numstr[1] == ch || numstr[1] == ch + ('a' - 'A'))
            return numstr[2 .. $];
    }
    return numstr;
}

unittest
{
    assert(withoutNumeralPrefix("0xCaba", 'X') == "Caba");
    assert(withoutNumeralPrefix("012345", 'X') == "012345");
    assert(withoutNumeralPrefix("", 'X') == "");
}


/*
 * Returns str sans leading ch's.
 */
string consumeChars(string str, char ch) pure nothrow @safe
{
    foreach (i, c; str)
    {
        if (c != ch)
            return str[i .. $];
    }
    return str;
}

unittest
{
    assert(consumeChars("", 'a') == "");
    assert(consumeChars("abcde", 'a') == "bcde");
    assert(consumeChars("aaabcde", 'a') == "bcde");
    assert(consumeChars("abbbcde", 'b') == "abbbcde");
}


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
// Common scanner
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

/*
 *
 */
Scanned!bool scanSign(string input) pure @safe
{
    bool   signed;
    string rest = input;

    if (input.length) switch (input[0])
    {
        case '+': signed = false; rest = rest[1 .. $]; break;
        case '-': signed =  true; rest = rest[1 .. $]; break;
        default : break;
    }
    return Scanned!bool(signed, rest);
}


/*
 *
 */
Scanned!ubyte scanBase(string input_, ubyte pref = 0) pure @safe
in
{
    assert(pref == 0 || (2 <= pref && pref <= MAX_BASE));
}
body
{
    string input = input_;
    ubyte  base  = (pref == 0) ? 10 : pref;

    if (input.length == 0)
        throw new Exception("No sufficient input");

    if (input[0] == '0' && input.length > 1)
    {
        immutable mark = input[1];
        size_t    skip;

        switch (mark)
        {
            case 'b', 'B': base =  2; skip = 2; break;
            case 'x', 'X': base = 16; skip = 2; break;

            default:
                if ('0' <= mark && mark <= '7')
                {
                    base = 8;
                    skip = 1;
                }
                break;
        }

        if (pref == 0 || base == pref)
            input = input[skip .. $];
    }
    return Scanned!ubyte(base, input);
}


/*
 * Scans _base-$(D base) digits as a numeric value of the type $(D Num).
 */
Scanned!Num scanDigits(Num)(
        string input_, ubyte base, Num limit = Num.max ) pure @safe
in
{
    assert(2 <= base && base <= MAX_BASE);
}
out(r)
{
    assert(r.value <= limit);
}
body
{
    /*
     * digit<base>   =  "0" | "1" | "2" | ...
     * digits<base>  =  ( digit<base> delim* )+
     */
    immutable preMulLimit = limit / base;
    string    input       = input_;
    Num       result      = 0;
    size_t    ndigits;

    while (input.length != 0)
    {
        // Read in a digit
        immutable maybeDigit = decodePossibleDigit(input[0], base);

        if (maybeDigit == NOT_DIGIT)
            break; // end of the token

        immutable digit = maybeDigit;
        assert(0 <= digit && digit < base);
        ++ndigits;

        // Accumulate
        if (result > preMulLimit)
            throw new Exception("Numeral overflow");
        result *= cast(const Num) base;

        if (result > limit - digit)
            throw new Exception("Numeral overflow");
        result += cast(const Num) digit;

        // Consume the digit character and delimiters (if any)
        input = input[1 .. $].consumeChars(DELIM);
    }

    if (ndigits == 0)
        throw new Exception("No digit is available");

    return Scanned!Num(result, input);
}

unittest
{
    // common case
    Scanned!uint r1 = scanDigits!uint("314159265", 10);
    assert(r1.value == 314159265);
    assert(r1.rest.length == 0);

    // hexadecimal
    Scanned!uint r2 = scanDigits!uint("DeadBeef", 16);
    assert(r2.value == 0xDeadBeef);
    assert(r2.rest.length == 0);

    // non-digits at the end
    Scanned!uint r3 = scanDigits!uint("123456789uL", 10);
    assert(r3.value == 123456789);
    assert(r3.rest  == "uL");

    // using delimiters
    Scanned!uint r4 = scanDigits!uint("123_456_789__uL", 10);
    assert(r4.value == 123456789);
    assert(r4.rest  == "uL");

    // using real as the numeric type
    Scanned!real r5 = scanDigits!real("1234xx", 10);
    assert(r5.value == 1234.L);
    assert(r5.rest  == "xx");

    // limiting
    try
    {
        scanDigits!ushort("1000", 10, 1000);
        scanDigits!ushort("1000", 10, 1001);
        scanDigits!ushort("1001", 10, 1001);
        scanDigits!ushort("999", 10, 999);
    }
    catch { assert(0); }

    try { scanDigits!ushort("11", 10, 10); assert(0); } catch {}
    try { scanDigits!ushort("100", 10, 99); assert(0); } catch {}
    try { scanDigits!ushort("101", 10, 100); assert(0); } catch {}

    // CTFE
    enum Scanned!ubyte r7 = scanDigits!ubyte("10110011:", 2);
    assert(r7.value == 0b10110011);
    assert(r7.rest  == ":");
}


/*
 */
struct ScanDigitsStatus(Num)
{
    Num    value;
    string rest;
    bool   overflow;
    size_t count;
}

