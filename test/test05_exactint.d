import std.stdio;

alias ulong mant_t;

static assert(real.mant_dig < 16*mant_t.sizeof);


void main()
{
    writefln("min = %.20e", real.min_normal);
    writefln("max = %.20e", real.max);

    //------------------------------------------------------------
    enum ubyte  MBIT = 8 * mant_t.sizeof;
    enum ubyte  HBIT = 4 * mant_t.sizeof;
    enum mant_t HALF = (cast(mant_t) 1u << HBIT) - 1u;
    enum mant_t STOP = mant_t.max / 100u;

    mant_t mantL = 0;
    mant_t mantH = 0;

    foreach (digchar; "3362103143112093506")
    //foreach (digchar; "33621031431120935063")
    //foreach (digchar; "18446744073709551615")
    //foreach (digchar; "170141183460469231731687303715884105727")
    //foreach (digchar; "170141183460469231731687303715884105728")
    //foreach (digchar; "340282366920938463463374607431768211455")
    {
        auto digit = digchar - '0';
        assert(0 <= digit && digit <= 9);

        auto tmpL = (mantL  & HALF) * 10u + digit;
        auto tmpM = (mantL >> HBIT) * 10u;
        auto tmpH =  mantH          * 10u;

        tmpM +=                         (tmpL >> HBIT);
        mantL =        (tmpM << HBIT) + (tmpL  & HALF);
        mantH = tmpH + (tmpM >> HBIT);
    }
    writefln("L = %16x (%d)", mantL, mantL);
    writefln("H = %16x", mantH);

    //
    real num = mantL;
    writefln("%.20e", num);
    writefln("%.20e", num*1e-18);
    //real num = mantL + mantH * 0x1.p+64L;
    //writefln("%.20e", num);
    //writefln("%.20e", 340282366920938463463374607431768211455.L);
    //writefln("%a", 340282366920938463463374607431768211455.L);
    //writefln("%a", num);
    //writefln("%a", 18446744073709551615.L);
    //writefln("%a", num * 1e-18L * 1e-4932L);

    //------------------------------------------------------------
    // Load base-10 exponent
    //
    //    10^exp = 5^exp + 2^exp
    //
    writeln("--------------------");

    int exp5, exp2;
    //exp5 = exp2 = -18 + (-4932);
    exp5 = exp2 = -18 + (-4932);

    while (exp5 < 0)
    {
        if (mantH <= HALF)
        {
            mantH <<= HBIT;
            mantH  |= mantL >> HBIT;
            mantL <<= HBIT;
            exp2 -= 32;
        }

        auto tmpH = mantH / 5;
        auto tmpM = ((mantH % 5) << HBIT) | (mantL >> HBIT);
        auto tmpL = ((tmpM % 5) << HBIT) | (mantL & HALF);
        tmpM /= 5;
        tmpL = (tmpL + 2) / 5;
        ++exp5;

        //writeln("## ", mantH, "\t", mantL);
        //writeln(">> ", tmpH, "\t", tmpM, "\t", tmpL);
        mantH = tmpH;
        mantL = (tmpM << HBIT) + tmpL;
    }
    //writeln("% ", exp2);

    num = mantH*0x1p64 + mantL;
    for (; exp2 < 0; ++exp2)
        num *= .5;

    writefln("%.20e", num);
    writefln("%a", num);

    return;
/+++
    //------------------------------------------------------------
    // Load base-10 exponent
    //
    //    10^exp = 5^exp + 2^exp
    //
    // 2^exp is always exact in floating point multiplication.
    // So, here we consume the 5^exp part with integer multiplication.
    // We can use 2^-x to prevent integer underflow.
    //
    int exp2, exp5;
    //exp2 = exp5 = 1234; // init

    while (exp5 > 0)
    {
        if (mantH > STOP)
        {
            mantL >>= 10;
            mantL  |= (mantH & 0x2FF) << (MBIT - 10);
            mantH >>= 10;
            exp2   += 10;
        }

        auto tmpL = (mantL  & HALF) * 5u;
        auto tmpM = (mantL >> HBIT) * 5u;
        auto tmpH =  mantH          * 5u;
        --exp5;

        tmpM +=                         (tmpL >> HBIT);
        mantL =        (tmpM << HBIT) + (tmpL  & HALF);
        mantH = tmpH + (tmpM >> HBIT);
    }

    //
    real pow2 = 1;
    while (exp2--)
        pow2 *= 2;

    //------------------------------------------------------------
    real num2 = (mantL + mantH * 0x1.p+64L) * pow2;
    writefln("%.20e", num2);
+++/
}

__EOF__

void main()
{
    ushort a, b;
    uint   c;

    a =  33221;
    c = 332210;

    // calculate a*10
    //
    //        HI    LO
    // x            10
    //-----------------
    //     10*HI 10*LO
    auto a1 = LO(a) * 10;
    auto a2 = HI(a) * 10;

    auto b1 = LO(a1);
    auto b2 = HI(a1) + a2;

    writeln(   a2, "\t",    a1);
    writeln(   b2, "\t",    b1);
    writeln(HI(c), "\t", LO(c));
}

ubyte  LO(uint u) { return cast(ubyte ) (u & 0xFF); }
ushort HI(uint u) { return cast(ushort) (u >> 8); }


