/*
 * For testing string-to-real convertion routines.
 *
-------------------
# portmaster math/gmp

% cc mpfbits.c -I /usr/local/include -L /usr/local/lib -l gmp -o mpfbits
% ./mpfbits 0.123
0x1.f7ced916872b020c49ba5e353f7ced916872b020c49ba5e353f7ced916872b0p-4
% _
-------------------
 */

#ifndef __FreeBSD__
#error "This program requires strtonum(), a FreeBSD extension."
#endif

#include <stdio.h>
#include <stdlib.h>
#include <sysexits.h>
#include <unistd.h>

#include <gmp.h>


/*
 * Default settings:
 */
enum
{
    DEFAULT_PRECISION = 256,    // MPF precision in bits
    MAX_PRECISION     = 65536,
    MAX_BASE          = 36,
};

static void print_mpf(const char *mpfstr, unsigned base, mp_bitcnt_t precision);
static void show_usage(FILE* out);


int main(int argc, char *argv[])
{
    mp_bitcnt_t precision = DEFAULT_PRECISION;
    unsigned    base      = 10;
    int         optchar;
    size_t      i;

    /*
     * Get options
     */
    while ((optchar = getopt(argc, argv, "p:b:h")) != -1)
    {
        switch (optchar)
        {
            const char *errstr; /* Set by strtonum() */

            case 'p':
                precision = (mp_bitcnt_t) strtonum(optarg, 1, MAX_PRECISION, &errstr);
                if (errstr)
                {
                    fprintf(stderr, "The precition option is %s: %s\n", errstr, optarg);
                    goto L_error_invalid_usage;
                }
                break;

            case 'b':
                base = (unsigned) strtonum(optarg, 2, MAX_BASE, &errstr);
                if (errstr)
                {
                    fprintf(stderr, "The base option is %s: %s\n", errstr, optarg);
                    goto L_error_invalid_usage;
                }
                break;

            case 'h':
                show_usage(stdout);
                goto L_success;

            case '?': /* missing optarg */
            default:
                goto L_error_invalid_usage;
        }
    }
    argc -= optind;
    argv += optind;

    /*
     * Prints specified floating-point numbers in the %a format.
     */
    if (argc < 1)
    {
        fputs("At least one number argument is required\n", stderr);
        goto L_error_invalid_usage;
    }

    for (i = 0; i < argc; ++i)
        print_mpf(argv[i], base, precision);

L_success:
    return EXIT_SUCCESS;

L_error_invalid_usage:
    show_usage(stderr);
    return EX_USAGE;
}


static void print_mpf(const char *mpfstr, unsigned base, mp_bitcnt_t precision)
{
    const unsigned formatprec = (precision >= 4 ? precision - 4 : 0) / 4;
    mpf_t          num;

    mpf_init2(num, precision);

    if (mpf_set_str(num, mpfstr, base) < 0)
    {
        (void) fprintf(stderr, "GMP mpf_set_str() failed for the number '%s'\n", mpfstr);
        exit(EX_DATAERR);
    }

    if (gmp_printf("%.*Fa\n", formatprec, num) < 0)
    {
        (void) fprintf(stderr, "GMP gmp_printf() failed for the number '%s'\n", mpfstr);
        exit(EX_IOERR);
    }

    mpf_clear(num);
}


static void show_usage(FILE *out)
{
    static const char const *usageLines[] =
    {
        "Usage: mpfbits [options...] number(s)...",
        "",
        "Options:",
        "  -p nbits     Set the precision to nbits (1 <= nbits <= 65536)",
        "  -b base      Use base as the numeral base (2 <= base <= 36)",
        "  -h           Print this message and exit"
    };
    enum { NLINES = sizeof(usageLines) / sizeof(char*) };
    size_t i;

    for (i = 0; i < NLINES; ++i)
    {
        if (fprintf(out, "%s\n", usageLines[i]) < 0)
        {
            perror("could not print usage");
            exit(EX_IOERR);
        }
    }
}

