"""Efficiently generating huge amounts of test data."""

import StringIO
import random
import sys

CHARS = "".join(chr(i) for i in range(32, 127))


if __name__ == "__main__":

    random.seed()
    r = random.Random()

    length = 3 * 80
    len_chars = len(CHARS)

    if len_chars < length:
        chars = CHARS * (length / len_chars) + CHARS[:length % len_chars]
    else:
        chars = CHARS

    try:
        n = int(sys.argv[1])
    except IndexError:
        n = 1

    s = 10000
 
    if n / s > 0:
        chunks = [s] * (n / s) + [n % s]
    else:
        chunks = [n]

    for c in chunks: 

        buf = StringIO.StringIO()

        for i in range(c):
            print >>buf, "".join(r.sample(chars, length))

        sys.stdout.write(buf.getvalue())

        buf.close()
