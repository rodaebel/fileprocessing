"""Efficiently generating huge amounts of test data."""

import StringIO
import random
import sys

CHARS = "".join(chr(i) for i in range(32, 127))

WORDS = [
    ("Hello", 5),
    ("Doctor", 6),
    ("Name", 4),
    ("continue", 8),
    ("yesterday", 9),
    ("Tomorrow", 8)
]

NUM_WORDS = len(WORDS)


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
            if random.randrange(2):
                w, l = WORDS[random.randrange(NUM_WORDS)]
                x = random.randrange(0, length - l - 2)
                print >>buf, "".join(r.sample(chars, x)), w, \
                             "".join(r.sample(chars, length - x - l - 2))
            else:
                print >>buf, "".join(r.sample(chars, length))

        sys.stdout.write(buf.getvalue())

        buf.close()
