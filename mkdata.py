import StringIO
import random
import sys

CHARS = " .0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

if __name__ == "__main__":

    random.seed()
    r = random.Random()

    length = 3 * 80
    chars = CHARS * (length / len(CHARS)) + CHARS[:length % len(CHARS)]

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
