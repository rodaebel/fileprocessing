import random
import sys

CHARS = " .0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

if __name__ == "__main__":

    random.seed()
    r = random.Random()

    length = 3 * 80
    chars = CHARS * (length % len(CHARS))

    try:
        n = int(sys.argv[1])
    except IndexError:
        n = 1

    for i in range(n):
        print("".join(r.sample(chars, length)))
