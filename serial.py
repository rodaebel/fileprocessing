import fileinput
import sys
import time

def main():
    start = time.time()
    c = 0
    data = fileinput.input(sys.argv[1:])
    for line in data:
        c += 1
    print(c)
    print((time.time() - start))

if __name__ == "__main__":
    main()
