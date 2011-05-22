import fileinput
import sys

def main():
    c = 0
    data = fileinput.input(sys.argv[1:])
    for line in data:
        c += 1
    print(c)

if __name__ == "__main__":
    main()
