#!/usr/bin/env python3
import sys


# Length of the longest prefix of the pattern P
# that is also a suffix of the provided string
def suffix_function(provided_string: str, pattern: str, pattern_length: int):
    result = 0
    for i in range(0, pattern_length):
        if provided_string.endswith(pattern[0:i + 1]):
            result = i + 1
    return result


def finite_automaton_matcher(input: str, pattern: str):
    result = []
    q = 0
    n = len(input)
    m = len(pattern)
    for i in range(1, n + 1):
        q = suffix_function(pattern[0:q] + input[i - 1], pattern, m)
        if q == m:
            result.append(i - m)
            print("Pattern occurs with shift", i - m)
    return result


if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Wrong input!")
        exit(-1)

    received_pattern = sys.argv[1]
    file = sys.argv[2]

    with open(file, "r") as f:
        text = f.read()
        output = finite_automaton_matcher(text, received_pattern)
        print(output)
