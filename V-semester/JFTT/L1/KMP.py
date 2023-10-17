#!/usr/bin/env python3
import sys
import numpy as np


# Matches P against itself
def prefix_function(pattern: str, pattern_length: int):
    pi = np.zeros(pattern_length, dtype=int)
    pi[0] = 0
    # pi[q] = length of the longest prefix P that is a
    # proper suffix of P[:q].
    k = 0
    for q in range(1, pattern_length):
        while k > 0 and pattern[k] != pattern[q]:
            k = pi[k]
        if pattern[k] == pattern[q]:
            k += 1
        pi[q] = k
    return pi


# Matches T against P
def kmp_matcher(input: str, pattern: str):
    result = []
    q = 0  # number of characters matched
    n = len(input)
    m = len(pattern)
    pi = prefix_function(pattern, m)
    # scanning the text from left to right
    for i in range(1, n + 1):
        # next character does not match
        while q > 0 and pattern[q] != input[i - 1]:
            q = pi[q - 1]
        # next character matches
        if pattern[q] == input[i - 1]:
            q += 1
        # is all of P matched?
        if q == m:
            result.append(i - m)
            print("Pattern occurs with shift", i - m)
            # look for the next match
            q = pi[q - 1]
    return result


if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Wrong input!")
        exit(-1)

    received_pattern = sys.argv[1]
    file = sys.argv[2]

    with open(file, "r") as f:
        text = f.read()
        output = kmp_matcher(text, received_pattern)
        print(output)
