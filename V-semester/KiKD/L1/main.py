import math
import sys


def probability_of_character_ocurring(symbol_counts, character, total_symbol_count):
    return symbol_counts[character] / total_symbol_count


def measure_of_information(symbol_counts, character, total_symbol_count):
    return -math.log2(probability_of_character_ocurring(symbol_counts, character, total_symbol_count))


def entropy(symbol_counts, total_symbol_count):
    output = 0.0
    for i in symbol_counts:
        output += (probability_of_character_ocurring(symbol_counts, i, total_symbol_count)
                   * measure_of_information(symbol_counts, i, total_symbol_count))

    return output


def conditional_probability_of_character_ocurring(symbol_counts, symbol_pairs_counts, prev, current):
    return symbol_pairs_counts[prev][current] / symbol_counts[prev]


def conditional_measure_of_information(symbol_counts, symbol_pairs_counts, prev, current):
    return -math.log2(conditional_probability_of_character_ocurring(symbol_counts, symbol_pairs_counts, prev, current))


def conditional_entropy(symbol_counts, total_symbol_count, symbol_pairs_counts):
    output = 0.0
    for i in symbol_counts:
        output += (probability_of_character_ocurring(symbol_counts, i, total_symbol_count)
                   * sum(conditional_probability_of_character_ocurring(symbol_counts, symbol_pairs_counts, i, j)
                         * conditional_measure_of_information(symbol_counts, symbol_pairs_counts, i, j)
                         for j in symbol_pairs_counts[i]))

    return output


def calculate(file_path):
    # Number of characters in the given file
    total_symbol_count = 0
    # Dictionary to store a count of a symbol
    symbol_counts = {}
    # Dictionary to store counts of symbol pairs
    symbol_pairs_counts = {}
    # Representing a single byte with the value 0
    previous = 0
    symbol_pairs_counts[previous] = {}
    # Open the file in read mode ('r')
    with open(file_path, 'rb') as file:
        # Read the file character by character
        while True:
            char = file.read(1)  # Read one character
            if not char:
                break  # Break the loop if the end of the file is reached

            # Increment the count for the symbol or initialize to 1 if not present
            symbol_counts[char] = symbol_counts.get(char, 0) + 1
            # Initialize if previous character is not present
            if previous not in symbol_pairs_counts:
                symbol_pairs_counts[previous] = {}
            # Increment the count for the pair of symbols or initialize to 1 if not present
            symbol_pairs_counts[previous][char] = symbol_pairs_counts[previous].get(char, 0) + 1
            # Count characters
            total_symbol_count += 1
            # Move to the next pair
            previous = char

    print(f"Entropy: ", entropy(symbol_counts, total_symbol_count))
    print(f"Conditional entropy: ", conditional_entropy(symbol_counts, total_symbol_count, symbol_pairs_counts))

if __name__ == '__main__':
    file_path = sys.argv[1]
    calculate(file_path)