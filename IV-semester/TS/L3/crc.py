from zlib import crc32
from math import ceil
import re
import filecmp

FRAME = '01111110'


def convert_string_to_bytes(information: str) -> bytes:
    information = information.ljust(ceil(len(information) / 8) * 8, '0')
    output = []

    for i in range(0, int(len(information) / 8)):
        b = 0
        for j in range(0, 8):
            if information[i * 8 + j] == '1':
                b += 2**(7-j)
        output.append(b)

    return bytes(output)


def convert_bytes_to_string(information: bytes) -> str:
    output = ''
    for byte in information:
        string = bin(byte)[2:].ljust(8, '0')
        output += string

    return output


def crc_adder(information: str) -> str:
    crc = crc32(convert_string_to_bytes(information))
    return convert_bytes_to_string(crc.to_bytes(4, 'big'))


def encode(information: str) -> str:
    information += crc_adder(information)
    information = re.sub('11111', '111110', information)

    return FRAME + information + FRAME


def decode(information: str) -> str:
    information = re.sub(FRAME, '', information)
    information = re.sub('111110', '11111', information)

    # verify the CRC
    crc = information[-32:]

    if crc != crc_adder(information[:-32]):
        raise Exception('invalid CRC => invalid frame')

    return information[:-32]


if __name__ == '__main__':
    input_file = 'input.txt'
    encoded_file = 'encoded.txt'
    decoded_file = 'decoded.txt'

    with open(input_file, 'r') as fin:
        data = fin.readline().replace('\n', '')
        parsed = encode(data)
        with open(encoded_file, 'w+') as fout:
            fout.write(parsed)
            fout.write('\n')

    with open(encoded_file, 'r') as fin:
        data = fin.readline().replace('\n', '')
        parsed = decode(data)
        with open(decoded_file, 'w+') as fout:
            fout.write(parsed)

    if filecmp.cmp(input_file, decoded_file):
        print("Decoding successful!\n")
