# Description
The program encodes and decodes a given TGA image and uses the parameter `k ∈ {1, ..., 7}`, which sets the number of bits for the quantizer.
It also calculates the mean square error (MSE) of the quantization (for the whole image and the RGB components) and the signal-to-noise ratio (SNR).
In my implementation, for each color I used differential coding with uniform quantization.

# Usage
`java Main <encode | decode> <inputFile> <outputFile> <1-7 (in encoding)>`
