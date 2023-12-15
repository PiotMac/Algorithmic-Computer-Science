# Description
Programs in the `src` directory are responsible for qunatization of uncompressed TGA images. The program **QuantizationMain.java** take as arguments:
1. path to the input TGA file that will be quantized
2. destination of the quantized image
3. an integer 'x' between 0 and 24 indicating to use $2^x$ colors in the quantization process

To achieve the specified number of colors the program uses the Linde-Buzo-Gray algorithm (for simplicity I used Manhattan geometry).
The program also prints the mean square error (MSE) of the quantization and the signal-to-noise ratio (SNR).

For testing, I also created the **QuantizationTesting.java** file where after writing certain parameters the program tests all the images from the `tests` directory,
generates `data.csv` file in the `output` directory along with the quantized images (their filenames take form of `<integer from point 3.><inputFile>`).

# Usage
In order to use this program, please write in your terminal: `java QuantizationMain <inputFile> <outputFile> <0-24>`
