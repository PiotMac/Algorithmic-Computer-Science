# Description
Programs in this directory are tasked with returning results of coding in terms of uncompressed TGA images. The results are created based on a few JPEG-LS predictors.
The program **TgaMain.java** prints the entropy of the input TGA image (and also the entropy of red, green and blue components). For each predictor, the difference between actual pixel and its prediction is calculated.
In the end, the program prints for each predictor its effectiveness by comparing the entropies returned by each of them.

# Usage
In order to use this program, please write in your terminal: `java TgaMain <inputFile>`
