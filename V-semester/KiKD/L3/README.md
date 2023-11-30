# Description
These programs in this directory are related to Lempel–Ziv–Welch (LZW) - universal lossless data compression algorithm (**LZW.java**), and also to universal codes such as:
1. Elias γ code (**GammaCoding.java**)
2. Elias δ code (**DeltaCoding.java**)
3. Elias ω code (**OmegaCoding.java**)
4. Fibonacci code (**FibonacciCoding.java**)
# Usage
In order to use these programs, please write in your terminal: `java Main <encode|decode> <d|g|o|f> <input file> <output file>`, where:
1. `d` option uses Elias γ codes
2. `g` option uses Elias δ codes
3. `o` option uses Elias ω codes
4. `f` option uses Fibonacci codes

When not given any explicit option, the progran uses Elias ω codes as default.
