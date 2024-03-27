public class GaussNumber {
    int realNumber;
    int imaginaryNumber;
    public GaussNumber(int real, int imaginary) {
        realNumber = real;
        imaginaryNumber = imaginary;
    }

    // N(x) = N(a + bi) = a^2 + b^2
    public int norm() {
        return (realNumber * realNumber) + (imaginaryNumber * imaginaryNumber);
    }

    public void print() {
        System.out.print(realNumber);
        if (imaginaryNumber < 0) {
            System.out.print("-");
        }
        else {
            System.out.print("+");
        }
        if (imaginaryNumber == 1 || imaginaryNumber == -1) {
            System.out.println("i");
        }
        else {
            System.out.println(Math.abs(imaginaryNumber) + "i");
        }
    }
}
