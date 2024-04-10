public class Main {
    public static void main(String[] args) {
        //testLab1_4();
        //testLab2_1();
        //testLab2_4();
        //testLab2_7();
        testLab2_8();
    }
    public static void testLab1_4() {
        GaussOperations gaussOperations = new GaussOperations();
        GaussNumber a = new GaussNumber(1, 3);
        GaussNumber b = new GaussNumber(3, 4);

        GaussNumber c = gaussOperations.gcd(a, b);
        GaussNumber d = gaussOperations.lcm(a, b);
        System.out.print("c = NWD(a, b) = ");
        c.print();
        System.out.print("d = NWW(a, b) = ");
        d.print();
    }
    public static void testLab2_1() {
        PolynomialOperations polynomialOperations = new PolynomialOperations();
        double[] aCoefficients = {1.0, 0.0, 1.0};
        Polynomial a = new Polynomial(aCoefficients);
        double[] bCoefficients = {1.0, 2.0, 1.0};
        Polynomial b = new Polynomial(bCoefficients);
        Polynomial[] result = polynomialOperations.divide(a, b);
        System.out.print("q = ");
        result[0].print();
        System.out.print("r = ");
        result[1].print();
    }

    public static void testLab2_4() {
        PolynomialOperations polynomialOperations = new PolynomialOperations();
        double[] aCoefficients = {1.0, 0.0, 1.0};
        Polynomial a = new Polynomial(aCoefficients);
        double[] bCoefficients = {1.0, 2.0, 1.0};
        Polynomial b = new Polynomial(bCoefficients);

        Polynomial c = polynomialOperations.gcd(a, b);
        Polynomial d = polynomialOperations.lcm(a, b);

        System.out.print("a = ");
        a.print();
        System.out.print("b = ");
        b.print();
        System.out.print("c = NWD(a, b) = ");
        c.print();
        System.out.print("d = NWW(a, b) = ");
        d.print();
    }

    public static void testLab2_7() {
        PolynomialOperations polynomialOperations = new PolynomialOperations();
        double[] aCoefficients = {1.0, 0.0, 1.0, 0.0, 1.0};
        Polynomial a = new Polynomial(aCoefficients);
        double[] bCoefficients = {1.0, 0.0, -1.0, -2.0, -1.0};
        Polynomial b = new Polynomial(bCoefficients);
        double[] cCoefficients = {1.0, 0.0, 0.0, -1.0};
        Polynomial c = new Polynomial(cCoefficients);

        Polynomial foundGcdOfThree = polynomialOperations.gcdOfThree(a, b, c);

        System.out.print("a = ");
        a.print();
        System.out.print("b = ");
        b.print();
        System.out.print("c = ");
        c.print();
        System.out.print("NWD(a, b, c) = ");
        foundGcdOfThree.print();

        aCoefficients = new double[]{1.0, 1.0, -4.0, -4.0};
        a = new Polynomial(aCoefficients);
        bCoefficients = new double[]{1.0, -1.0, -4.0, 4.0};
        b = new Polynomial(bCoefficients);
        cCoefficients = new double[]{1.0, -2.0, -1.0, 2.0};
        c = new Polynomial(cCoefficients);

        foundGcdOfThree = polynomialOperations.gcdOfThree(a, b, c);

        System.out.print("a = ");
        a.print();
        System.out.print("b = ");
        b.print();
        System.out.print("c = ");
        c.print();
        System.out.print("NWD(a, b, c) = ");
        foundGcdOfThree.print();
    }

    public static void testLab2_8() {
        PolynomialOperations polynomialOperations = new PolynomialOperations();
        double[] aCoefficients = {1.0, -1.0};
        Polynomial a = new Polynomial(aCoefficients);
        double[] bCoefficients = {1.0, 1.0};
        Polynomial b = new Polynomial(bCoefficients);

        Polynomial gcd = polynomialOperations.gcd(a, b);

        Polynomial[] result = polynomialOperations.extendedGCD(a, b, gcd);
        System.out.print("a = ");
        a.print();
        System.out.print("b = ");
        b.print();
        System.out.print("NWD(a, b) = ");
        result[0].print();
        System.out.print("X = ");
        result[1].print();
        System.out.print("Y = ");
        result[2].print();
    }
}