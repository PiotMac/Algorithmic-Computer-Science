public class Polynomial {
    double[] coefficients;
    public Polynomial(double[] coefficients) {
        this.coefficients = coefficients;
    }

    public int getDegree() {
        return coefficients.length - 1;
    }

    public void print() {
        for (int i = 0; i < coefficients.length; i++) {
            if (i != coefficients.length - 1) {
                if (coefficients[i] != 0.0) {
                    //if (coefficients[i + 1] > 0) {
                        System.out.print(coefficients[i] + "x^" + (coefficients.length - 1 - i) + "+");
                    //}
                    //else {
                        //System.out.print(coefficients[i] + "x^" + (coefficients.length - 1 - i));
                    //}
                }
            }
            else {
                if (coefficients[i] != 0.0) {
                    System.out.print(coefficients[i]);
                }
            }
        }
        System.out.println();
    }
}
