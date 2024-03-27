public class PolynomialOperations {
    public Polynomial add(Polynomial first, Polynomial second) {
        int maxLength = Math.max(first.coefficients.length, second.coefficients.length);
        double[] resultCoefficients = new double[maxLength];
        int differenceInSize = Math.abs(first.coefficients.length - second.coefficients.length);

        int counter1 = 0;
        int counter2 = 0;
        if (differenceInSize != 0) {
            if (first.coefficients.length >= second.coefficients.length) {
                System.arraycopy(first.coefficients, 0, resultCoefficients, 0, differenceInSize);
                counter1 = differenceInSize;
            }
            else {
                System.arraycopy(second.coefficients, 0, resultCoefficients, 0, differenceInSize);
                counter2 = differenceInSize;
            }
        }

        for (int i = 0; i < maxLength - differenceInSize; i++) {
            resultCoefficients[i + differenceInSize] = first.coefficients[i + counter1] + second.coefficients[i + counter2];
        }

        // Truncating leading zeros
        int counter = 0;
        while (resultCoefficients[counter] == 0.0 && counter != resultCoefficients.length - 1) {
            counter++;
        }

        if (counter != 0) {
            double[] finalCoefficients = new double[maxLength - counter];
            System.arraycopy(resultCoefficients, counter, finalCoefficients, 0, maxLength - counter);
            return new Polynomial(finalCoefficients);
        }

        return new Polynomial(resultCoefficients);
    }

    public Polynomial subtract(Polynomial first, Polynomial second) {
        int maxLength = Math.max(first.coefficients.length, second.coefficients.length);
        double[] resultCoefficients = new double[maxLength];

        for (int i = 0; i < maxLength; i++) {
            double firstCoefficient = (i < first.coefficients.length) ? first.coefficients[i] : 0.0;
            double secondCoefficient = (i < second.coefficients.length) ? second.coefficients[i] : 0.0;
            resultCoefficients[i] = firstCoefficient - secondCoefficient;
        }

        // Truncating leading zeros
        int counter = 0;
        while (resultCoefficients[counter] == 0.0 && counter != resultCoefficients.length - 1) {
            counter++;
        }

        if (counter != 0) {
            double[] finalCoefficients = new double[maxLength - counter];
            System.arraycopy(resultCoefficients, counter, finalCoefficients, 0, maxLength - counter);
            return new Polynomial(finalCoefficients);
        }


        return new Polynomial(resultCoefficients);
    }

    public Polynomial multiply(Polynomial first, Polynomial second) {
        int maxLength = first.coefficients.length + second.coefficients.length - 1;
        double[] resultCoefficients = new double[maxLength];

        for (int i = 0; i < first.coefficients.length; i++) {
            for (int j = 0; j < second.coefficients.length; j++) {
                resultCoefficients[i + j] += first.coefficients[i] * second.coefficients[j];
            }
        }

        return new Polynomial(resultCoefficients);
    }

    public Polynomial[] divide(Polynomial first, Polynomial second) {
        double[] zeroPolynomial = {0};
        Polynomial quotient = new Polynomial(zeroPolynomial);
        Polynomial remainder = new Polynomial(zeroPolynomial);

        Polynomial polynomial = new Polynomial(first.coefficients);

        while (polynomial.getDegree() >= second.getDegree() && polynomial.coefficients[0] != 0.0) {
            int differenceInDegrees = polynomial.getDegree() - second.getDegree() + 1;
            double[] newPolynomialCoefficients = new double[differenceInDegrees];

            newPolynomialCoefficients[0] = polynomial.coefficients[0] / second.coefficients[0];
            for (int i = 1; i < differenceInDegrees; i++) {
                newPolynomialCoefficients[i] = 0.0;
            }

            Polynomial quotientOfTerms = new Polynomial(newPolynomialCoefficients);
            quotient = add(quotient, quotientOfTerms);
            polynomial = subtract(polynomial, multiply(quotientOfTerms, second));
        }

        remainder = polynomial;
        Polynomial[] result = new Polynomial[2];
        result[0] = quotient;
        result[1] = remainder;

        return result;
    }

    public Polynomial gcd(Polynomial first, Polynomial second) {
        if (first.getDegree() < second.getDegree()) {
            return gcd(second, first);
        }
        Polynomial firstCopy = new Polynomial(first.coefficients);
        Polynomial secondCopy = new Polynomial(second.coefficients);

        while (secondCopy.coefficients[0] != 0.0) {
            Polynomial[] divisionResult = divide(firstCopy, secondCopy);

            firstCopy = new Polynomial(secondCopy.coefficients);
            secondCopy = new Polynomial(divisionResult[1].coefficients);
        }

        return firstCopy;
    }

    public Polynomial lcm(Polynomial first, Polynomial second) {
        Polynomial product = multiply(first, second);
        Polynomial gcd = gcd(first, second);
        Polynomial[] lcm = divide(product, gcd);
        return lcm[0];
    }
}
