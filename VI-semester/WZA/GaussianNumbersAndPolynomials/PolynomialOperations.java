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
        Polynomial foundGCD = new Polynomial(first.coefficients);
        Polynomial secondCopy = new Polynomial(second.coefficients);
        Polynomial[] divisionResult = new Polynomial[2];
        while (secondCopy.coefficients[0] != 0.0) {
            divisionResult = divide(foundGCD, secondCopy);

            foundGCD = new Polynomial(secondCopy.coefficients);
            secondCopy = new Polynomial(divisionResult[1].coefficients);
        }

        boolean takeCommonFactor = true;
        if (divisionResult[0].coefficients[0] % 1.0 != 0.0) {
            for (int i = 1; i < divisionResult[0].coefficients.length; i++) {
                if (divisionResult[0].coefficients[0] % 1.0 == 0.0) {
                    break;
                }
                if (divisionResult[0].coefficients[i] % divisionResult[0].coefficients[0] != 0) {
                    takeCommonFactor = false;
                }
            }
        }
        else {
            takeCommonFactor = false;
        }

        if (takeCommonFactor) {
            double[] factor = new double[1];
            factor[0] = divisionResult[0].coefficients[0];
            foundGCD = multiply(foundGCD, new Polynomial(factor));
        }

        return foundGCD;
    }

    public Polynomial gcdOfThree(Polynomial first, Polynomial second, Polynomial third) {
        Polynomial foundGCDOfTwo = gcd(first, second);

        return gcd(foundGCDOfTwo, third);
    }

    public Polynomial[] extendedGCD(Polynomial first, Polynomial second, Polynomial gcd) {
        if (first.getDegree() < second.getDegree()) {
            return extendedGCD(second, first, gcd);
        }
        Polynomial[] result = new Polynomial[3];
        result[0] = new Polynomial(gcd.coefficients);
        double[] o = {1.0};
        double[] p = {0.0};
        result[1] = new Polynomial(o);
        result[2] = new Polynomial(p);

        Polynomial firstCopy = new Polynomial(first.coefficients);
        Polynomial secondCopy = new Polynomial(second.coefficients);

        double[] oS = {1.0};
        double[] z = {0.0};
        double[] oT = {0.0};
        double[] x = {1.0};
        Polynomial oldS = new Polynomial(oS);
        Polynomial s = new Polynomial(z);
        Polynomial oldT = new Polynomial(oT);
        Polynomial t = new Polynomial(x);

        while (secondCopy.coefficients[0] != 0.0) {
            Polynomial[] divideResult = divide(firstCopy, secondCopy);
            Polynomial temp = new Polynomial(firstCopy.coefficients);

            firstCopy = new Polynomial(secondCopy.coefficients);
            secondCopy = subtract(temp, multiply(divideResult[0], secondCopy));

            temp = new Polynomial(oldS.coefficients);
            oldS = new Polynomial(s.coefficients);
            s = subtract(temp, multiply(divideResult[0], s));

            temp = new Polynomial(oldT.coefficients);
            oldT = new Polynomial(t.coefficients);
            t = subtract(temp, multiply(divideResult[0], t));
        }

        Polynomial[] res = divide(gcd, firstCopy);
        if (res[1].coefficients[0] == 0.0) {
            Polynomial copy = new Polynomial(gcd.coefficients);
            Polynomial[] k = divide(copy, firstCopy);
            result[1] = multiply(oldS, k[0]);
            result[2] = multiply(oldT, k[0]);
        }

        return result;
    }

    public Polynomial lcm(Polynomial first, Polynomial second) {
        Polynomial product = multiply(first, second);
        Polynomial gcd = gcd(first, second);
        Polynomial[] lcm = divide(product, gcd);
        return lcm[0];
    }
}
