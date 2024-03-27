// x = (a + bi), y = (c + di)
public class GaussOperations {
    // (a + bi) + (c + di) = (a + c) + (b + d)i
    public GaussNumber add(GaussNumber first, GaussNumber second) {
        return new GaussNumber(first.realNumber + second.realNumber,
                first.imaginaryNumber + second.imaginaryNumber);
    }

    // (a + bi) - (c + di) = (a - c) + (b - d)i
    public GaussNumber subtract(GaussNumber first, GaussNumber second) {
        return new GaussNumber(first.realNumber - second.realNumber,
                first.imaginaryNumber - second.imaginaryNumber);
    }

    // (a + bi) * (c + di) = (ac - bd) + (ad + bc)i
    public GaussNumber multiply(GaussNumber first, GaussNumber second) {
        return new GaussNumber(first.realNumber * second.realNumber - first.imaginaryNumber * second.imaginaryNumber,
                first.realNumber * second.imaginaryNumber + first.imaginaryNumber * second.realNumber);
    }

    // (a + bi) / (c + di) = (ac + bd) / N(y) + (bc - ad)i / N(y)
    public GaussNumber[] divide(GaussNumber first, GaussNumber second) {
        // (ac + bd) / N(y)
        double toRound = (double) (first.realNumber * second.realNumber + first.imaginaryNumber * second.imaginaryNumber) / (double) second.norm();
        int realQuotient = (int) Math.round(toRound);
        // (bc - ad) / N(y)
        toRound = (double) (first.imaginaryNumber * second.realNumber - first.realNumber * second.imaginaryNumber) / (double) second.norm();
        int imaginaryQuotient = (int) Math.round(toRound);
        GaussNumber quotient = new GaussNumber(realQuotient, imaginaryQuotient);

        // (a + bi) - (q1 + q2 * i)(c + di) = r1 + r2 * i
        GaussNumber remainder = subtract(first, multiply(quotient, second));

        GaussNumber[] result = new GaussNumber[2];
        result[0] = quotient;
        result[1] = remainder;

        return result;
    }

    public GaussNumber gcd(GaussNumber first, GaussNumber second) {
        if (first.norm() < second.norm()) {
            return gcd(second, first);
        }
        GaussNumber firstCopy = new GaussNumber(first.realNumber, first.imaginaryNumber);
        GaussNumber secondCopy = new GaussNumber(second.realNumber, second.imaginaryNumber);
        while (true) {
            GaussNumber[] divisionResult = divide(firstCopy, secondCopy);
            if (divisionResult[1].realNumber == 0 && divisionResult[1].imaginaryNumber == 0) {
                return secondCopy;
            }

            firstCopy = new GaussNumber(secondCopy.realNumber, secondCopy.imaginaryNumber);
            secondCopy = new GaussNumber(divisionResult[1].realNumber, divisionResult[1].imaginaryNumber);
        }
    }

    public GaussNumber lcm(GaussNumber first, GaussNumber second) {
        GaussNumber gcd = gcd(first, second);
        GaussNumber product = multiply(first, second);
        GaussNumber[] lcm = divide(product, gcd);
        return lcm[0];
    }
}
