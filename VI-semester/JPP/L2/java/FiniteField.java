public class FiniteField {
    private final int characteristic;
    private int value;
    public FiniteField(int primeNumber, int value) throws Exception {
        if (isPrime(primeNumber)) {
            characteristic = primeNumber;
        }
        else {
            throw new Exception("The received number in the constructor is NOT a prime number!");
        }
        this.value = (value % characteristic + characteristic) % characteristic;
    }

    private boolean isPrime(int number) {
        if (number <= 1) {
            return false;
        }
        if (number == 2) {
            return true;
        }
        if (number % 2 == 0) {
            return false;
        }
        for (int i = 3; i <= Math.sqrt(number); i += 2) {
            if (number % i == 0) {
                return false;
            }
        }
        return true;
    }

    public int getCharacteristic() {
        return characteristic;
    }
    public int getValue() { return value; }

    // ################### COMPARING OPERATORS ###################
    public boolean isEqual(FiniteField other) {
        return this.value == other.value;
    }

    public boolean isNotEqual(FiniteField other) {
        return this.value != other.value;
    }

    public boolean isLessOrEqual(FiniteField other) {
        return this.value <= other.value;
    }

    public boolean isGreaterOrEqual(FiniteField other) {
        return this.value >= other.value;
    }

    public boolean isLess(FiniteField other) {
        return this.value < other.value;
    }

    public boolean isGreater(FiniteField other) {
        return this.value > other.value;
    }

    // ################### ARITHMETIC OPERATORS ###################
    public FiniteField add(FiniteField other) throws Exception {
        return new FiniteField(characteristic, (this.value + other.value) % characteristic);
    }

    public FiniteField subtract(FiniteField other) throws Exception {
        int result = (this.value - other.value) % characteristic;
        if (result < 0) {
            result += characteristic;
        }
        return new FiniteField(characteristic, result % characteristic);
    }

    public FiniteField multiply(FiniteField other) throws Exception {
        int first = this.value % characteristic;
        int second = other.value % characteristic;
        int result = first;
        for(int i = 1; i < second; i++) {
            result += first;
            result = ((result % characteristic) + characteristic) % characteristic;
        }
        return new FiniteField(characteristic, result);
    }

    private int[] extended_gcd(int a, int b) {
        if (a == 0) {
            return new int[]{0, 1, b};
        }

        int[] gcd = extended_gcd(b % a, a);
        int x, y;
        x = gcd[1] - (b / a) * gcd[0];
        y = gcd[0];

        return new int[]{x, y, gcd[2]};
    }

    private int mod_invert(int a, int m) {
        int[] gcd = extended_gcd(a, m);

        if (gcd[2] != 1) {
            // Modular inverse does not exist
            return -1;
        } else {
            return modulo(gcd[0], m);
        }
    }

    public FiniteField divide(FiniteField other) throws Exception {
        int inv = mod_invert(other.value, other.characteristic);
        if (inv == -1) {
            throw new ArithmeticException("Modular inverse does not exist!");
        }
        FiniteField inverse = new FiniteField(characteristic, inv);
        return multiply(inverse);
        //inv = assign(inv);

        //return modulo(multiply(left, inv), characteristic);
    }

    public int modulo(int left, int right) {
        return (left % right + characteristic) % characteristic;
    }

    // ################### ASSIGNING OPERATORS ###################

    public FiniteField assign(FiniteField other) throws Exception {
        return new FiniteField(characteristic, other.value);
    }

    public FiniteField addAssign(FiniteField other) throws Exception {
        return new FiniteField(characteristic, add(other).value);
    }

    public FiniteField subtractAssign(FiniteField other) throws Exception {
        return new FiniteField(characteristic, subtract(other).value);
    }

    public FiniteField multiplyAssign(FiniteField other) throws Exception {
        return new FiniteField(characteristic, multiply(other).value);
    }

    public FiniteField divideAssign(FiniteField other) throws Exception {
        return new FiniteField(characteristic, divide(other).value);
    }
}
