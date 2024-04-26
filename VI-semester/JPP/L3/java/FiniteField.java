public class FiniteField extends FiniteFieldMethods {
    private final long characteristic;
    private long value;

    public FiniteField(long primeNumber, long value) throws Exception {
        if (isPrime(primeNumber)) {
            characteristic = primeNumber;
        }
        else {
            throw new Exception("The received number in the constructor is NOT a prime number!");
        }
        this.value = (value % characteristic + characteristic) % characteristic;
    }

    private boolean isPrime(long number) {
        if (number <= 1) {
            return false;
        }
        if (number == 2) {
            return true;
        }
        if (number % 2 == 0) {
            return false;
        }
        for (long i = 3; i <= Math.sqrt(number); i += 2) {
            if (number % i == 0) {
                return false;
            }
        }
        return true;
    }

    public long getCharacteristic() {
        return characteristic;
    }
    public void setValue(long value) { this.value = value; }
    public long getValue() { return value; }

    // ################### COMPARING OPERATORS ###################
    public boolean isEqual(FiniteFieldMethods other) {
        return this.value == other.getValue();
    }

    public boolean isNotEqual(FiniteFieldMethods other) {
        return this.value != other.getValue();
    }

    public boolean isLessOrEqual(FiniteFieldMethods other) {
        return this.value <= other.getValue();
    }

    public boolean isGreaterOrEqual(FiniteFieldMethods other) {
        return this.value >= other.getValue();
    }

    public boolean isLess(FiniteFieldMethods other) {
        return this.value < other.getValue();
    }

    public boolean isGreater(FiniteFieldMethods other) {
        return this.value > other.getValue();
    }

    // ################### ARITHMETIC OPERATORS ###################
    public FiniteFieldMethods add(FiniteFieldMethods other) throws Exception {
        return new FiniteField(characteristic, (this.value + other.getValue()) % characteristic);
    }

    public FiniteFieldMethods subtract(FiniteFieldMethods other) throws Exception {
        long result = (this.value - other.getValue()) % characteristic;
        if (result < 0) {
            result += characteristic;
        }
        return new FiniteField(characteristic, result % characteristic);
    }

    public FiniteFieldMethods multiply(FiniteFieldMethods other) throws Exception {
        return new FiniteField(characteristic, (this.value * other.getValue()) % characteristic);
    }

    private long[] extended_gcd(long a, long b) {
        if (a == 0) {
            return new long[]{0, 1, b};
        }

        long[] gcd = extended_gcd(b % a, a);
        long x, y;
        x = gcd[1] - (b / a) * gcd[0];
        y = gcd[0];

        return new long[]{x, y, gcd[2]};
    }

    private long mod_invert(long a, long m) {
        long[] gcd = extended_gcd(a, m);

        if (gcd[2] != 1) {
            // Modular inverse does not exist
            return -1;
        } else {
            return modulo(gcd[0], m);
        }
    }

    public FiniteFieldMethods divide(FiniteFieldMethods other) throws Exception {
        long inv = mod_invert(other.getValue(), other.getCharacteristic());
        if (inv == -1) {
            throw new ArithmeticException("Modular inverse does not exist!");
        }
        FiniteField inverse = new FiniteField(characteristic, inv);
        return multiply(inverse);
        //inv = assign(inv);

        //return modulo(multiply(left, inv), characteristic);
    }

    public long modulo(long left, long right) {
        return (left % right + characteristic) % characteristic;
    }

    // ################### ASSIGNING OPERATORS ###################

    public void assign(FiniteFieldMethods other) {
        this.value = other.getValue();
    }

    public void addAssign(FiniteFieldMethods other) throws Exception {
        this.value = add(other).getValue();
    }

    public void subtractAssign(FiniteFieldMethods other) throws Exception {
        this.value = subtract(other).getValue();
    }

    public void multiplyAssign(FiniteFieldMethods other) throws Exception {
        this.value = multiply(other).getValue();
    }

    public void divideAssign(FiniteFieldMethods other) throws Exception {
        this.value = divide(other).getValue();
    }

    @Override
    public FiniteFieldMethods copy() {
        try {
            return new FiniteField(this.characteristic, this.value);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}
