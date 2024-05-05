import java.security.SecureRandom;

public class RSA {
    private static final SecureRandom random = new SecureRandom();

    public long[][] generatePairOfKeys(long p, long q) {
        if (!isPrime(p) || !isPrime(q)) {
            throw new IllegalArgumentException("One of the numbers is not a prime number!");
        }

        if (p == q) {
            throw new IllegalArgumentException("Prime numbers p and q have to be different!");
        }

        long n = p * q;
        long phi = (p - 1) * (q - 1);

        long e;
        long[] extGCD;
        do {
            e = random.nextLong(2, phi);
            extGCD = extended_gcd(e, phi);
        } while (gcd(e, phi) != 1 || extGCD[0] < 0);

        long d = extGCD[0];

        long[][] keys = new long[2][2];
        // PUBLIC KEY
        keys[0][0] = n;
        keys[0][1] = e;

        // PRIVATE KEY
        keys[1][0] = n;
        keys[1][1] = d;

        return keys;
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

    private long gcd(long a, long b) {
        if (b > a) {
            long temp = a;
            a = b;
            b = temp;
        }

        long r;
        while (b != 0) {
            r = a % b;
            a = b;
            b = r;
        }
        return a;
    }

    public long[] extended_gcd(long a, long b) {
        long[] result = new long[2];

        long old_r = a, r = b;
        long old_s = 1, s = 0;
        long old_t = 0, t = 1;

        while (r != 0) {
            long quotient = old_r / r;
            long temp = old_r;

            old_r = r;
            r = temp - quotient * r;

            temp = old_s;
            old_s = s;
            s = temp - quotient * s;

            temp = old_t;
            old_t = t;
            t = temp - quotient * t;
        }

        if (old_r == 1) {
            if (old_s < 0) {
                old_s += b;
            }
            if (old_t < 0) {
                old_t += b;
            }
            result[0] = old_s;
            result[1] = old_t;
        }

        return result;
    }

    public long[] retrievePrimes(long n, long e, long d) {
        long kphi = d * e - 1;
        long t = kphi;
        long[] result = new long[2];

        while (t % 2 == 0) {
            t = (long) Math.floor((double) t / 2.0);
        }

        long a = 2;
        while (a < n) {
            long k = t;
            StringBuilder kToBits = new StringBuilder();
            kToBits.append(Long.toBinaryString(k));

            while (k < kphi) {
                long x = a;
                for (int i = kToBits.length() - 1; i >= 0; i--) {
                    x = (x * x) % n;
                    if (kToBits.charAt(i) == '1') {
                        x = (x * a) % n;
                    }
                }

                if (x != t && x != (n - 1) && (x * x) % n == 1) {
                    result[0] = gcd(x - 1, n);
                    break;
                }
                k *= 2;
                kToBits.append("0");
            }
            a += 2;
        }

        result[1] = n / result[0];

        return result;
    }
}
