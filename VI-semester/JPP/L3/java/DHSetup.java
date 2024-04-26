import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.HashSet;

public class DHSetup<T extends FiniteFieldMethods> {
    private T generator;
    private T field;

    public DHSetup(T field) throws Exception {
        this.field = field;
        SecureRandom random = new SecureRandom();
        HashSet<Long> chosenNumbers = new HashSet<>();
        ArrayList<Long> primesDividingCharacteristic = findAllPrimes(this.field.getCharacteristic());

        while (chosenNumbers.size() != field.getCharacteristic() - 2) {
            long randomNumber = random.nextLong(2, field.getCharacteristic());
            if (chosenNumbers.contains(randomNumber)) {
                continue;
            }
            chosenNumbers.add(randomNumber);
            if (isGenerator(randomNumber, field.getCharacteristic(), primesDividingCharacteristic)) {
                generator = (T) field.copy();
                generator.setValue(randomNumber);
                break;
            }
        }
    }

    private ArrayList<Long> findAllPrimes(long p) {
        boolean[] allPrimes = new boolean[(int) p + 1];

        for (long i = 0; i <= p; i++) {
            allPrimes[(int) i] = true;
        }
        allPrimes[0] = false;
        allPrimes[1] = false;

        for (int i = 2; (long) i * i <= p; i++) {
            if (allPrimes[i]) {
                for (int j = 2 * i; j <= p; j += i) {
                    allPrimes[j] = false;
                }
            }
        }

        ArrayList<Long> primesDividingCharacteristic = new ArrayList<>();

        for (long i = 2; i < allPrimes.length; i++) {
            if (allPrimes[(int) i] && (p - 1) % i == 0) {
                primesDividingCharacteristic.add(i);
            }
        }

        return primesDividingCharacteristic;
    }

    private boolean isGenerator(long number, long p, ArrayList<Long> primesDividingCharacteristic) throws Exception {
        T a = (T) field.copy();
        a.setValue(number);

        for (Long prime : primesDividingCharacteristic) {
            double y = (double) (p - 1) / prime;
            long x = (p - 1) / prime;
            T result = power(a, (p - 1) / prime);
            if (result.getValue() == 1) {
                return false;
            }
        }

        return true;
    }

    T getGenerator() {
        return generator;
    }

    T power(T a, long b) throws Exception {
        T result = (T) field.copy();
        result.setValue(1);

        T aCopy = (T) a.copy();

        while (b > 0) {
            if (b % 2 == 1) {
                result.multiplyAssign(aCopy);
            }
            aCopy.multiplyAssign(aCopy);
            b /= 2;
        }
        return result;
    }
}
