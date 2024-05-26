import java.security.SecureRandom;
import java.util.*;

public class Main {
    static SecureRandom random = new SecureRandom();
    public static void main(String[] args) {
        //testRC4();
        //testTheSameKey();
        testBankNumbers();
    }

    public static void testRC4() {
        RC4Algorithm rc4 = new RC4Algorithm();
        for (int i = 0; i < 1000; i++) {
            //String keyString = "Testing a key!";
            //int[] key = stringToIntArray(keyString);
            int sizeOfKey = random.nextInt(10, 1000);
            int[] key = new int[sizeOfKey];
            for (int j = 0; j < key.length; j++) {
                key[j] = random.nextInt(128);
            }

            int[] differentKey = Arrays.copyOf(key, sizeOfKey);
            int randomIndex = random.nextInt(sizeOfKey);
            differentKey[randomIndex] = key[randomIndex] ^ 1;

            int sizeOfData = random.nextInt(10, 1000);
            int[] data = new int[sizeOfData];
            for (int k = 0; k < data.length; k++) {
                data[k] = random.nextInt(0, 128);
            }

            int[] cipherText = rc4.useRC4Algorithm(key, data);
            int[] plainText = rc4.useRC4Algorithm(key, cipherText);
            int[] cipherText1 = rc4.useRC4Algorithm(differentKey, data);
            int[] plainText1 = rc4.useRC4Algorithm(differentKey, cipherText1);

            if (!Arrays.equals(data, plainText) || !Arrays.equals(data, plainText1)) {
                throw new AssertionError("Deciphering failed!");
            }
        }
        System.out.println("TESTING RC4: PASSED!");
    }

    public static void testTheSameKey() {
        RC4Algorithm rc4 = new RC4Algorithm();
        for (int i = 1; i <= 100; i++) {
            System.out.println("MESSAGE LENGTH = " + i);
            double percentageSuccessful = 0.0;
            for (int j = 0; j < 100; j++) {
                int sizeOfKey = 256;
                int[] key = new int[sizeOfKey];
                for (int k = 0; k < sizeOfKey; k++) {
                    key[k] = random.nextInt(0, 128);
                }

                int[] differentKey = Arrays.copyOf(key, sizeOfKey);
                int randomIndex = random.nextInt(sizeOfKey);
                differentKey[randomIndex] = key[randomIndex] ^ 1;

                int[] data = new int[i];
                int[] data1 = new int[i];
                int[] data2 = new int[i];
                for (int k = 0; k < i; k++) {
                    data[k] = random.nextInt(0, 128);
                    data1[k] = random.nextInt(0, 128);
                    data2[k] = random.nextInt(0, 128);
                }

                int[] cipherText1 = rc4.useRC4Algorithm(key, data);
                int[] cipherText2 = rc4.useRC4Algorithm(key, data1);
                int[] cipherText3 = rc4.useRC4Algorithm(differentKey, data2);

                if (!usesTheSameKey(cipherText1, cipherText3) && usesTheSameKey(cipherText1, cipherText2)) {
                    percentageSuccessful += 1.0;
                }
            }
            System.out.println("SUCCESSFULLY DETERMINED KEYS = " + percentageSuccessful + "%");
        }
    }

    private static boolean usesTheSameKey(int[] cipherText1, int[] cipherText2) {
        int minLength = Math.min(cipherText1.length, cipherText2.length);
        for (int i = 0; i < minLength; i++) {
            if ((cipherText1[i] ^ cipherText2[i]) >= 0x80) {
                return false;
            }
        }
        return true;
    }

    public static void testBankNumbers() {
        final int MAX_NO_BANK_NUMBERS = 55;

        double percentageGuessed = 0.0;

        String[] bankAccounts = generateBankAccounts(MAX_NO_BANK_NUMBERS);
        int[][] cryptograms = generateCryptograms(bankAccounts);

        int allPotentialAccounts = 0;

        ArrayList<ArrayList<int[]>> foundKeys = new ArrayList<>();

        for (int a = 0; a < MAX_NO_BANK_NUMBERS * BankAccountsTester.numeryRozliczeniowe.length; a++) {//2; a++) {
            BankAccountsTester tester = new BankAccountsTester(a);

            Map<Integer, Set<Integer>> combinations = tester.generateCombinations();
            ArrayList<ArrayList<Integer>> possibleNumbers = tester.generateAvailableNumbers(cryptograms, combinations);

            //int countOfPossibleNumbers = tester.countPossibleNumbers(possibleNumbers);
            //System.out.println(countOfPossibleNumbers);

            ArrayList<int[]> foundBankAccounts = tester.bruteForce(possibleNumbers, cryptograms);
            foundKeys.add(tester.keys);

            allPotentialAccounts += foundBankAccounts.size();

            for (int[] foundBankAccount : foundBankAccounts) {
                for (String bankAccount : bankAccounts) {
                    boolean found = true;
                    int[] intBankAccount = BankAccountsTester.stringToIntArray(bankAccount);
                    for (int i = 0; i < foundBankAccount.length; i++) {
                        if (!(intBankAccount[i] == foundBankAccount[i])) {
                            found = false;
                            break;
                        }
                    }

                    if (found) {
                        System.out.println("Bank account [" + (a + 1) + "]: " + Arrays.toString(foundBankAccount));
                        percentageGuessed += 1.0;
                        break;
                    }
                }
            }
        }

        Set<int[]> commonArrays = findCommonArrays(foundKeys);
        System.out.println("Size of common keys: " + commonArrays.size());
        //for (int[] array : commonArrays) {
        //    System.out.println(Arrays.toString(array));
        //}

        System.out.println("All potential accounts found: " + allPotentialAccounts);
        System.out.println("Chance to guess an account: " + (double) (MAX_NO_BANK_NUMBERS * 5 * 100) / allPotentialAccounts + "%");
        System.out.println("Percentage of original accounts that are contained in these potential accounts: " + (100.0 * percentageGuessed) / (MAX_NO_BANK_NUMBERS * 5.0) + "%");
    }

    public static Set<int[]> findCommonArrays(ArrayList<ArrayList<int[]>> listOfLists) {
        if (listOfLists.isEmpty()) {
            return new HashSet<>();
        }

        // Initialize the set with the first list's arrays
        Set<int[]> commonSet = new HashSet<>(listOfLists.get(0));

        for (int i = 1; i < listOfLists.size(); i++) {
            Set<int[]> currentSet = new HashSet<>(listOfLists.get(i));
            commonSet = retainAll(commonSet, currentSet);
        }

        return commonSet;
    }

    private static Set<int[]> retainAll(Set<int[]> set, Set<int[]> other) {
        Set<int[]> result = new HashSet<>();
        for (int[] array1 : set) {
            for (int[] array2 : other) {
                if (Arrays.equals(array1, array2)) {
                    result.add(array1);
                    break;
                }
            }
        }
        return result;
    }

    public static String[] generateBankAccounts(int n) {
        String[] numeryBankowe = new String[5 * n];
        int outerLoopCounter = 0;

        // Generowanie numerów kont bankowych
        for (int[] numer : BankAccountsTester.numeryRozliczeniowe) {
            for (int i = outerLoopCounter * n; i < n + outerLoopCounter * n; i++) {
                numeryBankowe[i] = "";
                int[] numerKlienta = new int[16];
                for (int j = 0; j < 16; j++) {
                    numerKlienta[j] = random.nextInt(10);
                }
                double tmp = 212500;
                for (int j = 0; j < 8; j++) {
                    tmp += (numer[j] * Math.pow(10, (28 - j)));
                }
                for (int j = 0; j < 16; j++) {
                    tmp += (numerKlienta[j] * Math.pow(10, (20 - j)));
                }
                tmp %= 97;
                tmp = 98 - tmp;
                numeryBankowe[i] = String.format("%02d", (int) tmp);

                for (int j = 0; j < 8; j++) {
                    numeryBankowe[i] += String.valueOf(numer[j]);
                }
                for (int j = 0; j < 16; j++) {
                    numeryBankowe[i] += String.valueOf(numerKlienta[j]);
                }
                if (!BankAccountsTester.validateBankAccount(BankAccountsTester.stringToIntArray(numeryBankowe[i]))) {
                    System.out.println(numeryBankowe[i]);
                }
            }
            outerLoopCounter++;
        }

        return numeryBankowe;
    }

    public static int[][] generateCryptograms(String[] bankAccounts) {
        RC4Algorithm rc4 = new RC4Algorithm();

        int sizeOfKey = 256;//random.nextInt(10, 1000);
        int[] key = new int[sizeOfKey];
        for (int j = 0; j < key.length; j++) {
            key[j] = random.nextInt(128);
        }

        int[][] kryptogramy = new int[bankAccounts.length][];
        int counter = 0;
        for (String numerBankowy : bankAccounts) {
            int[] numerBankowyIntArray = BankAccountsTester.stringToIntArray(numerBankowy);
            kryptogramy[counter] = rc4.useRC4Algorithm(key, numerBankowyIntArray);
            counter++;
        }

        return kryptogramy;
    }
}