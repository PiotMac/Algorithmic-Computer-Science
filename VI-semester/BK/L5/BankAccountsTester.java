import java.security.SecureRandom;
import java.util.*;

public class BankAccountsTester {
    final int[] digitIndices = {0, 1, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25};
    static SecureRandom random = new SecureRandom();
    final static int[][] numeryRozliczeniowe = {
            {1, 0, 1, 0, 0, 0, 2, 1}, // NBP
            {1, 0, 5, 0, 0, 0, 0, 8}, // ING
            {1, 1, 4, 0, 0, 3, 5, 7}, // mBank
            {1, 0, 3, 0, 0, 0, 0, 2}, // Citi
            {1, 0, 2, 0, 0, 0, 0, 1}  // PKO
    };
    static int COUNT = 0;
    ArrayList<int[]> keys = new ArrayList<>();

    public BankAccountsTester(int count) {
        COUNT = count;
    }

    public Map<Integer, Set<Integer>> generateCombinations() {
        Map<Integer, Set<Integer>> combinations = new HashMap<>();

        for (int i = '0'; i <= '9'; i++) {
            for (int j = '0'; j <= '9'; j++) {
                int x = i ^ j;
                combinations.computeIfAbsent(x, k -> new HashSet<>());
                if (i <= j) {
                    combinations.get(x).add(i);
                    combinations.get(x).add(j);
                }
            }
        }

        return combinations;
    }

    public ArrayList<ArrayList<Integer>> generateAvailableNumbers(int[][] cryptograms, Map<Integer, Set<Integer>> combinations) {
        ArrayList<HashMap<Integer, Boolean>> availableNumbers = new ArrayList<>();
        availableNumbers.ensureCapacity(26);

        for (int i = 0; i < 26; i++) {
            HashMap<Integer, Boolean> map = new HashMap<>();
            for (int j = '0'; j <= '9'; j++) {
                map.put(j, true);
            }
            availableNumbers.add(map);
        }

        int[] cryptogram1 = cryptograms[COUNT];

        for(int i = 0; i < cryptograms.length; i++) {
            if (i == COUNT) {
                continue;
            }
            int[] cryptogram2 = cryptograms[i];

            int[] xored = new int[26];

            for (int k = 0; k < xored.length; k++) {
                xored[k] = cryptogram1[k] ^ cryptogram2[k];
            }

            for(int j = 0; j < xored.length; j++) {
                HashMap<Integer, Boolean> currentMap = availableNumbers.get(j);
                Set<Integer> keysToUpdate = new HashSet<>();
                for (Map.Entry<Integer, Boolean> entry : currentMap.entrySet()) {
                    Integer k = entry.getKey();
                    Boolean available = entry.getValue();
                    if (available && !combinations.get(xored[j]).contains(k)) {
                        keysToUpdate.add(k);
                    }
                }
                for (Integer k : keysToUpdate) {
                    currentMap.put(k, false);
                }
            }
        }

        return getArrayLists(availableNumbers);
    }

    private static ArrayList<ArrayList<Integer>> getArrayLists(ArrayList<HashMap<Integer, Boolean>> availableNumbers) {
        ArrayList<ArrayList<Integer>> tempFilteredNumbers = new ArrayList<>();

        for (HashMap<Integer, Boolean> availableNumber : availableNumbers) {
            ArrayList<Integer> innerList = new ArrayList<>();
            for (Map.Entry<Integer, Boolean> entry : availableNumber.entrySet()) {
                Integer number = entry.getKey();
                Boolean available = entry.getValue();
                if (available) {
                    innerList.add(number);
                }
            }
            tempFilteredNumbers.add(innerList);
        }
        return tempFilteredNumbers;
    }

    public int countPossibleNumbers(ArrayList<ArrayList<Integer>> availableNumbers) {
        int count = 1;

        for(int i = 0; i < 2; i++) {
            if(!availableNumbers.get(i).isEmpty()) {
                count *= availableNumbers.get(i).size();
            }
        }

        for(int i = 10; i < availableNumbers.size(); i++) {
            if(!availableNumbers.get(i).isEmpty()) {
                count *= availableNumbers.get(i).size();
            }
        }

        return count * numeryRozliczeniowe.length;
    }

    public static boolean validateBankAccount(int[] bankAccount) {
        double tmp = 212500;
        for (int j = 2; j <= 9; j++) {
            tmp += (bankAccount[j] * Math.pow(10, (30 - j)));
        }
        for (int j = 10; j < 26; j++) {
            tmp += (bankAccount[j] * Math.pow(10, (30 - j)));
        }
        tmp %= 97;
        tmp = 98 - tmp;

        int firstDigit = ((int) tmp) / 10;
        int secondDigit = ((int) tmp) % 10;

        return firstDigit == bankAccount[0] && secondDigit == bankAccount[1];
    }

    private boolean verifyKey(int[][] cryptograms, int[] key) {
        boolean valid = true;

        for (int i = 0; i < cryptograms.length; i++) {
            if (i == COUNT) {
                continue;
            }
            int[] decryptedMessage = new int[26];

            for (int k = 0; k < decryptedMessage.length; k++) {
                decryptedMessage[k] = cryptograms[i][k] ^ key[k];
            }

            if(!validateBankAccount(decryptedMessage)) {
                valid = false;
                break;
            }
        }

        return valid;
    }

    private void bruteForceUtil(int depth, int[] bankAccount, int[][] cryptograms, ArrayList<int[]> foundBankAccounts, ArrayList<ArrayList<Integer>> possibleNumbers) {
        if(depth == digitIndices.length) {
            for (int i = 0; i < numeryRozliczeniowe.length; i++) {
                System.arraycopy(numeryRozliczeniowe[i], 0, bankAccount, 2, 8);

                if(validateBankAccount(bankAccount)) {
                    int[] key = new int[26];

                    for (int k = 0; k < key.length; k++) {
                        key[k] = cryptograms[COUNT][k] ^ bankAccount[k];
                    }

                    int[] bankAccountCopy = new int[26];
                    System.arraycopy(bankAccount, 0, bankAccountCopy, 0, bankAccount.length);

                    if(verifyKey(cryptograms, key)) {
                        foundBankAccounts.add(bankAccountCopy);
                        keys.add(key);
                    }
                }
            }
        } else {
            for(int i = 0; i < possibleNumbers.get(digitIndices[depth]).size(); i++) {
                int digit = possibleNumbers.get(digitIndices[depth]).get(i) - '0';
                bankAccount[digitIndices[depth]] = digit;
                bruteForceUtil(depth + 1, bankAccount, cryptograms, foundBankAccounts, possibleNumbers);
            }
        }
    }

    public ArrayList<int[]> bruteForce(ArrayList<ArrayList<Integer>> possibleNumbers, int[][] cryptograms) {
        int[] bankAccount = new int[26];
        for (int i = 0; i < 26; i++) {
            bankAccount[i] = 0;
        }

        for (int digitIndex : digitIndices) {
            int digit = possibleNumbers.get(digitIndex).get(0) - '0';
            bankAccount[digitIndex] = digit;
        }

        ArrayList<int[]> foundBankAccounts = new ArrayList<>();
        bruteForceUtil(0, bankAccount, cryptograms, foundBankAccounts, possibleNumbers);

        return foundBankAccounts;
    }

    // Convert string to integer array
    public static int[] stringToIntArray(String str) {
        int[] intArray = new int[str.length()];
        for (int i = 0; i < str.length(); i++) {
            intArray[i] = str.charAt(i) - '0';
        }
        return intArray;
    }
}
