import java.security.SecureRandom;

public class LongestCommonSubsequence {
    SecureRandom random = new SecureRandom();
    int SIZE = 50;

    public static void main(String[] args) {
        LongestCommonSubsequence lcs = new LongestCommonSubsequence();
        //String X = lcs.randomStringGenerator(lcs.SIZE);
        //String Y = lcs.randomStringGenerator(lcs.SIZE);
        String X = "exponential";
        String Y = "polynomial";
        int n = X.length();
        int m = Y.length();
        int result = lcs.lcsAlgorithm(X, Y, n, m);

        System.out.println("X: " + X);
        System.out.println("Y: " + Y);
        System.out.println("LCS: " + result);
    }

    public int lcsAlgorithm(String X, String Y, int n, int m) {
        int L[][] = new int[n + 1][m + 1];
        for (int i = 0; i <= n; i++) {
            for (int j = 0; j <= m; j++) {
                if (i == 0 || j == 0) {
                    L[i][j] = 0;
                }
                else if (X.charAt(i - 1) == Y.charAt(j - 1)) {
                    L[i][j] = L[i - 1][j - 1] + 1;
                }
                else {
                    L[i][j] = Math.max(L[i - 1][j], L[i][j - 1]);
                }
            }
        }
        return L[n][m];
    }

    public String randomStringGenerator(int size) {
        String output = "";
        char a = 'a';
        char z = 'z';
        for (int i = 0; i < size; i++) {
            int decimal = random.nextInt(a, z);
            char letter = (char) decimal;
            output = output.concat(String.valueOf(letter));
        }
        return output;
    }
}