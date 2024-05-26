public class RC4Algorithm {
    public int[] useRC4Algorithm(int[] key, int[] data) {
        int[] s = keySchedulingAlgorithm(key);

        return pseudoRandomGenerationAlgorithm(s, data);
    }
    public int[] keySchedulingAlgorithm(int[] key) {
        int[] s = new int[256];

        for (int i = 0; i < 256; i++) {
            s[i] = i;
        }
        int j = 0;
        for (int i = 0; i < 256; i++) {
            j = (j + s[i] + key[i % key.length]) % 256;
            int temp = s[i];
            s[i] = s[j];
            s[j] = temp;
        }

        return s;
    }

    public int[] pseudoRandomGenerationAlgorithm(int[] s, int[] data) {
        int i = 0;
        int j = 0;
        int[] ciphertext = new int[data.length];
        for (int k = 0; k < data.length; k++) {
            i = (i + 1) % 256;
            j = (j + s[i]) % 256;
            int temp = s[i];
            s[i] = s[j];
            s[j] = temp;
            int t = s[(s[i] + s[j]) % 256];
            ciphertext[k] = data[k] ^ t;
        }
        return ciphertext;
    }
}
