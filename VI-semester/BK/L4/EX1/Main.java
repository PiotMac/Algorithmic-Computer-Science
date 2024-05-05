import java.util.Arrays;

public class Main {
    public static void main(String[] args) {
        RSA rsa = new RSA();
        long p = 2711;
        long q = 6089;
        // FIRST "USER"
        long[][] keysGenerated1 = rsa.generatePairOfKeys(p, q);
        System.out.println("(sk₁, pk₁) = [(" + keysGenerated1[0][0] + ", " + keysGenerated1[0][1] + "), ("+ keysGenerated1[1][0] + ", " + keysGenerated1[1][1] + ")]");
        // SECOND "USER"
        long[][] keysGenerated2 = rsa.generatePairOfKeys(p, q);
        System.out.println("(sk₂, pk₂) = [(" + keysGenerated2[0][0] + ", " + keysGenerated2[0][1] + "), ("+ keysGenerated2[1][0] + ", " + keysGenerated2[1][1] + ")]");
        // FINDING PRIME NUMBERS p AND q
        System.out.println("Finding prime numbers . . .");
        long[] pAndq = rsa.retrievePrimes(keysGenerated1[0][0], keysGenerated1[1][1], keysGenerated1[0][1]);
        System.out.println("Found prime numbers: (p, q) = (" + pAndq[0] + ", " + pAndq[1] + ")");
        if ((pAndq[0] == p && pAndq[1] == q) || (pAndq[0] == q && pAndq[1] == p)) {
            System.out.println("Successfully generated prime numbers!");
        }
        // USING p AND q TO GET PRIVATE KEY
        long[] privateKey = rsa.extended_gcd(keysGenerated2[1][1], (pAndq[0] - 1) * (pAndq[1] - 1));
        System.out.println("Found sk₂ = (" + keysGenerated1[0][0] + ", " + privateKey[0] + ")");
        if (privateKey[0] == keysGenerated2[0][1]) {
            System.out.println("Successfully cracked private key!");
        }
    }
}