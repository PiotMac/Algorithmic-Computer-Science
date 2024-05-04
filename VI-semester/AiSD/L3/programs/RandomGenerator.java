import java.security.SecureRandom;

public class RandomGenerator {
    static SecureRandom secureRandom = new SecureRandom();

    public static void main(String[] args) {
        int size;
        try {
            size = Integer.parseInt(args[0]);
        }
        catch (NumberFormatException ex) {
            System.out.println("Wrong size input!");
            return;
        }
        if (size <= 0) {
            System.out.println("Size has to be bigger than zero!");
            return;
        }
        System.out.println(size);
        System.out.println(secureRandom.nextInt(1, size + 1));
        for (int i = 0; i < size; i++) {
            System.out.println(secureRandom.nextInt(0, 2 * size));
        }
    }
}

