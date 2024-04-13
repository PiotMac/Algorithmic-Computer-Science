import java.security.SecureRandom;

public class LIFO {
    static SecureRandom secureRandom = new SecureRandom();
    public final static int ITERATIONS = 50;
    public static void main(String[] args) {
        Stack stack = new Stack();
        System.out.println("ADDING " + ITERATIONS + " ELEMENTS!");
        int[] addedIntegers = new int[ITERATIONS];
        for (int i = 0; i < ITERATIONS; i++) {
            int value = secureRandom.nextInt(0, 100);
            addedIntegers[i] = value;
            stack.push(value);
        }
        System.out.println();
        System.out.println("REMOVING " + ITERATIONS + " ELEMENTS!");
        int[] removedIntegers = new int[ITERATIONS];
        for (int i = 0; i < ITERATIONS; i++) {
            removedIntegers[i] = stack.pop();
            if (removedIntegers[i] == -1) {
                System.out.println("ERROR: Removing from an empty stack!");
                return;
            }
        }
        System.out.println();
        for (int i = 0; i < ITERATIONS; i++) {
            if (addedIntegers[i] != removedIntegers[ITERATIONS - 1 - i]) {
                System.out.println("Something went wrong!");
                return;
            }
        }
        System.out.println("Success!");
    }
}
