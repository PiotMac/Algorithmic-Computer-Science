import java.security.SecureRandom;

public class FIFO {
    static SecureRandom secureRandom = new SecureRandom();
    public final static int ITERATIONS = 50;
    public static void main(String[] args) {
        Queue queue = new Queue();
        System.out.println("ADDING " + ITERATIONS + " ELEMENTS!");
        int[] addedIntegers = new int[ITERATIONS];
        for (int i = 0; i < ITERATIONS; i++) {
            int value = secureRandom.nextInt(0, 100);
            addedIntegers[i] = value;
            queue.addItem(value);
        }
        System.out.println();
        System.out.println("REMOVING " + ITERATIONS + " ELEMENTS!");
        int[] removedIntegers = new int[ITERATIONS];
        for (int i = 0; i < ITERATIONS; i++) {
            removedIntegers[i] = queue.removeItem();
            if (removedIntegers[i] == -1) {
                System.out.println("ERROR: Removing from an empty queue!");
                return;
            }
        }
        System.out.println();
        for (int i = 0; i < ITERATIONS; i++) {
            if (addedIntegers[i] != removedIntegers[i]) {
                System.out.println("Something went wrong!");
                return;
            }
        }
        System.out.println("Success!");
    }
}