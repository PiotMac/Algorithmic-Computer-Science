import java.security.SecureRandom;

public class Debugger {
    static final int size = 100000;
    static SecureRandom rand = new SecureRandom();
    public static void main(String[] args) {
        Utils utils = new Utils();
        TreeSplay tree = new TreeSplay(utils);
        tree.setPrintSize(size);

        int[] originalKeys = new int[size];

        for (int i = 0; i < size; i++) {
            originalKeys[i] = 2 * i;
        }

        for (int i = 0; i < size; i++) {
            tree.insert(originalKeys[i]);
            if (size <= 50) {
                System.out.println("Inserted: [" + originalKeys[i] + "]");
                tree.print();
            }
        }
        long comparisons = utils.no_comparisons;
        long swaps = utils.no_swaps;
        int height = tree.height();

        for (int i = 0; i < size; i++) {
            int randomValue = rand.nextInt(0, 2 * size);
            tree.delete(randomValue);
            if (size <= 50) {
                System.out.println("Deleted: [" + randomValue + "]");
                tree.print();
            }
        }

        System.out.println("########## AFTER INSERTIONS ##########");
        System.out.println("#comparisons: " + comparisons);
        System.out.println("#swaps = " + swaps);
        System.out.println("height = " + height);

        System.out.println("########## AFTER DELETIONS ##########");
        System.out.println("#comparisons = " + utils.no_comparisons);
        System.out.println("#swaps = " + utils.no_swaps);
        System.out.println("height = " + tree.height());
    }
}
