import java.security.SecureRandom;
import java.util.Scanner;

public class Tester {
    static SecureRandom random = new SecureRandom();

    public static void main(String[] args) {
        if (args.length != 1) {
            System.out.println("java Tester <mode>");
            return;
        }
        int mode = Integer.parseInt(args[0]);
        Scanner scanner = new Scanner(System.in);
        int size = scanner.nextInt();

        int[] originalKeys = new int[size];
        for (int i = 0; i < size; i++) {
            originalKeys[i] = scanner.nextInt();
        }

        Utils utils = new Utils();
        Tree tree;

        switch (mode) {
            case 1:
                tree = new TreeBST(utils);
                break;
            case 2:
                tree = new TreeRBT(utils);
                break;
            case 3:
                tree = new TreeSplay(utils);
                break;
            default:
                System.out.println("Modes: 1 - BST, 2 - RBT, 3 - Splay");
                return;
        }

        tree.setPrintSize(size);
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
            int randomValue = random.nextInt(0, 2 * size);
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