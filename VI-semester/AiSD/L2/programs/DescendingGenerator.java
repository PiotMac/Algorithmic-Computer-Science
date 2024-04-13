public class DescendingGenerator {
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
        for (int i = size - 1; i >= 0; i--) {
            System.out.println(2 * i);
        }
    }
}
