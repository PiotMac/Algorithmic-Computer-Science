public class Utils {
    long no_comparisons = 0;
    long no_swaps = 0;
    public boolean compare(int a, int b, int mode) {
        no_comparisons++;
        return switch (mode) {
            case 1 -> a > b;
            case 2 -> a < b;
            case 3 -> a >= b;
            case 4 -> a <= b;
            case 5 -> a == b;
            case 6 -> a != b;
            default -> throw new IllegalArgumentException();
        };
    }
    public void registerSwap() {
        no_swaps++;
    }

    public void resetCounters() {
        no_comparisons = 0;
        no_swaps = 0;
    }
}
