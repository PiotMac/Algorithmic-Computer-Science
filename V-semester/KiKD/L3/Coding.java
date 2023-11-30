import java.util.List;

public abstract class Coding {
    public static int encodedFileBytes = 0;
    public abstract String encodeNumber(int number);
    public abstract void encode(List<Integer> numbers, String outputFile);
    public abstract List<Integer> decode(String inputFile);
}
