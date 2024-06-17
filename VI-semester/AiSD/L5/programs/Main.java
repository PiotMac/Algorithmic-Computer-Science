import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;

public class Main {
    final static int N_MIN = 100;
    final static int N_MAX = 9000;
    final static int STEP = 100;
    final static int REPS = 10;

    public static void main(String[] args) throws IOException {
        MSTAlgorithms mstAlgorithms = new MSTAlgorithms();
        FileWriter fileWriter = new FileWriter("data/primVSkruskal.csv");
        fileWriter.write("n;prim;kruskal\n");

        for (int n = N_MIN; n <= N_MAX; n += STEP) {
            System.out.println("GENERATING FOR N = " + n);
            double avgPrimTime = 0.0;
            double avgKruskalTime = 0.0;
            for (int i = 1; i <= REPS; i++) {
                Graph graph = new Graph(n);

                ArrayList<Edge> mstEdgesPrim = new ArrayList<>();
                long startTime = System.nanoTime();

                double weight = mstAlgorithms.primAlgorithm(graph, mstEdgesPrim);

                long endTime = System.nanoTime();
                long elapsedTime = endTime - startTime;

                avgPrimTime += elapsedTime / 1000000000.0; // time in s

                ArrayList<Edge> mstEdgesKruskal = new ArrayList<>();

                startTime = System.nanoTime();

                weight = mstAlgorithms.kruskalAlgorithm(graph, mstEdgesKruskal);

                endTime = System.nanoTime();
                elapsedTime = endTime - startTime;

                avgKruskalTime += elapsedTime / 1000000000.0;
            }
            avgPrimTime /= REPS;
            avgKruskalTime /= REPS;
            System.out.println("AVG TIME (PRIM)    = " + avgPrimTime);
            System.out.println("AVG TIME (KRUSKAL) = " + avgKruskalTime);
            fileWriter.write(n + ";" + avgPrimTime + ";" + avgKruskalTime + "\n");
        }

        fileWriter.close();
    }
}