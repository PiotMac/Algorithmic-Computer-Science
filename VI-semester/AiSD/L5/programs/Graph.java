import java.util.ArrayList;
import java.util.Random;

public class Graph {
    final Random random;
    int no_nodes;
    int[] vertices;
    ArrayList<Edge> edges;
    ArrayList<ArrayList<Edge>> adjacencyList;

    public Graph(int n) {
        if (n <= 0) {
            throw new IllegalArgumentException("Number of nodes must be positive");
        }
        no_nodes = n;
        vertices = new int[n];
        for (int i = 0; i < no_nodes; i++) {
            vertices[i] = i + 1;
        }
        edges = new ArrayList<>();
        random = new Random();

        adjacencyList = new ArrayList<>();
        for (int i = 0; i <= no_nodes; i++) {
            adjacencyList.add(new ArrayList<>());
        }

        for (int u : vertices) {
            for (int v : vertices) {
                if (u < v) {
                    double weight = random.nextDouble();
                    while (weight == 0.0) {
                        weight = random.nextDouble();
                    }
                    Edge e = new Edge(u, v, weight);
                    edges.add(e);
                    adjacencyList.get(u).add(e);
                    adjacencyList.get(v).add(new Edge(v, u, weight));
                }
            }
        }
    }

}
