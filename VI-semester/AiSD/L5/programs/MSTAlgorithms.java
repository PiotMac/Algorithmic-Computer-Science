import java.util.ArrayList;
import java.util.Comparator;

public class MSTAlgorithms {
    public double primAlgorithm(Graph graph, ArrayList<Edge> mstEdges) {
        double[] keys = new double[graph.no_nodes + 1];
        int[] parents = new int[graph.no_nodes + 1];
        boolean[] visited = new boolean[graph.no_nodes + 1];
        double[] weights = new double[graph.no_nodes + 1];

        for (int i = 0; i <= graph.no_nodes; i++) {
            keys[i] = Double.MAX_VALUE;
            parents[i] = -1;
            visited[i] = false;
            weights[i] = 0.0;
        }

        keys[1] = 0;

        double totalWeight = 0.0;

        for (int i = 0; i < graph.no_nodes; i++) {
            int minKey = 1;
            double minWeight = Double.MAX_VALUE;
            for (int j = 1; j <= graph.no_nodes; j++) {
                if (!visited[j] && keys[j] < minWeight) {
                    minWeight = keys[j];
                    minKey = j;
                }
            }

            visited[minKey] = true;

            for (Edge edge : graph.adjacencyList.get(minKey)) {
                if (!visited[edge.destination] && edge.weight < keys[edge.destination]) {
                    parents[edge.destination] = minKey;
                    weights[edge.destination] = edge.weight;
                    keys[edge.destination] = edge.weight;
                }
            }
        }

        for (int i = 1; i <= graph.no_nodes; i++) {
            if (parents[i] != -1) {
                for (Edge edge : graph.adjacencyList.get(parents[i])) {
                    if (edge.destination == i && edge.weight == weights[i]) {
                        mstEdges.add(edge);
                        totalWeight += edge.weight;
                        break;
                    }
                }
            }
        }

        return totalWeight;
    }

    public double kruskalAlgorithm(Graph graph, ArrayList<Edge> mstEdges) {
        double totalWeight = 0.0;
        int[] parent = new int[graph.no_nodes + 1];
        int[] rank = new int[graph.no_nodes + 1];

        graph.edges.sort(new Comparator<Edge>() {
            @Override
            public int compare(Edge e1, Edge e2) {
                return Double.compare(e1.weight, e2.weight);
            }
        });

        // Performing makeSet(v) procedure
        // For each vertex:
        // parent(v) = v
        // rank(v) = 0
        for (int vertex : graph.vertices) {
            parent[vertex] = vertex;
            rank[vertex] = 0;
        }

        int e = 0;
        int i = 0;

        // We want a MST and we work on an undirected, connected graph
        // That is why |E| = |V| - 1
        while (e < graph.no_nodes - 1) {
            Edge edge = graph.edges.get(i);
            i += 1;

            int x = find(parent, edge.source);
            int y = find(parent, edge.destination);

            if (x != y) {
                e++;
                mstEdges.add(edge);
                totalWeight += edge.weight;
                union(parent, rank, x, y);
            }
        }

        return totalWeight;
    }

    private static int find(int[] parent, int i) {
        if (parent[i] != i) {
            parent[i] = find(parent, parent[i]);
        }
        return parent[i];
    }

    private static void union(int[] parent, int[] rank, int x, int y) {
        int xroot = find(parent, x);
        int yroot = find(parent, y);

        if (rank[xroot] < rank[yroot]) {
            parent[xroot] = yroot;
        } else if (rank[xroot] > rank[yroot]) {
            parent[yroot] = xroot;
        } else {
            parent[yroot] = xroot;
            rank[xroot]++;
        }
    }
}
