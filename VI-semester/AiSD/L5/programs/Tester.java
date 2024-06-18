import java.util.*;

public class Tester {
    final static int SIZE = 15;
    static Random rand = new Random();
    public static void main(String[] args) {
        // EX1
        MSTAlgorithms mstAlgorithms = new MSTAlgorithms();
        ArrayList<Edge> mstEdgesPrim = new ArrayList<>();
        ArrayList<Edge> mstEdgesKruskal = new ArrayList<>();

        Graph graph = new Graph(SIZE);
        double weightPrim = mstAlgorithms.primAlgorithm(graph, mstEdgesPrim);
        double weightKruskal = mstAlgorithms.kruskalAlgorithm(graph, mstEdgesKruskal);

        System.out.println("##################### PRIM ALGORITHM #####################");
        for (Edge edge : mstEdgesPrim) {
            edge.print();
        }
        System.out.println("Total weight [PRIM]    = " + weightPrim);
        System.out.println();
        System.out.println("################### KRUSKAL ALGORITHM ###################");
        for (Edge edge : mstEdgesKruskal) {
            edge.print();
        }
        System.out.println("Total weight [KRUSKAL] = " + weightKruskal);
        System.out.println();

        //EX2
        System.out.println("################## TEST OF 'ROUNDS' #####################");
        ArrayList<ArrayList<Integer>> adjacencyList = new ArrayList<>();

        for (int i = 0; i <= SIZE; i++) {
            adjacencyList.add(new ArrayList<>());
        }

        for (Edge edge : mstEdgesPrim) {
            adjacencyList.get(edge.source).add(edge.destination);
            adjacencyList.get(edge.destination).add(edge.source);
        }

        // Choosing random vertex as root
        int root = rand.nextInt(1, SIZE + 1);

        Map<Integer, List<Integer>> tree = new HashMap<>();
        Set<Integer> visited = new HashSet<>();

        Queue<Integer> queue = new LinkedList<>();
        queue.offer(root);
        visited.add(root);
        tree.put(root, new ArrayList<>());

        // Mapping all neighbours for every node using BFS
        while (!queue.isEmpty()) {
            int currentNode = queue.poll();
            for (int neighbor : adjacencyList.get(currentNode)) {
                if (!visited.contains(neighbor)) {
                    visited.add(neighbor);
                    queue.offer(neighbor);
                    tree.putIfAbsent(currentNode, new ArrayList<>());
                    tree.putIfAbsent(neighbor, new ArrayList<>());
                    tree.get(currentNode).add(neighbor);
                }
            }
        }

        // An object used to map every node with its round value ("v.time")
        Map<Integer, Integer> roundMap = new HashMap<>();
        // An object used to map every node with the order, in which it should alarm its children
        Map<Integer, List<PropagationPair>> order = new HashMap<>();

        RoundsExperiment.traverseTreeFromLeavesToRoot(root, tree, roundMap, order);

        int receivedRounds = 0;
        for (int round : roundMap.values()) {
            if (round > receivedRounds) {
                receivedRounds = round;
            }
        }

        System.out.println();
        for (Edge edge : mstEdgesPrim) {
            edge.print();
        }
        System.out.println();

        for (int i = 1; i <= SIZE; i++) {
            System.out.println("NODE [" + i + "]:");
            System.out.println("Propagation time = " + roundMap.get(i));
            List<PropagationPair> children = order.get(i);
            System.out.println("Order of propagation:");
            if (children == null) {
                System.out.println("    - null (This node is a leaf!)");
            }
            else {
                for (PropagationPair pair : children) {
                    System.out.println("    - NODE [" + pair.vertexID + "] with propagation time = " + pair.time);
                }
            }
            System.out.println();
        }
        System.out.println("PROPAGATION TIME FOR THE WHOLE GRAPH = " + receivedRounds);
    }
}
