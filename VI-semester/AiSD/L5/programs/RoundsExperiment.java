import java.io.FileWriter;
import java.io.IOException;
import java.util.*;

public class RoundsExperiment {
    static final int N_MIN = 100;
    static final int N_MAX = 9000;
    static final int STEP = 100;
    static final int REPS = 100;
    static Random rand = new Random();
    public static void main(String[] args) throws IOException {
        MSTAlgorithms mst = new MSTAlgorithms();
        FileWriter fileWriter = new FileWriter("data/rounds.csv");
        fileWriter.write("n;max;min;avg\n");

        for (int n = N_MIN; n <= N_MAX; n += STEP) {
            int maxRounds = 0;
            int minRounds = Integer.MAX_VALUE;
            double avgRounds = 0.0;

            Graph graph = new Graph(n);
            ArrayList<Edge> mstEdges = new ArrayList<>();
            mst.primAlgorithm(graph, mstEdges);
            graph.edges = null;
            graph.adjacencyList = null;

            // Converting the MST edges to the adjacency list
            ArrayList<ArrayList<Integer>> adjacencyList = new ArrayList<>();

            for (int i = 0; i <= n; i++) {
                adjacencyList.add(new ArrayList<>());
            }

            for (Edge edge : mstEdges) {
                adjacencyList.get(edge.source).add(edge.destination);
                adjacencyList.get(edge.destination).add(edge.source);
            }

            for (int k = 0; k < REPS; k++) {
                // Choosing random vertex as root
                int root = rand.nextInt(1, n + 1);

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

                traverseTreeFromLeavesToRoot(root, tree, roundMap, order);

                int receivedRounds = 0;
                for (int round : roundMap.values()) {
                    if (round > receivedRounds) {
                        receivedRounds = round;
                    }
                }

                if (receivedRounds < minRounds) {
                    minRounds = receivedRounds;
                }
                if (receivedRounds > maxRounds) {
                    maxRounds = receivedRounds;
                }
                avgRounds += receivedRounds;
            }
            avgRounds = avgRounds / REPS;
            fileWriter.write(n + ";" + maxRounds + ";" + minRounds + ";" + avgRounds + "\n");
            System.out.println("RESULTS FOR N = " + n);
            System.out.println("RoundsMAX: " + maxRounds);
            System.out.println("RoundsMIN: " + minRounds);
            System.out.println("RoundsAVG: " + avgRounds);
        }

        fileWriter.close();
    }

    private static void traverseTreeFromLeavesToRoot(int root, Map<Integer, List<Integer>> tree, Map<Integer,
            Integer> roundMap, Map<Integer, List<PropagationPair>> order) {
        Map<Integer, Integer> parentMap = new HashMap<>();

        // Mapping children with their parents
        buildParentMap(root, -1, tree, parentMap);

        // Find all leaves
        List<Integer> leaves = new ArrayList<>();
        for (int node : tree.keySet()) {
            if (tree.get(node).isEmpty()) {
                leaves.add(node);
                // Leaves have "time" attribute equal to 0
                roundMap.put(node, 0);
                // They also have no children, so their order is equal to null
                order.put(node, null);
            }
        }

        // Perform bottom-up traversal
        Set<Integer> visited = new HashSet<>();
        Queue<Integer> queue = new LinkedList<>(leaves);
        while (!queue.isEmpty()) {
            int currentNode = queue.poll();
            if (!visited.contains(currentNode)) {
                // Get time from current node's children
                if (!tree.get(currentNode).isEmpty()) {
                    // Get current node's children
                    List<Integer> children = tree.get(currentNode);
                    List<PropagationPair> propagationPairs = new ArrayList<>();
                    boolean isErrorDetected = false;

                    // Map every child with its already existing time attribute
                    for (int child : children) {
                        // If a child doesn't have its time set, go back to the while loop
                        // and add current's node to the back
                        if (!visited.contains(child)) {
                            queue.offer(currentNode);
                            isErrorDetected = true;
                            break;
                        }
                        propagationPairs.add(new PropagationPair(child, roundMap.get(child)));
                    }

                    // Go back to the while loop
                    if (isErrorDetected) {
                        continue;
                    }

                    // Sort children in descending order according to their time attributes
                    propagationPairs.sort((p1, p2) -> Integer.compare(p2.time, p1.time));

                    // Current node's propagation time is equal to: max{1 + c_1, 2 + c_2, ..., k + c_k}
                    // k = #children
                    // c_k = k-th child's time attribute
                    int propagationTimeForThisNode = 0;
                    for (int i = 0; i < propagationPairs.size(); i++) {
                        if ((propagationPairs.get(i).time + (i + 1)) > propagationTimeForThisNode) {
                            propagationTimeForThisNode = propagationPairs.get(i).time + (i + 1);
                        }
                    }
                    // Map the propagation time with current node
                    roundMap.put(currentNode, propagationTimeForThisNode);
                    // Map this node with the order in which it must inform its children
                    order.put(currentNode, propagationPairs);
                }
                // Add the current node to visited and add to the queue its parent if it was not visited
                visited.add(currentNode);
                int parent = parentMap.get(currentNode);
                if (parent != -1 && !visited.contains(parent)) {
                    queue.offer(parent);
                }
            }
        }
    }

    private static void buildParentMap(int currentNode, int parent, Map<Integer, List<Integer>> tree,
                                       Map<Integer, Integer> parentMap) {
        parentMap.put(currentNode, parent);
        for (int child : tree.get(currentNode)) {
            buildParentMap(child, currentNode, tree, parentMap);
        }
    }
}
