public class Edge {
    int source;
    int destination;
    double weight;
    public Edge(int source, int destination, double weight) {
        this.source = source;
        this.destination = destination;
        this.weight = weight;
    }

    public void print() {
        System.out.println("[" + source + "] <----> [" + destination + "] : " + weight);
    }
}
