class HuffmanNode implements Comparable<HuffmanNode> {
    byte data;
    int frequency;
    HuffmanNode left, right;

    public int compareTo(HuffmanNode node) {
        return this.frequency - node.frequency;
    }
}