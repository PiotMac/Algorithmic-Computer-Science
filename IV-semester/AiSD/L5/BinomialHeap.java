import java.util.*;

public class BinomialHeap {

    public  int operationComparisons = 0;

    private class Node {
        int key;
        int degree;
        Node parent;
        Node child;
        Node sibling;

        Node(int key) {
            this.key = key;
            this.degree = 0;
            this.parent = null;
            this.child = null;
            this.sibling = null;
        }
    }

    private Node head;

    public BinomialHeap() {
        head = null;
    }

    public boolean isEmpty() {
        return head == null;
    }

    public void heapInsert(int key) {
        BinomialHeap tempHeap = new BinomialHeap();
        tempHeap.head = new Node(key);
        head = mergeHeaps(head, tempHeap.head);
    }

    public void heapUnion(BinomialHeap heap1, BinomialHeap heap2) {
        head = mergeHeaps(heap1.head, heap2.head);
    }

    public int extractMin() {
        if (isEmpty())
            throw new NoSuchElementException("Heap is empty");

        Node minNode = findMinimumNode(head);
        head = removeMinNode(head, minNode);
        return minNode.key;
    }

    private Node mergeHeaps(Node heap1, Node heap2) {
        if (heap1 == null)
            return heap2;
        else if (heap2 == null)
            return heap1;
        else {
            Node mergedList;
            operationComparisons++;
            if (heap1.key <= heap2.key) {
                mergedList = heap1;
                heap1 = heap1.sibling;
            } else {
                mergedList = heap2;
                heap2 = heap2.sibling;
            }
            Node temp = mergedList;

            while (heap1 != null && heap2 != null) {
                operationComparisons++;
                if (heap1.degree <= heap2.degree) {
                    temp.sibling = heap1;
                    heap1 = heap1.sibling;
                } else {
                    temp.sibling = heap2;
                    heap2 = heap2.sibling;
                }
                temp = temp.sibling;
            }

            if (heap1 != null)
                temp.sibling = heap1;
            else
                temp.sibling = heap2;

            return mergedList;
        }
    }

    private Node findMinimumNode(Node heap) {
        Node minNode = heap;
        Node current = heap;

        while (current != null) {
            operationComparisons++;
            if (current.key < minNode.key)
                minNode = current;
            current = current.sibling;
        }

        return minNode;
    }

    private Node removeMinNode(Node heap, Node minNode) {
        if (heap == minNode)
            heap = minNode.sibling;
        else {
            Node current = heap;
            while (current.sibling != minNode)
                current = current.sibling;
            current.sibling = minNode.sibling;
        }

        BinomialHeap newHeap = new BinomialHeap();
        newHeap.head = reverseList(minNode.child);
        heap = mergeHeaps(heap, newHeap.head);
        return heap;
    }

    private Node reverseList(Node node) {
        if (node == null || node.sibling == null)
            return node;

        Node prev = null;
        Node current = node;
        Node next = current.sibling;

        while (current != null) {
            current.sibling = prev;
            current.parent = null;
            prev = current;
            current = next;
            if (next != null)
                next = next.sibling;
        }

        return prev;
    }
}
