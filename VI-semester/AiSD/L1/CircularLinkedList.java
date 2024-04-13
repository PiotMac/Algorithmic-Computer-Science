public class CircularLinkedList {
    public CircularLinkedList() {
        head = null;
        size = 0;
    }
    Item head;
    int size;
    public void add(int value) {
        Item newItem = new Item(value, head);
        if (head == null) {
            head = newItem;
            head.next = head;
            size++;
            return;
        }
        Item iterator = head;
        for (int i = 0; i < size - 1; i++) {
            iterator = iterator.next;
        }
        iterator.next = newItem;
        size++;
    }
}
