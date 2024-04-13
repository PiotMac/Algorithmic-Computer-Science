public class CircularDoublyLinkedList {
    public CircularDoublyLinkedList() {
        head = null;
        tail = null;
        size = 0;
    }
    DoublyLinkedItem head;
    DoublyLinkedItem tail;
    int size;

    public void add(int value) {
        DoublyLinkedItem newItem = new DoublyLinkedItem(value, head, tail);
        if (head == null) {
            head = tail = newItem;
            size++;
            return;
        }
        head.prev = newItem;
        tail.next = newItem;
        tail = newItem;
        size++;
    }
}
