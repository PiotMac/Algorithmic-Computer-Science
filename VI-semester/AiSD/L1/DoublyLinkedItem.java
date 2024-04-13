public class DoublyLinkedItem {
    public DoublyLinkedItem(int value, DoublyLinkedItem next, DoublyLinkedItem prev) {
        this.next = next;
        this.prev = prev;
        this.value = value;
    }
    DoublyLinkedItem next;
    DoublyLinkedItem prev;
    int value;
}
