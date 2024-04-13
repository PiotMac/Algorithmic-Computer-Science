public class Item {
    public Item(int value, Item next) {
        this.next = next;
        this.value = value;
    }
    Item next;
    int value;
}
