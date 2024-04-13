public class Queue {
    public Queue() {
        front = null;
        back = null;
    }
    Item front;
    Item back;

    public void addItem(int value) {
        Item newItem = new Item(value, null);
        System.out.print(newItem.value + " ");
        if (back == null) {
            front = back = newItem;
        }
        else {
            back.next = newItem;
            back = newItem;
        }
    }

    public int removeItem() {
        if (front == null) {
            return -1;
        }
        int removedValue = front.value;
        System.out.print(removedValue + " ");
        front = front.next;
        if (front == null) {
            back = null;
        }
        return removedValue;
    }
}
