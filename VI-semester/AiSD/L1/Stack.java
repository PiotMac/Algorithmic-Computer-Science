public class Stack {
    public Stack() {
        top = null;
    }
    Item top;

    public void push(int value) {
        Item newItem = new Item(value, top);
        System.out.print(newItem.value + " ");
        top = newItem;
    }

    public int pop() {
        if (top == null) {
            return -1;
        }
        int removedValue = top.value;
        System.out.print(removedValue + " ");
        top = top.next;
        return removedValue;
    }
}
