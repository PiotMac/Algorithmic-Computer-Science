import java.security.SecureRandom;

public class CircularDoublyLinkedListMain {
    static SecureRandom secureRandom = new SecureRandom();
    public static void main(String[] args) {
        testMerge();
        System.out.println();
        checkCosts();
    }
    public static void insert(CircularDoublyLinkedList circularDoublyLinkedList, int value) {
        circularDoublyLinkedList.add(value);
    }

    public static void merge(CircularDoublyLinkedList list1, CircularDoublyLinkedList list2) {
        list1.tail.next = list2.head;
        list1.head.prev = list2.tail;
        list2.head.prev = list1.tail;
        list2.tail.next = list1.head;
        list1.tail = list2.tail;
        list2.head = list1.head;

        list1.size += list2.size;
        list2.size = list1.size;
    }

    public static void testMerge() {
        CircularDoublyLinkedList list1 = new CircularDoublyLinkedList();
        CircularDoublyLinkedList list2 = new CircularDoublyLinkedList();
        for (int i = 0; i < 10; i++) {
            int firstListValue = secureRandom.nextInt(10, 100);
            int secondListValue = secureRandom.nextInt(10, 100);
            insert(list1, firstListValue);
            insert(list2, secondListValue);
        }
        System.out.println("########## FIRST LIST ##########");
        DoublyLinkedItem firstIterator = list1.head;
        System.out.print(firstIterator.value + " ");
        for (int i = 0; i < 9; i++) {
            firstIterator = firstIterator.next;
            System.out.print(firstIterator.value + " ");
        }
        System.out.println();
        System.out.println("########## SECOND LIST ##########");
        DoublyLinkedItem secondIterator = list2.head;
        System.out.print(secondIterator.value + " ");
        for (int i = 0; i < 9; i++) {
            secondIterator = secondIterator.next;
            System.out.print(secondIterator.value + " ");
        }
        System.out.println();
        merge(list1, list2);
        DoublyLinkedItem mergedIterator = list1.head;
        System.out.println("########## MERGED LIST ##########");
        System.out.print(mergedIterator.value + " ");
        for (int i = 0; i < 19; i++) {
            mergedIterator = mergedIterator.next;
            System.out.print(mergedIterator.value + " ");
        }
    }

    public static void checkCosts() {
        int[] array = new int[10000];
        CircularDoublyLinkedList list = new CircularDoublyLinkedList();
        for (int i = 0; i < 10000; i++) {
            array[i] = secureRandom.nextInt(0, 100001);
            insert(list, array[i]);
        }
        double averageCost = 0.0;
        for (int i = 0; i < 1000; i++) {
            int randomIndex = secureRandom.nextInt(0, 10000);
            int randomValue = array[randomIndex];
            int comparisons = 0;
            DoublyLinkedItem listIterator = list.head;
            if (secureRandom.nextBoolean()) {
                while (listIterator.value != randomValue) {
                    comparisons++;
                    listIterator = listIterator.next;
                }
            }
            else {
                while (listIterator.value != randomValue) {
                    comparisons++;
                    listIterator = listIterator.prev;
                }
            }
            averageCost += comparisons;
        }
        averageCost /= 1000.0;
        System.out.println("FIRST CASE = " + averageCost);
        averageCost = 0.0;
        for (int i = 0; i < 1000; i++) {
            int randomValue = secureRandom.nextInt(0, 100001);
            int comparisons = 0;
            DoublyLinkedItem listIterator = list.head;
            if (secureRandom.nextBoolean()) {
                while (listIterator.value != randomValue && comparisons != list.size) {
                    comparisons++;
                    listIterator = listIterator.next;
                }
            }
            else {
                while (listIterator.value != randomValue && comparisons != list.size) {
                    comparisons++;
                    listIterator = listIterator.prev;
                }
            }
            averageCost += comparisons;
        }
        averageCost /= 1000.0;
        System.out.println("SECOND CASE = " + averageCost);
    }
}
