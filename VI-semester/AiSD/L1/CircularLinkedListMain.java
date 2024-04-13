import java.security.SecureRandom;

public class CircularLinkedListMain {
    static SecureRandom secureRandom = new SecureRandom();
    public static void main(String[] args) {
        testMerge();
        System.out.println();
        checkCosts();
    }
    public static void insert(CircularLinkedList circularLinkedList, int value) {
        circularLinkedList.add(value);
    }

    public static void merge(CircularLinkedList list1, CircularLinkedList list2) {
        Item firstListTail = list1.head;
        for (int i = 0; i < list1.size - 1; i++) {
            firstListTail = firstListTail.next;
        }
        firstListTail.next = list2.head;

        Item secondListTail = list2.head;
        for (int i = 0; i < list2.size - 1; i++) {
            secondListTail = secondListTail.next;
        }
        secondListTail.next = list1.head;
        int firstSize = list1.size;
        list1.size += list2.size;
        list2.size += firstSize;
    }

    public static void testMerge() {
        CircularLinkedList list1 = new CircularLinkedList();
        CircularLinkedList list2 = new CircularLinkedList();
        for (int i = 0; i < 10; i++) {
            int firstListValue = secureRandom.nextInt(10, 100);
            int secondListValue = secureRandom.nextInt(10, 100);
            insert(list1, firstListValue);
            insert(list2, secondListValue);
        }
        System.out.println("########## FIRST LIST ##########");
        Item firstIterator = list1.head;
        System.out.print(firstIterator.value + " ");
        for (int i = 0; i < 9; i++) {
            firstIterator = firstIterator.next;
            System.out.print(firstIterator.value + " ");
        }
        System.out.println();
        System.out.println("########## SECOND LIST ##########");
        Item secondIterator = list2.head;
        System.out.print(secondIterator.value + " ");
        for (int i = 0; i < 9; i++) {
            secondIterator = secondIterator.next;
            System.out.print(secondIterator.value + " ");
        }
        System.out.println();
        merge(list1, list2);
        Item mergedIterator = list1.head;
        System.out.println("########## MERGED LIST ##########");
        System.out.print(mergedIterator.value + " ");
        for (int i = 0; i < 19; i++) {
            mergedIterator = mergedIterator.next;
            System.out.print(mergedIterator.value + " ");
        }
        System.out.println();
    }

    public static void checkCosts() {
        int[] array = new int[10000];
        CircularLinkedList list = new CircularLinkedList();
        for (int i = 0; i < 10000; i++) {
            array[i] = secureRandom.nextInt(0, 100001);
            insert(list, array[i]);
        }
        double averageCost = 0.0;
        for (int i = 0; i < 1000; i++) {
            int randomIndex = secureRandom.nextInt(0, 10000);
            int randomValue = array[randomIndex];
            int comparisons = 0;
            Item listIterator = list.head;
            while (listIterator.value != randomValue) {
                comparisons++;
                listIterator = listIterator.next;
            }
            averageCost += comparisons;
        }
        averageCost /= 1000.0;
        System.out.println("FIRST CASE = " + averageCost);
        averageCost = 0.0;
        for (int i = 0; i < 1000; i++) {
            int randomValue = secureRandom.nextInt(0, 100001);
            int comparisons = 0;
            Item listIterator = list.head;
            while (listIterator.value != randomValue && comparisons != list.size) {
                comparisons++;
                listIterator = listIterator.next;
            }
            averageCost += comparisons;
        }
        averageCost /= 1000.0;
        System.out.println("SECOND CASE = " + averageCost);
    }
}
