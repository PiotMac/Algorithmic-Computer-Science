public class Philosopher implements Runnable {
    private int id;
    private int mealsToEat;
    private int mealsEaten = 0;
    private final Fork leftFork;
    private final Fork rightFork;

    public Philosopher(int id, int mealsToEat, Fork leftFork, Fork rightFork) {
        this.mealsToEat = mealsToEat;
        this.id = id;
        this.leftFork = leftFork;
        this.rightFork = rightFork;
    }

    private void think() throws InterruptedException {
        System.out.println("Philosopher " + id + " is thinking.");
        Thread.sleep(((int) (Math.random() * 1000)));
    }

    private void eat() throws InterruptedException {
        System.out.println("Philosopher " + id + " is eating.");
        mealsEaten++;
        Thread.sleep(((int) (Math.random() * 1000)));
    }

    @Override
    public void run() {
        try {
            while (mealsEaten < mealsToEat) {
                think();

                System.out.println("Philosopher " + id + " is trying to pick up left fork.");
                leftFork.pickUp();
                System.out.println("Philosopher " + id + " has picked up left fork.");
                System.out.println("Philosopher " + id + " is trying to pick up right fork.");
                rightFork.pickUp();
                System.out.println("Philosopher " + id + " has picked up right fork.");

                eat();

                leftFork.putDown();
                rightFork.putDown();
                System.out.println("Philosopher " + id + " has finished eating - MEALS LEFT: [" + (mealsToEat - mealsEaten) + "]");
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }
}
