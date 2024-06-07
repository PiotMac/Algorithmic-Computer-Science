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
        System.out.println("Philosopher[" + id + "] --> THINKING . . .");
        Thread.sleep(((int) (Math.random() * 1000)));
    }

    private void eat() throws InterruptedException {
        System.out.println("Philosopher[" + id + "] --> EATING . . .");
        mealsEaten++;
        Thread.sleep(((int) (Math.random() * 1000)));
    }

    @Override
    public void run() {
        try {
            while (mealsEaten < mealsToEat) {
                think();

                System.out.println("Philosopher[" + id + "] --> TRYING TO PICK UP LEFT FORK . . .");
                leftFork.pickUp();
                System.out.println("Philosopher[" + id + "] --> PICKED UP LEFT FORK!");
                System.out.println("Philosopher[" + id + "] --> TRYING TO PICK UP RIGHT FORK . . .");
                rightFork.pickUp();
                System.out.println("Philosopher[" + id + "] --> PICKED UP RIGHT FORK!");

                eat();

                leftFork.putDown();
                rightFork.putDown();

                System.out.println("Philosopher[" + id + "] --> FINISHED EATING (MEALS YET TO EAT: " + (mealsToEat - mealsEaten) + ")");
            }
            System.out.println("Philosopher[" + id + "] --> LEAVES!");
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }
}
