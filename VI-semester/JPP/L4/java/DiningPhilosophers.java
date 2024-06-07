public class DiningPhilosophers {
    public static void main(String[] args) throws InterruptedException {
        int numPhilosophers = 5;
        int maxMeals = 3;

        Fork[] forks = new Fork[numPhilosophers];
        for (int i = 0; i < numPhilosophers; i++) {
            forks[i] = new Fork();
        }

        Thread[] philosophers = new Thread[numPhilosophers];
        for (int i = 0; i < numPhilosophers; i++) {
            philosophers[i] = new Thread(new Philosopher(i + 1, maxMeals, forks[i], forks[(i + 1) % numPhilosophers]));
            philosophers[i].start();
        }

        for (int i = 0; i < numPhilosophers; i++) {
            philosophers[i].join();
        }

        System.out.println("All philosophers have finished eating.");
    }
}