public class Fork {
    private boolean isAvailable = true;

    public synchronized void pickUp() throws InterruptedException {
        while (!isAvailable) {
            wait();
        }
        isAvailable = false;
    }

    public synchronized void putDown() {
        isAvailable = true;
        notifyAll();
    }
}
