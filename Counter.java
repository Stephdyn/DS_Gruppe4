class Counter {
    private int c = 0;

    //public void increment() {
    public synchronized void increment() {
        c = c + 1;
    }

    //public void decrement() {
    public synchronized void decrement() {
        c = c - 1;
    }

    //public int value() {
    public synchronized int value() {
        return c;
    }
}