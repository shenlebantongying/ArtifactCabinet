class ExampleThread extends java.lang.Thread {
    int i;
    ExampleThread(int i) { this.i = i; }
    public void run() {
        System.out.println("Thread " + i + " says hi");
        System.out.println("Thread " + i + " says bye");
    }
}

public class p01 {
    public static void main(String[] args) {
        for (int i = 0; i < 20; i++) {
            ExampleThread t = new ExampleThread(i);
            t.start();
        }
    }
}
