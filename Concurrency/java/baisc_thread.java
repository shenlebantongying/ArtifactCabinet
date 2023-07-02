public class baisc_thread extends java.lang.Thread{

    int acc;

    baisc_thread() {
        acc = 0;
    }

    public synchronized int getAcc() {
        acc += 1;
        return acc;
    }

    public void run(){
        for (int i = 0; i < 10; i++) {
            try {
                sleep(1000);
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
            System.out.println(this.threadId() + " " + getAcc());
        }
    }

    public static void main(String[] args) {
        (new baisc_thread()).start();
        (new baisc_thread()).start();
        (new baisc_thread()).start();

    }
}