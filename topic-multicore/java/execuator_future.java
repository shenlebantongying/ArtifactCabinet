import java.util.concurrent.Executor;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

public class execuator_future {

    public final ExecutorService execuator =
            Executors.newFixedThreadPool(8);

    int acc = 0;

    public Future<Integer> getAcc(){
        return execuator.submit(() -> {
            acc += 1;
            System.out.println(acc);
            return acc;
        });
    }

    public static void main(String[] args) {

        execuator_future ef = new execuator_future();

        for (int i = 0; i < 100; i++) {
            System.out.println(ef.getAcc());
        }
        int a =0 ;

    }
}
