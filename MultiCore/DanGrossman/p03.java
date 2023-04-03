import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.RecursiveTask;

class SumArray extends RecursiveTask<Integer>{
    static int SEQUENTIAL_THRESHOLD = 500;
    // A "cutoff" -> use none-parallel when the case is too small

    int lo;
    int hi;
    int[] arr;

    SumArray(int[]a, int l, int h) {lo=l;hi=h;arr=a;}

    @Override
    public Integer compute() {
        if(hi-lo <= SEQUENTIAL_THRESHOLD){
            int ans=0;
            for (int i = lo; i < hi; i++) {
                ans+=arr[i];
            }
            return ans;
        }  else {
            SumArray left  = new SumArray(arr,lo,(hi+lo)/2);
            SumArray right = new SumArray(arr,(hi+lo)/2,hi);
            left.fork(); // .start() of p2
            int rightAns = right.compute(); // .run() of p2

            // At this point of execution:
            // Left is running in a thread.
            // Right is running too by itself.

            // If Left finished before Right,
            // Well, it is finished and in the same time, Right is computing,
            // The main thread will proceed once the right also finished
            // The next statement will cause no waiting

            // If right finished before Left,
            // The next statement will make sure the main thread wait Left to finish

            int leftAns = left.join();
            //                 ^ will return the result form sub-computes

            // NOTE: There is only one call to join because only
            // one helper thread was created. The order here is still essential
            // so that the two halves of the work are done in parallel

            return leftAns + rightAns;

        }
    }
}

public class p03 {
    static int sumArray(int[] array){
        // TODO: read doc of ForkJoinPool
        return ForkJoinPool
                .commonPool()
                .invoke(new SumArray(array,0,array.length));
    }

    public static void main(String[] args) {
        int[] ar=new int[10000];
        for (int i = 1; i <= 10000; i++) {
            ar[i-1]=i;
        }
        System.out.println(sumArray(ar)); //=> 50005000
    }
}
