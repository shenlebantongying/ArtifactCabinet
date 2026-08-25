class SumThread extends java.lang.Thread {
    int lo; //
    int hi;
    int[] arr;
    int ans=0;

    SumThread(int[]a,int l,int h){
        lo=l;hi=h;arr=a;
    }

    public void run(){
        for (int i = lo; i <hi ; i++) {
            ans+=arr[i];
        }
    }
}

public class p02 {
    static int sum(int[] arr) throws InterruptedException {
        int len=arr.length;
        int ans=0;

        SumThread[] ts=new SumThread[4];
        for (int i = 0; i < 4; i++) {
            ts[i] = new SumThread(arr,(i*len)/4,((i+1)*len)/4);
            ts[i].start();
        }
// Wait all helper threads to finish before get the final sum
//      for (int i = 0; i < 4; i++) {
//          ts[i].join();
//      }

// Put join & combining ans together,
        for (int i = 0; i < 4; i++) {
            ts[i].join(); // Do not join until the i_th thread is finished
                          // Note: they are already running on ts[o].start();
            ans+=ts[i].ans;
        }
        return ans;
    }

    public static void main(String[] args) throws InterruptedException {
        int[] ar=new int[50];
        for (int i = 0; i < 50; i++) {
            ar[i]=i;
        }
        System.out.println(sum(ar)); // => 1225
    }
}
