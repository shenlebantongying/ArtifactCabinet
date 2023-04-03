class myClass {
    private final int innerValue;

    myClass(int passedValue){
        this.innerValue=passedValue;
    }

    void printInnerValue(){
        System.out.println(this.innerValue);
    }
}

public class pass_by_what_q {
    public static void main(String[] args) {

        int outValue=1;
        myClass myObj = new myClass(outValue);

        myObj.printInnerValue();

        outValue=2;
        myObj.printInnerValue();

        // It is pass-by-value.
        // The innerValue is encapsulated, as change of outValue won't affect it.

        // There is no de-reference operator in Java.

        // However, the type of passedValue can be a generic type,
        // and if you pass a subtype of that type,
        // then, the type of this.val will change accordingly.

        // This is a method to change the type of class at runtime.
    }
}
