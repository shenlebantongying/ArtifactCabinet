// https://www.infoq.com/articles/data-oriented-programming-java/

public class dataOriented {

    sealed interface Option {
        record op1(int n) implements Option { }
        record op2(String s) implements Option { }
    }

    public Option judge() {

        if (Math.round(Math.random()) == 0) {
            return new Option.op1(2);
        } else {
            return new Option.op2("what");
        }
    }

    public static void main(String[] args) {
        var nice = new dataOriented();

        for (int i = 0; i < 10; i++) {
            switch (nice.judge()) {
                case Option.op1(int n) -> System.out.println("op1 -> " + n);
                case Option.op2(String s) -> System.out.println("op2 -> " + s);
                default -> throw new IllegalStateException("Unexpected value: " + nice.judge());
            }
        }
    }
}