public class algebraDataType {
    sealed interface Expr {
        record Sum(Expr left, Expr right) implements Expr {
        }

        record Prod(Expr left, Expr right) implements Expr {
        }

        record Neg(Expr e) implements Expr {
        }

        record Num(int c) implements Expr {
        }
    }

    static int eval(Expr e) {
        return switch (e) {
            case Expr.Sum(var a, var b) -> eval(a) + eval(b);
            case Expr.Prod(var a, var b) -> eval(a) * eval(b);
            case Expr.Neg(var a) -> -eval(a);
            case Expr.Num(var i) -> i;
        };
    }

    public static void main(String[] args) {
        // (3+5) * -10
        System.out.println(eval(new Expr.Prod(new Expr.Sum(new Expr.Num(3), new Expr.Num(5)), new Expr.Neg(new Expr.Num(10)))));

    }
}
