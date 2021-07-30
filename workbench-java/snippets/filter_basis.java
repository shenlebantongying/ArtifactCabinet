import java.util.*;
import java.util.stream.Collectors;

public class filter_basis {
    public static void main(String[] args) {

            List<String> words =
                    Arrays.asList("asd", "Qwev","Qxce","iop","hjks");

            List<String> filterd
                    = words.stream().
                        filter(x->x.length()>3).
                        filter(x->Character.isUpperCase(x.charAt(0))).
                        collect(Collectors.toList());

            filterd.forEach(System.out::println);
    }
}