import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.stream.Collectors;

public class HEA {

  public static void main(String[] args) throws IOException {

    var f = Files.readAllLines(Path.of("./data/rosalind_hea.txt"));
    ArrayList<Integer> ll =
        Arrays.stream(f.get(1).split(" "))
            .map(Integer::parseInt)
            .collect(Collectors.toCollection(ArrayList::new));

    Heap heap = new Heap(ll);
    heap.bubble_up();

    System.out.println(heap);
    System.out.println(heap.verify());
  }
}

record Heap(ArrayList<Integer> o) {
  public void bubble_up() {
    for (int i = 1; i < o.size(); i++) {
      int pos = i;
      int parent = Math.floorDivExact(i - 1, 2);
      while (o.get(pos) > o.get(parent)) {
        Collections.swap(o, pos, parent);
        pos = parent;
        if (pos == 0) {
          break;
        }
        parent = Math.floorDivExact(pos - 1, 2);
      }
    }
  }

  public boolean verify() {
    for (int i = 1; i < o.size(); i++) {
      int parent = Math.floorDivExact(i - 1, 2);
      if (o.get(parent) < o.get(i)) {
        System.out.println(o.get(parent));
        System.out.println(o.get(i));
        return false;
      }
    }
    return true;
  }

  @Override
  public String toString() {
    return o.stream().map(Object::toString).collect(Collectors.joining(" "));
  }
}
