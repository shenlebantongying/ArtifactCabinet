import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/* All in OOP! */

public class CC_oop {
  public static void main(String[] args) throws IOException {
    Graph g = new Graph();

    List<int[]> fields = get_fields(Path.of("./data/rosalind_cc.txt"));
    int n = fields.getFirst()[0];
    for (int i = 1; i <= n; i++) {
      g.addNode(i);
    }
    fields.stream().skip(1).forEach(nums -> g.connect(nums[0], nums[1]));

    int counter = 0;
    for (var cp = g.getNextComponent(); !cp.isEmpty(); cp = g.getNextComponent()) {
      System.out.println(cp);
      counter++;
    }

    System.out.println(counter);
  }

  static List<int[]> get_fields(Path path) throws IOException {
    return Files.readAllLines(path).stream()
        .map(
            l -> {
              String[] f = l.split(" ");
              return new int[] {Integer.parseInt(f[0]), Integer.parseInt(f[1])};
            })
        .toList();
  }
}

class Graph {
  ArrayList<Node> nodes = new ArrayList<>();

  public void addNode(Integer n) {
    nodes.add(new Node(n));
  }

  public Node getNode(Integer n) {
    return nodes.get(n - 1);
  }

  public void connect(Integer a, Integer b) {
    getNode(a).neighbors.add(getNode(b));
    getNode(b).neighbors.add(getNode(a));
  }

  public Node getNextUnvisited() {
    for (Node n : nodes) {
      if (!n.visited) {
        return n;
      }
    }
    return null;
  }

  public ArrayList<Node> getNextComponent() {
    Node start = getNextUnvisited();
    if (start == null) {
      return new ArrayList<>();
    }
    ArrayList<Node> components = new ArrayList<>();
    components.add(start);
    start.visited = true;

    ArrayList<Node> current = start.neighbors;
    while (!current.isEmpty()) {
      ArrayList<Node> new_current = new ArrayList<>();
      current.forEach(
          c -> {
            if (!c.visited) {
              c.visited = true;
              components.add(c);
              new_current.addAll(c.neighbors);
            }
          });
      current = new_current;
    }
    return components;
  }

  @Override
  public String toString() {
    StringBuilder a = new StringBuilder();
    nodes.forEach(v -> a.append(v.toString()));
    return a.toString();
  }
}

class Node {

  public Node(Integer v) {
    this.v = v;
  }

  public Integer v;
  public Boolean visited = false;
  public ArrayList<Node> neighbors = new ArrayList<>();

  @Override
  public String toString() {
    return "("
        + v.toString()
        + " "
        + (visited ? "yes" : "not")
        + (!neighbors.isEmpty()
            ? " -> " + neighbors.stream().map(v -> v.v.toString()).collect(Collectors.joining(" "))
            : "")
        + ")";
  }
}
