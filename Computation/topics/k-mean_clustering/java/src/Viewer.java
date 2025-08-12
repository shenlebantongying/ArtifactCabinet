import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.List;

public class Viewer extends JFrame {

  GraphicsPane graphics;
  JButton before;
  JButton next;
  Color[] cluster_color = new Color[] {Color.yellow, Color.green, Color.cyan, Color.pink};

  List<List<Cluster>> clusterSnapshots = new ArrayList<>();
  int snapshotIndex = 0;

  Viewer(int w) {
    super();
    setSize(w, w);

    setLayout(new BorderLayout());

    var box = Box.createVerticalBox();
    var boxBtns = Box.createHorizontalBox();

    before = new JButton("Before");
    next = new JButton("Next");
    boxBtns.add(before);
    boxBtns.add(next);
    before.addActionListener(
        _ -> {
          if (snapshotIndex > 0) {
            System.out.println(snapshotIndex);
            snapshotIndex -= 1;
            displayClusters(clusterSnapshots.get(snapshotIndex));
          }
        });

    next.addActionListener(
        _ -> {
          if (snapshotIndex < clusterSnapshots.size() - 1) {
            System.out.println(snapshotIndex);
            snapshotIndex += 1;
            displayClusters(clusterSnapshots.get(snapshotIndex));
          }
        });

    graphics = new GraphicsPane();
    box.add(boxBtns);
    box.add(graphics);

    setContentPane(box);
  }

  public void displayClusters(List<Cluster> clusters) {
    graphics.drawCommands.clear();
    for (int i = 0; i < clusters.size(); i++) {
      graphics.addPoint(clusters.get(i).centroid, Color.black);

      for (var p : clusters.get(i).points) {
        graphics.addPoint(p, cluster_color[i]);
      }
    }
    graphics.updateUI();
  }

  public void snapshotCluster(List<Cluster> clusters) {
    List<Cluster> clusters_copy = new ArrayList<>();
    for (var c : clusters) {
      clusters_copy.add(new Cluster(c.points, c.centroid));
    }
    clusterSnapshots.add(clusters_copy);
  }

  public void showWindow() {
    snapshotIndex = clusterSnapshots.size() - 1;
    displayClusters(clusterSnapshots.get(snapshotIndex));
    setVisible(true);
  }
}

class GraphicsPane extends JPanel {

  record PointDraw(DataPoint p, Color c) {}

  GraphicsPane() {
    this.setBackground(Color.white);
  }

  List<PointDraw> drawCommands = new ArrayList<>();

  public void addPoint(DataPoint p, Color c) {
    drawCommands.add(new PointDraw(p, c));
  }

  @Override
  public void paintComponent(Graphics g) {
    super.paintComponent(g);
    for (var d : drawCommands) {
      g.setColor(d.c);
      g.fillOval(d.p.x().intValue(), d.p.y().intValue(), 5, 5);
    }
  }
}
