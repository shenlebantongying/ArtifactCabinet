import java.util.*;

public class KMean {
  public static void main(String[] args) throws Exception {

    Double WIDTH = 800.0;
    int K = 4;
    Random rad = new Random();

    Viewer viewer = new Viewer(WIDTH.intValue());

    List<DataPoint> data = new ArrayList<>();

    for (int i = 0; i < 1000; i++) {
      data.add(DataPoint.randomPoint(rad, WIDTH));
    }
    List<Cluster> clusters = new ArrayList<>();
    for (int i = 0; i < K; i++) {
      clusters.add(new Cluster(new HashSet<>(), DataPoint.randomPoint(rad, WIDTH)));
    }

    for (int loops = 0; ; loops++) {

      for (var c : clusters) {
        c.resetPoints();
      }

      var oldCentroids = new ArrayList<DataPoint>();
      for (var c : clusters) {
        oldCentroids.add(c.centroid);
      }

      // For each point, find the closest centroid.
      for (var point : data) {
        double minDistance = Double.MAX_VALUE;
        int minIndex = -1;
        for (int j = 0; j < K; j++) {
          var distance = point.distance(clusters.get(j).getCentroid());
          if (distance < minDistance) {
            minIndex = j;
            minDistance = distance;
          }
        }
        clusters.get(minIndex).addPoint(point);
      }

      // update centroids
      for (var c : clusters) {
        c.updateCentroid();
      }

      var newCentroids = new ArrayList<DataPoint>();
      for (var c : clusters) {
        newCentroids.add(c.centroid);
      }
      System.out.println("Generation: " + loops);
      viewer.snapshotCluster(clusters);
      if (compareCentroids(newCentroids, oldCentroids)) {
        break;
      }
    }
    viewer.showWindow();
  }

  static boolean compareCentroids(List<DataPoint> a, List<DataPoint> b) throws Exception {
    if (a.size() != b.size()) {
      throw new Exception();
    }

    for (int i = 0; i < a.size(); i++) {
      if (a.get(i).distance(b.get(i)) > 1) {
        return false;
      }
    }
    return true;
  }
}
