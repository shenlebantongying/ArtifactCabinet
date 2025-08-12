import java.util.Set;

public class Cluster {

  Set<DataPoint> points;
  DataPoint centroid;

  Cluster(Set<DataPoint> points, DataPoint centroid) {
    this.points = points;
    this.centroid = centroid;
  }

  public void addPoint(DataPoint p) {
    points.add(p);
  }

  public DataPoint getCentroid() {
    return centroid;
  }

  public void resetPoints() {
    points.clear();
  }

  public void updateCentroid() {
    Double x_sum = 0.0;
    Double y_sum = 0.0;
    int n = points.size();
    for (var p : points) {
      x_sum += p.x();
      y_sum += p.y();
    }
    centroid = new DataPoint(x_sum / n, y_sum / n);
  }
}
