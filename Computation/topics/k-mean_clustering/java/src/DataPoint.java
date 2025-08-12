import java.util.Random;

public record DataPoint(Double x, Double y) {
  public Double distance(DataPoint other) {
    return Math.sqrt(Math.pow(x - other.x, 2) + Math.pow(y - other.y, 2));
  }

  public static DataPoint randomPoint(Random rad, Double range) {
    return new DataPoint(rad.nextDouble() * range, rad.nextDouble() * range);
  }
}
