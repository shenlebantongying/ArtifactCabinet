/*

The inefficient implementation of Chapter 1's SlowConvecHull.

Computational Geometry: Algorithms and Applications (2008)

*/

#include <CGAL/Cartesian.h>
#include <CGAL/Line_2.h>
#include <CGAL/Point_2.h>
#include <raylib.h>

using K = CGAL::Cartesian<float>;
using PointPair = std::array<CGAL::Point_2<K>, 2>;

std::vector<PointPair> compute_convex(std::vector<CGAL::Point_2<K>> points) {
  std::vector<PointPair> pointPairs{};
  std::vector<CGAL::Line_2<K>> pointPairs_as_lines{};
  for (auto &p1 : points) {
    for (auto &p2 : points) {
      if (p1 != p2) {
        pointPairs.emplace_back(std::array{p1, p2});
        pointPairs_as_lines.emplace_back(p1, p2);
      }
    }
  }

  std::vector<PointPair> result{};

  for (size_t i = 0; i < pointPairs_as_lines.size(); ++i) {
    bool valid = true;
    for (auto &point : points) {
      if (pointPairs_as_lines[i].oriented_side(point) == CGAL::ON_POSITIVE_SIDE) {
        valid = false;
      }
    }
    if (valid) {
      result.push_back(pointPairs[i]);
    }
  }
  return result;
}

std::vector<CGAL::Point_2<K>> gen_random_points(int n) {

  std::vector<CGAL::Point_2<K>> points{};
  points.reserve(50);
  for (size_t i = 0; i < points.capacity(); ++i) {
    points.emplace_back(GetRandomValue(100, 400), GetRandomValue(100, 400));
  }

  return points;
}

int main(int, char **) {

  constexpr auto w = 500;
  InitWindow(w, w, "hi");
  SetTargetFPS(1);

  while (!WindowShouldClose()) {
    BeginDrawing();

    auto pts = gen_random_points(50);
    auto result = compute_convex(pts);

    for (const PointPair &l : result) {
      DrawLineEx(Vector2{l[0].x(), l[0].y()}, Vector2{l[1].x(), l[1].y()}, 2,
                 RED);
    }

    for (const auto &p : pts) {
      DrawCircle(p.x(), p.y(), 3, GREEN);
    }

    ClearBackground(RAYWHITE);
    EndDrawing();
  }

  return 0;
}
