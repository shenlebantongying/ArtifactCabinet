// dijkstra method, 4 direction path finding

#include <fmt/format.h>
#include <functional>
#include <queue>

const int X = 12;
const int Y = 12;

const int infinity = 9999;

struct node {
    bool obstacle = false;
    bool visited = false;
    int dist = infinity;
};

struct cord {
    int x;
    int y;
};

struct Grid {
    node in[Y][X] = {};

    cord start{};

    Grid() = default;

    using Cond = std::function<std::string(node &)>;

    void pprint(const Cond &cond) {
        for (auto &row: in) {
            for (auto &o: row) {
                fmt::print("{:3} ", cond(o));
            }
            fmt::println("");
        }
    }

    node *ref(int x, int y) { return &in[y][x]; }

    node *ref(cord c) { return &in[c.y][c.x]; }

    bool visit(cord c, int new_dist) {
        node *n = ref(c.x, c.y);
        if (!n->visited and new_dist < n->dist and !n->obstacle) {
            n->dist = new_dist;
            n->visited = true;
            return true;
        } else {
            return false;
        }
    }

    static std::vector<cord> get_neighbors(cord p) {
        std::vector<cord> neighbors{};
        if (p.y - 1 >= 0) {
            neighbors.push_back({p.x, p.y - 1});
        }
        if (p.x + 1 < X) {
            neighbors.push_back({p.x + 1, p.y});
        }
        if (p.y + 1 < Y) {
            neighbors.push_back({p.x, p.y + 1});
        }
        if (p.x - 1 >= 0) {
            neighbors.push_back({p.x - 1, p.y});
        }
        return neighbors;
    }

    void walk() {
        auto *fronts = new std::queue<cord>();
        fronts->push(start);


        while (!fronts->empty()) {
            cord p = fronts->front();

            if (!fronts->empty()) {
                fronts->pop();
            }

            const int new_dist = ref(p)->dist + 1;

            std::vector<cord> neighbors = get_neighbors(p);

            for (auto &c: neighbors) {
                if (visit(c, new_dist)) {
                    fronts->push(c);
                }
            }
        }
    }

    std::vector<cord> get_routine_to(cord dest) {
        std::vector<cord> res;
        res.push_back(dest);

        int dist = ref(dest)->dist;

        cord p = dest;

        while (dist != 0) {
            std::vector<cord> neighbors = get_neighbors(p);
            p = *std::min_element(neighbors.begin(), neighbors.end(),
                                  [this](cord &a, cord &b) {
                                      return ref(a)->dist < ref(b)->dist;
                                  });

            dist = ref(p)->dist;
            res.push_back(p);
        }
        std::reverse(res.begin(), res.end());
        return res;
    }

    void make_obst(cord c) { ref(c)->obstacle = true; }

    void set_start_point(cord c) {
        start = c;
        ref(c)->dist = 0;
        ref(c)->visited = true;
    }
};

Grid::Cond dist = [](const node &o) -> std::string {
    if (o.dist == infinity) {
        return "⬛";
    } else {
        return std::to_string(o.dist);
    }
};


int main() {
    auto *g = new Grid();

    g->set_start_point({1, 1});

    for (int i = 0; i < 5; ++i) {
        g->make_obst({3, 2 + i});
    }

    g->make_obst({5, 5});

    g->walk();
    g->pprint(dist);

    for (auto &c: g->get_routine_to({9, 5})) {
        fmt::println("{} {}", c.x, c.y);
    }

    delete g;

    return 0;
}
