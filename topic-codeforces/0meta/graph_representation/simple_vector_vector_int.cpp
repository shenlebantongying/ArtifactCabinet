#include <bits/stdc++.h>

using namespace std;

// plan of attack: represent a graph and list all connections in a node

vector<vector<int>> graph{};
vector<int> vortex{};

void init_graph(int N){
    graph.assign(N,vector<int>());
}

void add_edge(int from, int to){
    graph[from].push_back(to);
    if (find(vortex.begin(),vortex.end(),from)!=vortex.end()) {
        vortex.push_back(from);
    }
};

void print_edges(){
    for (int i = 0; i < graph.size(); ++i) {
        for (auto x: graph[i]) {
            cout<< i << " -> " << x <<endl;;
        }
    }
};

int main(){
    init_graph(100);
    add_edge(1,2);
    add_edge(1,3);
    add_edge(2,3);
    add_edge(3,6);
    add_edge(4,5);
    add_edge(1,4);
    print_edges();
};