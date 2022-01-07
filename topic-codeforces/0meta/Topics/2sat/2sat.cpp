#include <bits/stdc++.h>

using namespace std;

// https://cp-algorithms.com/graph/2SAT.html
//
// TODO: the original coding style is shit
// https://codeforces.com/blog/entry/92977

// TODO: let the code make more sense.

struct two_sat {
    int n; // variables numbered as N

    /*
     * Graph described deduction relationship
     * To symbol numbered as N, 2*N is the complement of it.
     * ¬a => b can be described in program as
     * edge graph[2*a] => graph[b]
     *
     * It is not a very human friendly solution.
     * */

    vector<vector<int>> graph, graph_reversed;
    vector<int> components,
            answer,
            strong_connects,
            order;
    vector<bool> visited;

    explicit two_sat(int n) : n(n) {
        int two_n = 2 * n;
        graph.assign(two_n, vector<int>());
        graph_reversed.assign(two_n, vector<int>());
        components.resize(two_n);
        visited.resize(two_n);
        answer.resize(two_n);
    }

    void add_edge(int from, int end) {
        graph[from].push_back(end);
        graph_reversed[end].push_back(from);
    }

    int neg(int i) {
        return i + n;
    }

    //A∨B is equivalent to ¬A ⇒ B ∧ ¬B ⇒ A (if one of the two variables is false, then the other one must be true).
    void add_or(int a, bool aff_a_q, int b, bool aff_b_q) {
        add_edge(aff_a_q ? neg(a) : a,  // ¬A // if a is affirmation, we choose n+a (aka negation of a)
                 aff_b_q ? b : neg(b)); //  ⇒ B
        add_edge(aff_b_q ? neg(b) : b, // ¬B
                 aff_a_q ? a : neg(a)); //  ⇒ A
    }

    // xor -> only one true
    // A⊕B is equivalent of

    // (A ∨ B) ∧ (¬ A ∨ ¬ B)
    //  (A ∨ B)     => there must be at least one true
    //  (¬ A ∨ ¬ B) => there must be at least one false
    //  ⊕ is binary operator => set {A,B} = {True,False}

    // there are other equivalents https://en.wikipedia.org/wiki/Exclusive_or#Equivalencies.2C_elimination.2C_and_introduction
    void add_xor(int a, bool aff_a_q, int b, bool aff_b_q){
        add_or(a,aff_a_q,b,aff_b_q);
        add_or(a,not aff_b_q,b,not aff_b_q);
    }

    void add_and(int a, bool aff_a_q, int b, bool aff_b_q){
        add_xor(a,neg(aff_a_q),b,aff_b_q);
    }

    // todo: not sure about this order
    void top_sort(int i){
        visited[i] = true;
        for (const auto &v : graph[i]){
            if (not visited[v]){
                top_sort(v);
            }
        }
        order.push_back(i);
    }

    void scc (int i, int id){
        visited[i] = true;
        components[i] = id;

        for (const auto &v: graph_reversed[i]){
            if(not visited[v]){
                scc(v, id);
            }

        }
    }

    bool satisfy_q() {
        fill(visited.begin(),visited.end(), false);

        for(int i=0; i < 2 * n; i++){
            if(not visited[i]){
                top_sort(i);
            }
        }

        fill(visited.begin(),visited.end(), false);

        reverse(order.begin(),order.end());

        int id=0;
        for(const auto &v : order){
            if(not visited[v]){
                scc(v,id++);
            }
        }

        for (int i = 0; i < n; ++i) {
            if(components[i] == components[i+n]){
                return false;
            }
            answer[i] = (components[i] > components[i+n]?1:0);
        }
        return true;
    }

    void check_sat(){
        if(this->satisfy_q()){
            cout<<"ok"<<endl;
        } else {
            cout <<"no"<<endl;
        }
    }
};

int main() {
    auto t = new two_sat(3);

    // (a∨¬b)∧(¬a∨b)∧(¬a∨¬b)∧(a∨¬c)
    t->add_or(0, true,1, false);
    t->add_or(0, false,1,true);
    t->add_or(0, false,1, false);
    t->add_or(0, true,2, false);

    t->check_sat();

    auto not_t = new two_sat(2);
    //(a∨b)∧(¬a∨¬b)∧(¬a∨b)∧(a∨¬b)
    not_t->add_or(0,true,1,true);
    not_t->add_or(0, false,1, false);
    not_t->add_or(0, false,1, true);
    not_t->add_or(0, true,1, false);
    not_t->check_sat();

}