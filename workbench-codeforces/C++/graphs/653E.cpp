#include <bits/stdc++.h>
using namespace std;

// https://codeforces.com/contest/653/submission/123785724

set<int> remaining;
set<pair<int,int>> forbidden;

void NO(){
    cout<<"impossible"<<endl;
    exit(0);
}

bool not_forbidden(const int a,const int b){
    return forbidden.find(make_pair(min(a,b),max(a,b))) == forbidden.end();
}

void dfs(int a){

    vector<int> temp;

    for (int b: remaining) {
        if (not_forbidden(a,b)){
            temp.push_back(b);
        }
    }

    for (int x: temp) {
        remaining.erase(x);

    }

    for (int b: temp) {
        dfs(b);
    }
}


int main () {

    int n,m,k; cin>>n>>m>>k;
    //  the number of vertices
    //  the number of forbidden pairs
    //  the degree of vertex 1

    for (int i = 2; i <= n; ++i) {
        remaining.insert(i);
    }

    int max_1_degree = n-1;

    for (int i = 0; i < m; ++i) {
        int a,b;
        cin>>a>>b;
        if (a>b) swap(a,b);
        if (a == 1){
            max_1_degree -= 1;
        }
        forbidden.insert(make_pair(a,b));
    }

    if (max_1_degree < k) NO(); // There are no enough vertex to connect with 1

    int k_actual = 0; // subtrees connected with 1

    // try to construct every possible sub trees
    for (int i = 2; i <=n ; ++i) {
        if (not_forbidden(1,i) && remaining.find(i)!=remaining.end()){
            dfs(i);
            ++k_actual;
        }

    }

    if(!remaining.empty()) NO(); //=> still has some vertex that will have no place to put
    if(k_actual > k) NO(); // => we constructed more sub tress than k.

    cout<<"possible"<<endl;
}