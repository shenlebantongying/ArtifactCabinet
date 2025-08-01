#include <bits/stdc++.h>
using namespace std;

// chain visiting, until a found the one already visited

int main ()
{
    int n; cin>>n;
    vector<int> v(n+1);

    for (int i = 1; i <= n; ++i) {
        cin>>v[i-1];
    }

    v.insert(v.begin(),0); // offset the v to ease vector indexing

    set<int> visited;

    for (int i = 1; i <= n; ++i) {
        visited.insert(i);
        for(int blame=v[i];;blame=v[blame]){
            // TODO: eliminate the blame==i
            if(visited.find(blame)!=visited.end() || blame==i){
                cout<<blame<<" ";
                break;
            } else {
                visited.insert(blame);
            }
        }
        visited.clear();
    }
    cout<<endl;
}
