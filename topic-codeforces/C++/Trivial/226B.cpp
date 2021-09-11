#include <bits/stdc++.h>

using namespace std;

int main(){
    int n,t;
    string s;
    cin >> n >> t;
    cin >> s;
    vector<int> mark{};
    for (int i = 0; i < t; ++i) {
        for (int j = 0; j < n-1; ++j) {
            if ((s[j] == 'B') && (s[j + 1] == 'G')) {
                mark.emplace_back(j);
            }
        }

        for(auto x : mark){
            swap(s[x],s[x+1]);
        }

        mark.clear();
    }
    cout<<s<<endl;
    exit(0);
}