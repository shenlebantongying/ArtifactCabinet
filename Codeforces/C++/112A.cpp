#include <bits/stdc++.h>
using namespace std;

// https://codeforces.com/problemset/problem/112/A
// TODO: Currying lambda functions

int main() {
    string a,b;
    cin>> a >> b;

    transform(a.begin(),a.end(),a.begin(),[](unsigned char c){return tolower(c);});
    transform(b.begin(),b.end(),b.begin(),[](unsigned char c){return tolower(c);});

    cout<<a.compare(b)<<endl;

    return 0;
}
