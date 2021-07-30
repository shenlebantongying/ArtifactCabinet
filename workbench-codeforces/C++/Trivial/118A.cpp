#include <bits/stdc++.h>
using namespace std;

int main() {
    string s;
    string res;
    cin >> s;
    for(char i: s){
        i= (char) tolower(i);
        if(i=='a'||i=='e'||i=='o'||i=='u'||i=='i'||i=='y'){
            continue;
        } else {
            res+='.';
            res+=i;
        }
    }
    cout<<res<<endl;
    exit(0);
}
