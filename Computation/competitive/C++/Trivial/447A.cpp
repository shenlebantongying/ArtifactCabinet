#include <bits/stdc++.h>
using namespace std;

// TODO: check the tutorial, and implement with prefix sums instead.

int main() {
    string s;
    int acc{0};
    cin>>s;
    unsigned int s_len=s.length();
    for (auto a=0; a<s_len;a++){
        for (auto b=a;b<s_len;b++){
            for(auto c=b;c<s_len;c++){
                if ( s[a]=='Q' && s[b]=='A' && s[c]=='Q'){
                    acc+=1;
                };
            }
        }
    }
cout<<acc<<endl;
exit(0);

}
