#include <bits/stdc++.h>
using namespace std;
int Acc;

int main ()
{
    string s;
    cin>>s;

    Acc=0;
    char * perv = &s.at(0);
    for(char & c : s){
        if(c != *perv){
            Acc=1;
        } else {
            Acc+=1;
            if (Acc>=7){
                cout<<"YES"<<endl;
                exit(0);
            }
        }
        perv=&c;
    }
    cout<<"NO"<<endl;
    exit(0);
}
