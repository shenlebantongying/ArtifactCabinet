#include <bits/stdc++.h>
using namespace std;

int main ()
{
    string s;
    cin>>s;

    int acc=1;

    for(pair<string::iterator,string::iterator>i(s.begin(),s.begin()+1);
        i.second != s.end();
        ++i.first,++i.second)
    {
        if (*i.first==*i.second){
            acc+=1;
            if (acc>=7){
                cout<<"YES"<<endl;
                exit(0);
            }
        } else {
            acc=1;
        }
    }
    cout<<"NO"<<endl;
    exit(0);
}

