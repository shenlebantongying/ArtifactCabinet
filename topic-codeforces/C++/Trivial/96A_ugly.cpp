#include <bits/stdc++.h>
using namespace std;
int zeroAcc,oneAcc;

// can be improved by use single Acc

void reset_to_one(){
    zeroAcc = 1;
    oneAcc  = 1;
}

int main ()
{
    string s;
    cin>>s;

    zeroAcc = 0;
    oneAcc  = 0;
    char * perv = &s.at(0);

    for(char & c : s){
        if(c != *perv){
            reset_to_one();
        } else{
            switch (c) {
                case '0':
                    zeroAcc+=1;
                    if (zeroAcc>=7) {
                        cout<<"YES"<<endl;
                        exit(0);
                    }
                    break;
                case '1':
                    oneAcc+=1;
                    if (oneAcc>=7) {
                        cout<<"YES"<<endl;
                        exit(0);
                    }
                    break;
                default:
                    break;
            }
        }
        perv=&c;
        // cout<<"cur-> "<<c<<"-> "<<zeroAcc<<" "<<oneAcc<<endl;
    }
    cout<<"NO"<<endl;
    exit(0);
}

