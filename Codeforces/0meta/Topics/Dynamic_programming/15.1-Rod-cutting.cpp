/*
 * Textbook: The introduction to Algorithm 3rd
 * Chapter 15.1 Rod cutting
 *
 * TODO: clear mind on 0-based and 1-based
 * TODO: unfinished!
 */


#include<bits/stdc++.h>

using namespace std;

// n => total length
// p[i] => price for sub-length i
// q => highest price
vector<int> p;
vector<int> result;

void print_duration(void (*lambda)()){
    auto t1 = chrono::high_resolution_clock::now();
    lambda();
    auto t2 = chrono::high_resolution_clock::now();
    cout<<(t2-t1).count()<<" ms"<<endl;
}

bool test(const function<int (int)>& f){
    for (int i = 1; i <p.size(); ++i)
    {
        //cout<<result[i-1]<<" "<<f(i)<<endl;
        if(result[i-1]!=f(i)){
            return false;
        } else {
            continue;
        }
    }
    return true;
}

void justify(bool x){
    if(x){
        cout<<"YES"<<endl;
    } else {
        cout<<"NO" <<endl;
    }
}


// Recursion method;
int rec_cut_rod(int n)
{   if(n==1) return 1;
    if(n==0) return 0;
    int q=0;
    for (int i = 1; i < n; i++) {
        // why n-i-1?
        // Because p[i] actually add 1 to actual value
        // due to zero-based array
        q=max(q,p[i]+ rec_cut_rod(n-i-1));
    }
    return q;
}

int bottom_up_cut_rod(int n){
    if(n==1) return 1;
    n+=1;
    std::vector<int> dp(n+3,0);
    for (int j =1; j < n; ++j) {
        int q=0;
        for (int i = 1; i < j; ++i) {
            q=max(q,p[i]+dp[j-i-1]);
        }
        dp[j]=q;
    }

    return dp[n-1];
}

int main() {

    result={1,5,8,10,13,17,18,22,25,30};
    p={1,5,8,9,10,17,17,20,24,30};

    print_duration( [](){rec_cut_rod(10);} );
    print_duration( [](){bottom_up_cut_rod(10);} );

    justify(test(rec_cut_rod));
    justify(test(bottom_up_cut_rod));

    return 0;
}

