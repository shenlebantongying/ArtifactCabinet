#include <bits/stdc++.h>
using namespace std;

#define MAX 100005

// TODO: better implementation

typedef long long ll;
ll n,m,a,b,between_distance;
ll travel_len[MAX],path[MAX];
map <pair<ll, ll> ,ll> lengths;
vector<ll>links[MAX];

void f (ll x)
{
    if (x!=1) f(path[x]);
    cout<<x<<' ';
}

int main ()
{
    cin>>n>>m;
    while (m--)
    {
        cin >> a >> b >> between_distance;
        links[a].push_back(b);
        links[b].push_back(a);
        lengths[{a, b}]=between_distance;
        lengths[{b, a}]=between_distance;
    }

    // we want shortest path, thus we can set travel length to n as maximum.
    fill_n(travel_len, MAX, std::numeric_limits<long long int>::max());

    priority_queue<ll> q;
    q.push(1); //beginning node

    while (!q.empty())
    {
        ll cur_node=q.top();
        q.pop();
        for (ll next_node:links[cur_node])
        {
            between_distance=lengths[{cur_node, next_node}];
            if (travel_len[cur_node] + between_distance < travel_len[next_node])
            {
                travel_len[next_node]= travel_len[cur_node] + between_distance;
                path[next_node]=cur_node;
                q.push(next_node);
            }
        }
    }

    if (travel_len[n] == std::numeric_limits<long long int>::max()){
        cout<<-1;
        exit(0);
    }
    f(n); // backward searching
    return 0;
}
