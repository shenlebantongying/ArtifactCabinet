+ https://codeforces.com/blog/entry/64218
+ https://codeforces.com/blog/entry/10124

```cpp
// Codeforces require speed rather than safty or architecture

#define MIN(a,b) (a) < (b) ? (a) : (b)
#define MAX(a,b) (a) > (b) ? (a) : (b)
#define ABS(a) (a) > 0 ? (a) : -(a)

// Include EVERY std librarys of C and C++
// https://github.com/gcc-mirror/gcc/blob/master/libstdc%2B%2B-v3/include/precompiled/stdc%2B%2B.h
// Path under GCC tree libstdc++-v3/include/precompiled/stdc++.h
#include <bits/stdc++.h>

using namespace std;

//Speed up hack
ios_base::sync_with_stdio(false);
cin.tie(NULL);

// Read all lines
cin >> n;
while (n--){}

// <algorithm> sorting first 30 of a char[]
#include <algorithm>
char a[100];
int j(30);
sort(a,a+j);

#define all(x) (x).begin(), (x).end()
sort(vec.begin(), vec.end()) => sort(all(vec))

//Output answer
cout<<x<<endl;
```

built-in c++1x functionos:

Greatest common divisor (gcd) -> the minimum is always 1

Least common multiple   (lcm) -> the biggest is always infinity
