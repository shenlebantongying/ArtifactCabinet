/*
 * Textbook: The introduction to Algorithm 3rd
 * Chapter 15.4 LCS Page 393
 */

#include<bits/stdc++.h>

using namespace std;

// TODO: replace array with vector or other C++ native datatype

// Scan a line to a vector
// TODO: a simple C alternative.
// https://stackoverflow.com/questions/51215473/how-can-i-take-several-integers-as-input-until-newline-occurs
void scan_line(vector<char> &dest) {
    dest.push_back(0);
    string text_line;
    getline(cin, text_line);
    istringstream input_stream(text_line);
    char cr;
    while (input_stream >> cr) {
        dest.push_back(cr);
    }
}


enum lcsArrow {
    aUp, aLeft, aUpLeft
};

// X_i, Y_j
vector<char> X;
vector<char> Y;

unsigned int m, n;

template<class T>
void print_array(vector<vector<T>> array) {
    for (int i = 0; i < m; ++i) {
        for (int j = 0; j < n; ++j) {
            cout << array[i][j];
        }
        cout << endl;
    }
    cout << endl;
}

void print_array(vector<vector<lcsArrow>> array) {
    for (int i = 0; i < m; ++i) {
        for (int j = 0; j < n; ++j) {
            switch (array[i][j]) {
                case aUp:
                    cout << "up" << " ";
                    break;
                case aLeft:
                    cout << "lf" << " ";
                    break;
                case aUpLeft:
                    cout << "ul" << " ";
                    break;
            }
        }
        cout << endl;
    }
    cout << endl;
}

void LCS_length(vector<vector<lcsArrow>> &b, vector<vector<int>> &c) {

    for (int i = 0; i < m; ++i) {
        c[i][0] = 0;
    }

    for (int j = 0; j < n; ++j) {
        c[0][j] = 0;
    }

    for (int i = 1; i < m; ++i) {
        for (int j = 1; j < n; ++j) {

            if (X[i] == Y[j]) {
                c[i][j] = c[i - 1][j - 1] + 1;
                b[i][j] = aUpLeft;
            } else if (c[i - 1][j] >= c[i][j - 1]) {
                c[i][j] = c[i - 1][j];
                b[i][j] = aUp;
            } else {
                c[i - 1][j] = c[i][j - 1];
                b[i][j] = aLeft;
            }
        }
    }

    print_array(c);
    print_array(b);
}

void print_LCS(vector<vector<lcsArrow>> &b, vector<char> &x, unsigned int i, unsigned int j) {
    if (0 == i || 0 == j)
        return;

    if (b[i][j] == aUpLeft) {
        print_LCS(b, x, i - 1, j - 1);
        cout << x[i] << " ";
    } else if (b[i][j] == aUp) {
        print_LCS(b, x, i - 1, j);
    } else {
        print_LCS(b, x, i, j - 1);
    }

}

int main() {
    cin >> noskipws;
    scan_line(X);
    scan_line(Y);

    m = X.size();
    n = Y.size();

    vector<vector<lcsArrow>> b(m, vector<lcsArrow>(n));
    vector<vector<int>> c(m, vector<int>(n));

    LCS_length(b, c);
    print_LCS(b, X, m - 1, n - 1);
    return 0;
}

/******************************

Input:

A B C B D A B
B D C A B A

Theory result

   X: A B   C B D A B
diff: - | + | - - | | +
   B:   B D C     A B A
 LCS:   B   C B   A

Expected output:

0000010
0010102
0110222
0112223
0102233
0122030
0122334
0122344

up up up up up up up
up up up up ul lf ul
up ul lf up up ul up
up up lf ul lf up up
up ul lf up lf ul lf
up up ul up lf up lf
up up up up ul up ul
up ul up up up ul up

B C B A

*****************************/
