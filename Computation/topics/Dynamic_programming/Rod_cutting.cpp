/*
 * Textbook: The introduction to Algorithm 4th
 * Chapter 14.1 Rod cutting
 * TODO: not most efficient.
 */

#include "stdall.h"

// n => total length
// p[i] => price table for sub-length i
// result[i] => maximum possible value of a rod with length i
vector p = {0, 1, 5, 8, 9, 10, 17, 17, 20, 24, 30};

vector result = {0, 1, 5, 8, 10, 13, 17, 18, 22, 25, 30};

void test(const function<int(int)>& f) {
  vector f_result = views::iota(0, static_cast<int>(result.size())) |
                    views::transform([f](auto i) { return f(i); }) |
                    ranges::to<std::vector>();

  if (result != f_result) {
    println(
        "Wrong->\n"
        "theory -> {}\n"
        "actual -> {}\n",
        result, f_result);
  } else {
    println("ok.");
  }
}

// Naive Method1: Brutal force Recursion
int rec_cut_rod(int n) {
  if (n == 0) {
    return 0;
  }
  int q = 0;
  for (int i = 1; i <= n; i++) {
    q = max(q, p[i] + rec_cut_rod(n - i));
  }
  return q;
}

// Naive Method2: Bottom up
// dp[j] or dp[j-i]-> optimal cutting when cutting a certain length j (already
// cut part) dp[j] will be set by trying all possible cutting lengths i
int bottom_up_cut_rod(int n) {
  vector dp(n + 1, 0);
  for (int j = 0; j < dp.size(); j++) {
    int q = 0;
    for (int i = 1; i <= j; i++) {
      q = max(q, p[i] + dp[j - i]);
    }
    dp[j] = q;
  }
  return dp.back();
}

int main() {
  test(rec_cut_rod);
  test(bottom_up_cut_rod);

  return 0;
}
