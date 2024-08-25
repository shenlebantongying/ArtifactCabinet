#include "fuzzy.h"
#include <algorithm>
#include <print>

void assert_vec_eq(const std::vector<std::string_view> &lhs,
                   std::initializer_list<std::string_view> rhs) {
  if (!std::equal(lhs.cbegin(), lhs.cend(), rhs.begin())) {
    std::print("Fail -> {} {}", lhs, rhs);
    exit(1);
  }
}

int main() {
  // splitting
  assert_vec_eq(split("a_b_c", "_"), {"a", "b", "c"});
  // matching

  assert_vec_eq(match("hel wor", {"world ", "hello word", "hello", "nice"}),
                {"hello word", "world ", "hello"});
}
