#include "fuzzy.h"
#include <map>
#include <ranges>

auto split(std::string_view source, const std::string_view delimiters)
    -> std::vector<std::string_view> {
  std::vector<std::string_view> results;

  source.remove_prefix(source.find_first_not_of(delimiters));

  while (!source.empty()) {
    if (const auto r = source.find_first_of(delimiters);
        r == std::string_view::npos) {
      results.push_back(source);
      break;
    } else {
      results.push_back(source.substr(0, r));
      source.remove_prefix(r + 1);
      if (auto n = source.find_first_not_of(delimiters);
          n != std::string_view::npos) {
        source.remove_prefix(n);
      }
    }
  }
  return results;
}

auto single_candidate_match(std::string_view input, std::string_view candidate)
    -> int {
  auto prefixes = split(input);
  auto words = split(candidate);
  auto rank = 0;
  for (auto prefix : prefixes) {
    for (auto word : words) {
      if (word.starts_with(prefix)) {
        rank += 1;
      }
    }
  }

  return rank;
}

auto match(const std::string_view input,
           const std::vector<std::string_view> &candidates)
    -> std::vector<std::string_view> {
  std::multimap<int, std::string_view, std::greater<>> results{};

  for (auto candidate : candidates) {
    if (auto rank = single_candidate_match(input, candidate); rank > 0) {
      results.insert({single_candidate_match(input, candidate), candidate});
    }
  }

  return results | std::views::values |
         std::ranges::to<std::vector<std::string_view>>();
}
