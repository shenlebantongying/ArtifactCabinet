#pragma once

#include <format>
#include <string_view>

auto split(std::string_view source, std::string_view delimiters = " ")
    -> std::vector<std::string_view>;

auto match(std::string_view input,
           const std::vector<std::string_view> &candidates)
    -> std::vector<std::string_view>;
