#pragma once 

#include <vector>
#include <string>
#include <sstream>
#include <algorithm>
#include <numeric>
#include <cstdlib>
#include <ranges>
#include <string_view>
#include <unordered_map>

namespace aoc::year2025::day01 {

struct Groups {
    std::vector<int> first_location_ids;
    std::vector<int> second_location_ids;
};


int to_int(auto&& range) {
    return std::stoi(std::string(range.begin(), range.end()));
}

Groups parse(const std::string& input) {
    Groups result;

    auto lines = input
               | std::views::split('\n')
               | std::views::filter([](auto&& line) {
                    return std::ranges::distance(line) > 0;
                 });

    auto parse_line = [](auto&& line_range) -> std::pair<int, int> {
        auto fields = line_range
            | std::views::split(' ')
            | std::views::filter([](auto&& f) {
                  return std::ranges::distance(f) > 0;
              });
        auto it = fields.begin();

        int first = to_int(*it++);
        int second = to_int(*it); // <- FIXED HERE

        return {first, second};
    };

    auto pairs = lines | std::views::transform(parse_line);

    // Destructure into result vectors
    std::ranges::for_each(pairs, [&](const auto& p) {
        result.first_location_ids.push_back(p.first);
        result.second_location_ids.push_back(p.second);
    });

    return result;
}

int part1(const std::string& input) {
    Groups groups = parse(input);

    std::sort(groups.first_location_ids.begin(), groups.first_location_ids.end());
    std::sort(groups.second_location_ids.begin(), groups.second_location_ids.end());

    // Compute sum of absolute differences
    int total = 0;
    for (size_t i = 0; i < groups.first_location_ids.size(); ++i) {
        total += std::abs(groups.first_location_ids[i] - groups.second_location_ids[i]);
    }

    return total;
}

int part2(const std::string& input) {

    Groups groups = parse(input);

    std::unordered_map<int, int> right_frequency;

    // ranges::for_each ?
    for (int r: groups.second_location_ids) {
        right_frequency[r]++;
    }

    // range accumulate ?
    int score = 0;
    for (int l: groups.first_location_ids) {
        score += l * right_frequency[l];
    }

    return score;
}

}