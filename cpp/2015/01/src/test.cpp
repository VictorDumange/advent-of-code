#include <iostream>
#include <optional>
#include <string>
#include <cassert>

#include "solution.h"

#include "../../../tools/io/io.h"

#include "unordered_map"

int main() {

    std::unordered_map<std::string, int> test_cases_1 = {
        {"(())", 0}, {"()()", 0},
        {"(((", 3} , {"(()(()(", 3}, {"))(((((", 3},
        {"())", -1}, {"))(", -1},
        {")))", -3}, {")())())", -3}
    };

    for (const auto& [input, expected] : test_cases_1) {
        assert(aoc::year2015::day01::part1(input) == expected);
    }

    std::unordered_map<std::string, int> test_cases_2 = {
        {")", 1}, {"()())", 5}
    };

    for (const auto& [input, expected] : test_cases_2) {
        assert(aoc::year2015::day01::part2(input) == expected);
    }

    std::cout << "All tests passed!\n";

    return 0;
}