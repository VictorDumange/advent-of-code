#include <iostream>
#include <optional>
#include <string>
#include <cassert>

#include "solution.h"

#include "../../../tools/io/io.h"

#include "unordered_map"

int main() {

    std::unordered_map<std::string, int> test_cases_1 = {
        {"R2, L3", 5}, {"R2, R2, R2", 2}, {"R5, L5, R5, R3", 12}
    };

    for (const auto& [input, expected] : test_cases_1) {
        assert(aoc::year2016::day01::part1(input) == expected);
    }

    assert(aoc::year2016::day01::part2("R8, R4, R4, R8") == 4);

    std::cout << "All tests passed!\n";

    return 0;
}