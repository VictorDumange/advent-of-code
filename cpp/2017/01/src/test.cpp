#include "../../../tools/io/io.h"
#include "../src/solution.h"

#include <cassert>
#include <iostream>
#include <unordered_map>
#include <string>

int main() {

    std::cout << "Test Day 01 - 2017" << std::endl;
    
    std::unordered_map<std::string,int> test_cases_1 = {
        {"1122", 3}, {"1111", 4}, {"1234", 0}, {"91212129",9}
    };

    for (const auto&[input, expected]: test_cases_1) {
        assert(aoc::year2017::day01::part1(input) == expected);
    }

    std::unordered_map<std::string, int> test_cases_2 = {
        {"1212", 6}, {"1221", 0}, {"123425", 4}, {"123123",12}, {"12131415", 4}
    };

    for (const auto&[input, expected]: test_cases_2) {
        assert(aoc::year2017::day01::part2(input) == expected);
    }

    std::cout << "All tests passed!\n";
}