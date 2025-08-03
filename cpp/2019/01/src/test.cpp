#include "../../../tools/io/io.h"
#include "../src/solution.h"

#include <cassert>
#include <iostream>
#include <unordered_map>

int main() {

    std::cout << "Test Day 01 - 2019" << std::endl;
    std::unordered_map<int, int> test_cases_1 = {
        {12, 2}, {14, 2}, {1969, 654}, {100756, 33583}
    };

    for (const auto& [mass, expected_fuel_requirement] : test_cases_1) {
        assert(aoc::year2019::day01::fuel_requirement_elementar(mass) == expected_fuel_requirement);
    }

    std::unordered_map<int, int> test_cases_2 = {
        {14, 2}, {1969, 966}, {100756, 50346}
    };

    for (const auto& [mass, expected_fuel_requirement] : test_cases_2) {
        int fuel = aoc::year2019::day01::fuel_requirement_corrected(mass);
        assert( fuel == expected_fuel_requirement);
    }

    std::cout << "All tests passed!\n";
}