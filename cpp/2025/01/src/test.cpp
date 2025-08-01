#include "../../../tools/io/io.h"
#include "../src/solution.h"

#include <cassert>
#include <iostream>

int main() {

    std::cout << "Test Day 01 - 2025" << std::endl;
    std::optional<std::string> maybe_input = aoc::tools::io::read_input("../input/test_input.txt");
    if (!maybe_input.has_value()) {
        std::cout << "Cannot read input .. " << std::endl;
        return 1;
    }
    std::string input = maybe_input.value();
    assert(aoc::year2025::day01::part1(input) == 11);
    assert(aoc::year2025::day01::part2(input) == 31);

    std::cout << "All tests passed!\n";
}