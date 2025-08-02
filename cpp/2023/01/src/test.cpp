#include <iostream>
#include <optional>
#include <string>
#include <cassert>

#include "solution.h"

#include "../../../tools/io/io.h"


int main() {

    std::cout << "Test Day 01 - 2023" << std::endl;
    std::optional<std::string> maybe_input = aoc::tools::io::read_input("../input/test_input.txt");
    std::optional<std::string> maybe_input_2 = aoc::tools::io::read_input("../input/test_input_2.txt");
    if (!maybe_input.has_value() || !maybe_input_2.has_value()) {
        std::cout << "Cannot read input .. " << std::endl;
        return 1;
    }
    std::string input = maybe_input.value();
    std::string input_2 = maybe_input_2.value();
    auto part1_res = aoc::year2023::day01::part1(input);
    std :: cout << "Result of Part 1 : " << part1_res << std::endl;
    auto part2_res = aoc::year2023::day01::part2(input_2);
    std :: cout << "Result of Part 2 : " << part2_res << std::endl;
    assert(part1_res == 142);
    assert(part2_res == 281);

    std::cout << "All tests passed!\n";
}