#include <iostream>
#include <string>
#include <chrono>

#include "../../../tools/io/io.h"
#include "solution.h"

int main() {

    std::cout << "Day 01 - 2022" << std::endl;
    std::optional<std::string> maybe_input = aoc::tools::io::read_input("../input/input.txt");
    if (!maybe_input.has_value()){
        std::cout << "Unable to read input .. " << std::endl;
        return 1;
    }
    std::string input = maybe_input.value();

    auto start1 = std::chrono::high_resolution_clock::now();
    auto result1 = aoc::year2022::day01::part1(input);
    auto end1 = std::chrono::high_resolution_clock::now();

    auto start2 = std::chrono::high_resolution_clock::now();
    auto result2 = aoc::year2022::day01::part2(input);
    auto end2 = std::chrono::high_resolution_clock::now();

    std::cout << "Part 1: " << result1 << " (" << std::chrono::duration_cast<std::chrono::microseconds>(end1 - start1).count() << " µs)\n";
    std::cout << "Part 2: " << result2 << " (" << std::chrono::duration_cast<std::chrono::microseconds>(end2 - start2).count() << " µs)\n";

    return 0;

}