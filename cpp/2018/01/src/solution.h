#include <string>
#include <ranges>
#include <numeric>
#include <unordered_set>

namespace aoc::year2018::day01 {

auto to_int = [](auto&& range) {
    return std::stoi(std::string(range.begin(), range.end()));
};

auto non_empty_range = [](auto&& range) {
    return std::ranges::distance(range) > 0;
};

auto parse = [](const std::string& input) {
    return input
        | std::views::split('\n')
        | std::views::filter(non_empty_range)
        | std::views::transform(to_int);
};

int part1(const std::string& input) {
    auto frequencies = parse(input);
    return std::accumulate(frequencies.begin(), frequencies.end(), 0);
}

int part2(const std::string& input) {
    
    auto frequencies = parse(input);
    int current_freq = 0;
    std::unordered_set<int> seen_frequencies = {current_freq};

    // very ugly, but recusivity causes stack overflow
    // maybe should use tail-rec
    while (true) {
        for (const auto& frequency : frequencies) {
            current_freq += frequency;
            if (seen_frequencies.contains(current_freq)) return current_freq;
            else seen_frequencies.insert(current_freq);
        }
    } 
    throw std::runtime_error("No frequency visisted twice");
}

}