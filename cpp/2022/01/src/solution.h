
#include <string>
#include <ranges>
#include <string_view>
#include <algorithm>
#include <functional>   
#include <numeric>

namespace aoc::year2022::day01 {

int to_int(auto&& range) {
    return std::stoi(std::string(range.begin(), range.end()));
}

auto compute_calories(const std::string& input, int k) {

    auto input_per_elf = input 
        | std::views::split(std::string_view("\n\n"));

    auto food_per_elf = input_per_elf
        | std::views::transform([](auto&& block) {
            return block 
            | std::views::split('\n')
            | std::views::transform([](auto&& range) {
                return to_int(range);
            });
        });
    
    auto calories_per_elf = food_per_elf
        | std::views::transform([](auto&& calories) {
            return std::ranges::fold_left(calories, 0, std::plus<int>());
        });

    std::vector<int> calories_vec(calories_per_elf.begin(), calories_per_elf.end());
    std::ranges::sort(calories_vec, std::greater{});

    // sum the top k elements
    return std::accumulate(calories_vec.begin(), calories_vec.begin() + k, 0);
}

int part1(const std::string& input) {
    return compute_calories(input, 1);
}

int part2(const std::string& input) {
    return compute_calories(input, 3);
}

}