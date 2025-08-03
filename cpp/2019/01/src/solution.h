#include <string>
#include <ranges>
#include <string_view>
#include <numeric>
#include <functional>

namespace aoc::year2019::day01 {

auto to_int = [](auto&& range) {
    return std::stoi(std::string(range.begin(), range.end()));
};

auto non_empty_range = [](auto&& range) {
    return std::ranges::distance(range) > 0;
};

auto fuel_requirement_elementar = [](int mass) {
    return mass / 3 - 2;
};

std::function<int(int)> fuel_requirement_corrected = [](int fuel) -> int {
    int current = fuel_requirement_elementar(fuel);
    if (current <= 0) return 0;
    return current + fuel_requirement_corrected(current);
};

auto parse = [](const std::string& input) {
    return input
        | std::views::split('\n')
        | std::views::filter(non_empty_range)
        | std::views::transform(to_int);
};

int part1(const std::string& input) {
    auto values = parse(input)
        | std::views::transform(fuel_requirement_elementar);

    return std::accumulate(values.begin(), values.end(), 0);
}

int part2(const std::string& input) {
    auto values = parse(input)
        | std::views::transform(fuel_requirement_corrected);

    return std::accumulate(values.begin(), values.end(), 0);
}


}