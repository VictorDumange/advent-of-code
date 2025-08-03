#include <string>
#include <ranges>
#include <string_view>
#include <algorithm>
#include <numeric>
#include <vector>

namespace aoc::year2021::day01 {

int to_int(auto&& range) {
    return std::stoi(std::string(range.begin(), range.end()));
}

auto parse(const std::string& input) {
    return input
        | std::views::split('\n')
        | std::views::filter([](auto&& rv) {
            std::string_view sv(&*rv.begin(), std::ranges::distance(rv));
            return !sv.empty();
        })
        | std::views::transform([](auto&& rv) {
            return to_int(rv);
        });
}

int compute_increasing(auto&& measurements) {
    auto comparing_measurements = measurements
        | std::views::slide(2);

    return std::ranges::fold_left(comparing_measurements, 0, [](int acc, auto&& pair) {
        auto it = pair.begin();
        int first = *it++;
        int second = *it;
        return acc + (second > first ? 1 : 0);
    });
}

int part1(const std::string& input) {
    auto measurements = parse(input);
    return compute_increasing(measurements);
}
 

int part2(const std::string& input) {
    auto line_view = parse(input);

    // Store into a concrete container so slide and transform can work reliably
    std::vector<int> measurements(line_view.begin(), line_view.end());

    // compute sum of three_sliding windows
    auto three_measurement_windows = measurements
        | std::views::slide(3)
        | std::views::transform([](auto&& window) {
            return std::accumulate(window.begin(), window.end(), 0);
        });

    return compute_increasing(three_measurement_windows);
}

}