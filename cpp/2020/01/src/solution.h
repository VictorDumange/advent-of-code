#include <string>
#include <unordered_set>
#include <ranges>
#include <string_view>
#include <charconv>
#include <optional>
#include <vector>

namespace aoc::year2020::day01 {

int to_int(std::string_view sv) {
    int result = 0;
    std::from_chars(sv.data(), sv.data() + sv.size(), result);
    return result;
}

auto parse(const std::string& input) {
    return input 
        | std::views::split('\n')
        | std::views::filter([](auto&& rv) {
            std::string_view sv(&*rv.begin(), std::ranges::distance(rv));
            return !sv.empty();
        })
        | std::views::transform([](auto&& rv) {
            std::string_view sv(&*rv.begin(), std::ranges::distance(rv));
            return to_int(sv);
        });
}

template<std::ranges::input_range Range>
std::optional<std::pair<int, int>> two_sum(Range&& range, int target) {
    std::unordered_set<int> seen;

    auto it = std::ranges::find_if(range, [&](int value) {
        int complement = target - value;
        if (seen.contains(complement)) {
            return true;
        } else {
            seen.insert(value);
            return false;
        }
    });

    if (it == std::ranges::end(range)) return std::nullopt;
    
    int found = *it;
    return std::pair{found, target - found};
}


// 2-Sum
int part1(const std::string& input) {
    auto values = parse(input);
    auto maybe_pair = two_sum(values, 2020);
    if (!maybe_pair.has_value()) throw std::runtime_error("Not found");
    
    auto [a, b] = maybe_pair.value();
    return a * b;
}

// 3-Sum ysing 2-Sum
int part2(const std::string& input) {
    auto values = parse(input);

    std::vector<int> vec_values(values.begin(), values.end());

    for (std::size_t i = 0; i < vec_values.size(); ++i) {
        int a = vec_values[i];
        int target = 2020 - a;

        // View of the remaining elements (after i)
        auto tail = std::views::drop(vec_values, i + 1);

        if (auto maybe_pair = two_sum(tail, target); maybe_pair.has_value()) {
            auto [b, c] = *maybe_pair;
            return a * b * c;
        }
    }

    throw std::runtime_error("Three numbers summing to 2020 not found");
}




}