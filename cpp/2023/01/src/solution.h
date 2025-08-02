#include <iostream>
#include <string_view>
#include <ranges>
#include <cctype>
#include <numeric>
#include <string>

#include <unordered_map>

namespace aoc::year2023::day01 {

int to_int(auto&& r) {
    int result = 0;
    for (char c : r) {
        if (c >= '0' && c <= '9') {
            result = result * 10 + (c - '0');
        }
    }
    return result;
}

int part1(const std::string& input) {
    std::string_view input_view = input;

    auto lines = input_view 
        | std::views::split('\n')
        | std::views::filter([](auto&& l) {
            std::string_view sv_l(l.begin(), l.end());
            return !sv_l.empty();
        });

    auto digits_only = lines
        | std::views::transform([](auto&& line_range) {
            return line_range 
                | std::views::filter([](char c) { return std::isdigit(c); });
        });

    auto kept_digits = digits_only
        | std::views::transform([](auto&& digits_range) {
            // materialize in order to use front and back for exeprssivness
            auto sv = std::string(digits_range.begin(), digits_range.end());
            char first = sv.front();
            char last = sv.back();

            // alternative : on the fly computation.
            /*
            auto it = std::ranges::begin(digits_range);
            auto end = std::ranges::end(digits_range);

            if (it == end) {
                return std::string(); // no digits
            }

            char first = *it;
            char last = first;

            for (; it != end; ++it) {
                last = *it;
            }
            */
            return std::string{first, last};
        });

    auto integers = kept_digits
        | std::views::transform([](auto&& r) { return to_int(r); });

    return std::accumulate(integers.begin(), integers.end(), 0);
}

void replace_str_by_digit(
    std::unordered_map<std::string, std::string>& replacing_instructions,
    const std::string& initial_input,
    int current_pos,
    std::string& updated_input
) {
    if (current_pos >= initial_input.size()) return;

    for (int len = 5; len >= 3; --len) {
        if (current_pos + len <= initial_input.size()) {
            std::string sliding_window = initial_input.substr(current_pos, len);
            if (replacing_instructions.contains(sliding_window)) {
                updated_input.append(replacing_instructions[sliding_window]);
                replace_str_by_digit(replacing_instructions, initial_input, current_pos + 1, updated_input);
                return;
            }
        }
    }

    updated_input.push_back(initial_input[current_pos]);
    replace_str_by_digit(replacing_instructions, initial_input, current_pos + 1, updated_input);
}


int part2(const std::string& input) {
    
    std::unordered_map<std::string, std::string> replacing_instructions = {
        {"one", "1"},
        {"two", "2"},
        {"three", "3"},
        {"four", "4"},
        {"five", "5"},
        {"six", "6"},
        {"seven", "7"},
        {"eight", "8"},
        {"nine", "9"},
    };

    std::string updated_input;
    replace_str_by_digit(replacing_instructions, input, 0, updated_input);
    return part1(updated_input);

}



}