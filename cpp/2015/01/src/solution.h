#include <string>

#include <ranges>

namespace aoc::year2015::day01 {

int part1(const std::string& input) {
    int floor = 0;
    for (int i=0; i<input.size(); ++i) {
        if (input[i] == '(') floor++;
        else if (input[i] == ')') floor--;
    }
    return floor;
}

int part2(const std::string& input) {
    int floor = 0;
    for (int i=0; i<input.size(); ++i) {
        if (input[i] == '(') floor++;
        else if (input[i] == ')') floor--;
        if (floor == -1) return i+1;
    }
    return input.size();
}

}