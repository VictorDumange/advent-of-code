#include <string>
#include <iostream>

namespace aoc::year2017::day01 {

int compute_sum(const std::string& input, int k) {
    int sum = 0;
    int n = input.size();

    for (int i = 0; i < n; ++i) {
        if (input[i] == input[(i + k) % n]) {
            sum += input[i] - '0';  
        }
    }

    return sum;
}

int part1(const std::string& input) {
    return compute_sum(input, 1);
}

int part2(const std::string& input) {
    int k = input.size()/2;
    return compute_sum(input, k);
}

}