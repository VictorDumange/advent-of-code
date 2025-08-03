#include <string>
#include <vector>
#include <ranges>
#include <charconv>
#include <algorithm>
#include <cctype>
#include <stdexcept>
#include <iostream>
#include <unordered_set>


namespace aoc::year2016::day01 {

enum class Rotation { L, R };
enum class Orientation { N, E, S, W };

struct Instruction {
    Rotation rotation;
    int distance;
};

struct Position {
    int x = 0;
    int y = 0;

    friend bool operator==(const Position& a, const Position& b) {
        return a.x == b.x && a.y == b.y;
    }
};

struct State {
    Position pos;
    Orientation orientation;
};

Orientation apply_rotation(Orientation current, Rotation rot) {
    switch (current) {
        case Orientation::N: return rot == Rotation::L ? Orientation::W : Orientation::E;
        case Orientation::E: return rot == Rotation::L ? Orientation::N : Orientation::S;
        case Orientation::S: return rot == Rotation::L ? Orientation::E : Orientation::W;
        case Orientation::W: return rot == Rotation::L ? Orientation::S : Orientation::N;
    }
    throw std::runtime_error("Invalid orientation");
}

// Move forward in facing direction
State apply_translation(const State& state, int distance) {
    Position p = state.pos;
    switch (state.orientation) {
        case Orientation::N: p.y += distance; break;
        case Orientation::S: p.y -= distance; break;
        case Orientation::E: p.x += distance; break;
        case Orientation::W: p.x -= distance; break;
    }
    return {p, state.orientation};
}

State apply_instruction(const State& state, const Instruction& instr) {
    Orientation new_orientation = apply_rotation(state.orientation, instr.rotation);
    return apply_translation({state.pos, new_orientation}, instr.distance);
}

std::vector<Instruction> parse_instructions(const std::string& input) {
    std::vector<Instruction> result;
    auto parts = input 
    | std::views::split(',') 
    | std::views::transform([](auto&& r) {
        auto sv = std::string_view(&*r.begin(), std::ranges::distance(r));
        auto first = sv.find_first_not_of(" \n\r\t");
        return sv.substr(first); // trim leading whitespace
    });

    for (auto sv : parts) {
        if (sv.size() < 2) continue;

        Rotation rot = (sv[0] == 'L') ? Rotation::L : Rotation::R;
        int distance = std::stoi(std::string(sv.substr(1)));
        result.push_back({rot, distance});
    }

    return result;
}

int part1(const std::string& input) {
    auto instructions = parse_instructions(input);

    State state{{0, 0}, Orientation::E};
    for (const auto& instr : instructions) {
        state = apply_instruction(state, instr);
    }

    return std::abs(state.pos.x) + std::abs(state.pos.y);
}

struct PositionHash {
    std::size_t operator()(const aoc::year2016::day01::Position& p) const noexcept {
        std::size_t h1 = std::hash<int>{}(p.x);
        std::size_t h2 = std::hash<int>{}(p.y);
        return h1 ^ (h2 << 1);
    }
};

int part2(const std::string& input) {
    std::unordered_set<Position, PositionHash> visited_pos; // provide an hash
    auto instructions = parse_instructions(input);

    State state{{0, 0}, Orientation::E};
    for (const auto& instr : instructions) {
        state.orientation = apply_rotation(state.orientation, instr.rotation);

        for (int i = 0; i < instr.distance; ++i) {
            state = apply_translation(state, 1);

            if (visited_pos.contains(state.pos))
                return std::abs(state.pos.x) + std::abs(state.pos.y);
            else
                visited_pos.insert(state.pos);
        }
    }
    return std::abs(state.pos.x) + std::abs(state.pos.y);
}

} 


