#ifndef IO_H
#define IO_H

#include <fstream>
#include <optional>
#include <string>
#include <iterator>
#include <iostream>

namespace aoc::tools::io {

std::optional<std::string> read_input(const std::string& path) {
    std::ifstream in(path);
    if (!in) return std::nullopt;
    return std::string{std::istreambuf_iterator<char>(in), {}};
}

}


#endif
