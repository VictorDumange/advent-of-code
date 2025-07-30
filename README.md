# 🎄 Advent of Code — C++ Solutions and Toolbox

This repository is my personal journey through [Advent of Code](https://adventofcode.com/) puzzles using **modern C++ (C++23)**. My goal is not only to solve the problems but to **build a reusable algorithmic toolbox** that explores and demonstrates:

- Generic programming using **templates** and **concepts**
- Idiomatic use of the **STL**, including **ranges**, **views**, and **spans**
- The C++ philosophy of **zero-cost abstractions**
- Functional patterns like **ad-hoc polymorphism** and **typeclass-like traits**
- How to structure a small yet powerful **reusable library** in modern C++

Could use other programming languages in the future.
---

## 🗂️ Project Structure

```bash
cpp/
├── toolbox/             # Reusable, generic algorithms and utilities
│   ├── algorithms/      # Primality, sorting, shortest paths, etc.
│   ├── parsing/         # Tokenization, line/group parsing, extractors
│   ├── util/            # Traits, range utilities, span helpers
│   └── toolbox.hpp      # Entry point header for the toolbox
├── 2022/                # AoC solutions by year
├── 2023/
├── 2024/
└── CMakeLists.txt       # Modular build setup
