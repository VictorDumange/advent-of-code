# Advent of Code — a functional approach

Reading *Structure and Interpretation of Computer Programs* made me want to explore how
different languages let you solve problems with a functional approach. This repository is
that exploration: [Advent of Code](https://adventofcode.com/) puzzles solved in **OCaml**,
**Clojure** and **C++**.

The goal is not only to solve each puzzle, but to see how far small, composable building
blocks can go: pure functions, immutable and persistent data structures, and a reusable
toolbox per language that grows from one puzzle to the next.

## Languages

| Language | Puzzles | Highlights |
|---|---|---|
| [OCaml](ocaml/aoc) | 2024 | Modules and functors, `.mli` signatures, persistent data structures |
| [Clojure](clojure) | 2025 | Sequence transformations, immutable maps and sets, tested toolbox |
| [C++](cpp) | 2015–2024 | Functional style with ranges, views and concepts |

## OCaml

Built with [dune](https://dune.build/). Each day is a module exposing `part1` and `part2`,
built on top of a small toolbox:

- `Pqueue` — persistent min-priority queue (leftist heap), used for Dijkstra-like searches
- `Fqueue` — persistent FIFO queue (two lists), used for breadth-first searches
- `Graph` — graph functor, generic over the node type
- `Grid` — 2D grid parsing and navigation
- `Parsing`, `Number`, `Algebra` — input parsing and arithmetic helpers

```bash
cd ocaml/aoc
dune build
dune test
```

## Clojure

Solutions live in `src/year2025`, the reusable toolbox in `src/toolbox`: bit manipulation,
disjoint-set union, graphs, grids, intervals, parsing and tables, with tests in `test/toolbox`.

## Repository structure

```
ocaml/aoc/
├── tools/        # Reusable toolbox (Pqueue, Fqueue, Graph, Grid, ...)
└── years/2024/   # One module per day
clojure/
├── src/toolbox/  # Reusable toolbox
├── src/year2025/ # One namespace per day
└── test/         # Toolbox tests
cpp/              # C++ solutions
```

## Inputs

Puzzle inputs are not committed, as requested by Advent of Code. To run a solution, place
your own input in an `input/` folder next to the sources (for OCaml:
`ocaml/aoc/input/<year>/dayXX.txt`).
