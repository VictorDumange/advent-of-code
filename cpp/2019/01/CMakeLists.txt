cmake_minimum_required(VERSION 3.14)
project(2019day01)

# Set the C++ standard globally
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_executable(main src/main.cpp)
target_include_directories(main PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../../tools)

add_executable(test src/test.cpp)
target_include_directories(test PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../../tools)
