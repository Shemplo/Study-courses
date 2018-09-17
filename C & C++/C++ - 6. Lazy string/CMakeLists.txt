cmake_minimum_required(VERSION 3.3.2)
project(LazyString)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

set(SOURCE_FILES main.cpp)
add_executable(task6 ${SOURCE_FILES})
add_library(lazyString lazyString.cpp)

target_link_libraries(task6 lazyString)

install(TARGETS lazyString DESTINATION lib)
install(FILES lazyString.h DESTINATION include)