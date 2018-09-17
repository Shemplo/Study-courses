cmake_minimum_required(VERSION 3.1)

project(Format CXX)

add_library(format STATIC format.cpp format.cpp format.h)
target_compile_features(format PRIVATE cxx_variadic_templates)
install (TARGETS format DESTINATION ${CMAKE_INSTALL_PREFIX}/lib)
install (FILES format.h DESTINATION ${CMAKE_INSTALL_PREFIX}/include)

add_definitions(-DRELEASE)