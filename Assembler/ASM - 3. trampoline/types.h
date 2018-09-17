#ifndef TYPES_H
#define TYPES_H

#include <xmmintrin.h>

/**
 * Structure that saves the number of
 * integer and SSE arguments givent as parametrs
 */
template <typename ... Args>
struct types;

template <>
struct types <> {
	static const int reg = 0;
	static const int sse = 0;
};

template <typename Return, typename ... Args>
struct types <Return, Args ...> {
	// Here must be more accurate check for the return statement
	static const int reg = types <Args ...>::reg + 1;
	static const int sse = types <Args ...>::sse;
};

template <typename ... Args>
struct types <double, Args ...> {
	static const int reg = types <Args ...>::reg;
	static const int sse = types <Args ...>::sse + 1;
};

template <typename ... Args>
struct types <float, Args ...> {
	static const int reg = types <Args ...>::reg;
	static const int sse = types <Args ...>::sse + 1;
};

#endif // TYPES_H