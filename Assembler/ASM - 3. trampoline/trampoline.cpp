// g++ -std=c++11 -Werror -Wall -o tramp types.h memmgr.h memmgr.cpp trampoline.h trampoline.cpp

#include "trampoline.h"
#include <iostream>

int main () {
	int external = 43;
	
	Trampoline <int (int)> simple (
		[&] (int a) {
			std::cout << "First\n"; 
			return printf ("a=%d, ext=%d\n", a, external);
		}
	);
	
	auto f = simple.get ();
	f (2);
	
	// JS, is that YOU?
	simple.get () (435);
	
	Trampoline <int (int, int, int, int, int, int, int, int)> eight (
		[&] (int a1, int a2, int a3, int a4, int a5, int a6, int a7, int a8) {
			std::cout << "Second\n"; 
			return printf ("a1=%d, a2=%d, a3=%d, a4=%d, a5=%d, a6=%d, a7=%d, a8=%d, ext=%d\n",
				a1, a2, a3, a4, a5, a6, a7, a8, external);
		}
	);
	
	auto f1 = eight.get ();
	f1 (1, 2, 3, 4, 5, 6, 7, 8);

	Trampoline <int (double, double, float)> dbl_and_flt (
		[&] (double a1, double a2, float a3) {
			std::cout << "Third\n";
			return printf ("a1=%f, a2=%f, a3=%f, ext=%d\n",
				a1, a2, a3, external);
		}
	);
	
	auto f2 = dbl_and_flt.get ();
	f2 (1.2, 43.4, 32.3);
	
	external = 205;
	f2 (132.2, 429.4, 32.3);
	
	Trampoline <int (int, int, double, int, int, double, int, int, int, float)> hell_boy (
		[&] (int a1, int a2, double a3, int a4, int a5, double a6, int a7, int a8, int a9, float a10) {
			std::cout << "Fourth\n";
			return printf ("a1=%d, a2=%d, a3=%f, a4=%d, a5=%d, a6=%f, a7=%d, a8=%d, a9=%d, a10=%f, ext=%d\n",
				a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, external);
		}
	);
	
	auto f31 = hell_boy.get ();
	auto f32 = hell_boy.get ();
	
	f31 (546, 572, 1.54, 76, 12, 2.6, 1, 676, -32, 0.3242);
	
	external = 32;
	f32 (52, 97, 1.2414, 2176, 123, -2.4356, 143, 676324, -4232, 0.3234242);

	Trampoline <int (float, float, float, float, float, float, float, float, float, float, int, int, int)> too_many_floats (
		[&] (float a1, float a2, float a3, float a4, float a5, float a6, float a7, float a8, float a9, float a10, int a11, int a12, int a13) {
			std::cout << "Fifth\n";
			return printf ("a1=%f, a2=%f, a3=%f, a4=%f, a5=%f, a6=%f, a7=%f, a8=%f, a9=%f, a10=%f, a11=%d, a12=%d, a13=%d, ext=%d\n",
				a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, external);
		}
	);

	auto ff = too_many_floats.get ();
	ff (1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11, 12, 13);
	
	return 0;
}