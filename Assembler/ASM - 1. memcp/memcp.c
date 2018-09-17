// gcc -std=c11 -Werror -Wall -o memcp memcp.c

#include <stdio.h>
#include <emmintrin.h>
#include <time.h>
#include <limits.h>

//#define SIZE ((1 << 30) + (1 << 29))
#define SIZE (1 << 28)

/**
 * (Memory copy)
 * 
 * Copies big massives of data.
 * 
 * @param src pointer to start of copying block
 * @param dest pointer to start of destination space
 * @param size amount of data to be copied
 * @return pointer to end of copied block
 *  
 */
void *memcp (void *src, void *dest, size_t size);

/**
 * (Part copy)
 *
 * Copies small massives of data.
 *
 * @param src pointer to start of copying block
 * @param dest pointer to start of destination space
 * @param size amount of data to be copied
 * @return pointer to end of copied block
 *
 */
void *prtcp (void *src, void *dest, size_t size);

int check (void *, void *, size_t);

/* ===| ENTER POINT |=== */

int main () {
	srand (time (0));
	
	char *src = malloc (sizeof (char) * (SIZE + 10));
	char *dest = malloc (sizeof (char) * (SIZE + 10));
	
	printf ("Making part copy\n");
	for (size_t i = 0; i < SIZE; i ++) {
		src [i] = rand () % 128;
		dest [i] = rand () % 128;
	}
	
	clock_t start = clock ();
	prtcp (src, dest, SIZE); // Do part copy
	int part_check = check (src, dest, SIZE);
	clock_t end = clock ();
	double part_time = (end - start) / (double) CLOCKS_PER_SEC;
	
	printf ("Making memory copy\n");
	for (size_t i = 0; i < SIZE; i ++) {
		src [i] = rand () % 128;
		dest [i] = rand () % 128;
	}
	
	start = clock ();
	memcp (src, dest, SIZE); // Do memory copy
	end = clock ();
	int mem_check = check (src, dest, SIZE);
	double mem_time = (end - start) / (double) CLOCKS_PER_SEC;
	
	printf ("Results for copying %d chars:\n", SIZE);
	printf ("* Function prtcp: %lf - ", part_time);
	if (part_check) {
		printf ("Copied correctly\n");
	} else {
		printf ("Copied not correctly\n");
	}
	
	printf ("* Function memcp: %lf - ", mem_time);
	if (mem_check) {
		printf ("Copied correctly\n");
	} else {
		printf ("Copied not correctly\n");
	}
	
	printf ("* Boost: %lf times\n", part_time / mem_time);
	
	free (dest);
	free (src);
	return 0;
}

int check (void *src1, void *src2, size_t size) {
	size_t offset = size & 15;
	
	for (size_t i = 0; i < size; i += 16) {
		// Loading blocks to XMM registers to compare
		__m128i reg1 = _mm_loadu_si128 ((__m128i *) src1);
		__m128i reg2 = _mm_loadu_si128 ((__m128i *) src2);
		
		// Comparing registers for equivalence
		__m128i res = _mm_cmpeq_epi8 (reg1, reg2);
		
		// Check that they are equal
		if (_mm_extract_epi16 (res, 7) == 0) { return 0; }
	}
	
	// Checking not 
	for (size_t i = size - offset; i < size; i ++) {
		const char *cp1 = (const char *) src1 + i;
		const char *cp2 = (const char *) src2 + i;
		if (*cp1 != *cp2) { return 0; }
	}
	
	return 1;
}

/* ===| IMPLEMENTATIONS |=== */

void *memcp (void *src, void *dest, size_t size) {
	// OPTIONAL: this ckeck for the size of data
	if (size <= 1024 * sizeof (__m128i) / 8) {
		// In case if amount of data is less than
		// 1024 XMM registers #prtcp () can be used
		return prtcp (src, dest, size);
	}
	
	// Aligning the destination pointer to multiplicity of 16
	size_t start = (size_t) dest;
	size_t offset = (16 - (start & 15)) & 15;
	prtcp (src, dest, offset);
	
	src += offset;           // move src pointer on offset value
	dest += offset;          // move dest pointer to aligned value
	size -= offset;          // rest of data
	offset = size & 15; // not aligned src
	
	// Copying data by 2 bytes untill the end of aligned block
	for (size_t from = (size_t) src; (size_t) src <= from + size; src += 16, dest += 16) {
		// Loading data to one XMM register 
		__m128i reg = _mm_loadu_si128 ((__m128i *) src);
		
		// Copying data from register to *dest
		_mm_stream_si128 ((__m128i *) dest, reg);
	}
	
	_mm_sfence ();
	
	// Copying rest of not aligned src data
	prtcp (src, dest, offset);
	
	return dest;
}

void *prtcp (void *src, void *dest, size_t size) {
	
	size_t offset = 0;
	while (size > 0) {
		// Getting char to be copied
		const char *cp = (const char *) src + offset;
		
		// Copying it to destination place
		*((char *) dest + (offset)) = *cp;
		
		offset ++;
		size --; // Char copied (decrease rest)
	}
	
	return dest;
}
