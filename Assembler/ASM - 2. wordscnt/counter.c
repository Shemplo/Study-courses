// gcc -std=c11 -mssse3 -Werror -Wall -o counter counter.c

#include <stdio.h>
#include <emmintrin.h>
#include <tmmintrin.h>
#include <time.h>
#include <limits.h>

#define LENGTH (1 << 28)

/**
 * (Count by byte)
 *
 * Counts words in string.
 *
 * This function counts words in given
 * string by bytes. The criteria of word
 * is that not space chars separeted with ' '
 * 
 * @param string pointer to the start of string
 * @param length length of string
 * @return number of words in string
 * 
 */
size_t cntbb (char *string, size_t length);

/**
 * (Count by register)
 *
 * Counts words in string.
 *
 * This function counts words in given
 * string by XMM registers. The criteria of word
 * is that not space chars separeted with ' '
 * 
 * @param string pointer to the start of string
 * @param length length of string
 * @return number of words in string
 *
 */
size_t cntbr (char *string, size_t length);

/* ===| ENTER POINT |=== */

int main () {
	srand (time (0));
	char *string = malloc (sizeof (char) * LENGTH);
	
	for (size_t i = 0; i < LENGTH; i ++) {
		string [i] = 'a' + (rand () & 25);
	}
	for (size_t i = 0; i < LENGTH; i ++) {
		if ((rand () & 3) == 0) { string [i] = ' '; }
	}
	
	// NOTICE: It can be uncommented in small lengthes
	//printf ("String of %d chars generated\n", LENGTH);
	//printf ("String: %s\n", string);
	
	clock_t start = clock ();
	size_t bb_words = cntbb (string, LENGTH);
	clock_t end = clock ();
	double byte_time = (end - start) / (double) CLOCKS_PER_SEC;
	printf ("Words: %ld (time: %lf)\n", bb_words, byte_time);
	
	start = clock ();
	size_t br_words = cntbr (string, LENGTH);
	end = clock ();
	double reg_time = (end - start) / (double) CLOCKS_PER_SEC;
	printf ("Words: %ld (time: %lf)\n", br_words, reg_time);
	
	// Final check for not comparing numbers by "hands"
	if (bb_words == br_words) {
		printf ("Test OK: nubers are equal\n");
	} else {
		printf ("Test FAIL: numbers are not equal\n");
	}
	
	free (string);
	return 0;
}

/* ===| IMPLEMENTATIONS |=== */

size_t cntbb (char *string, size_t length) {
	size_t words = 0;
	int was_space = 1;

	// Running by the string as array
	for (size_t i = 0; i < length; i ++) {
		char c = *(string + i); // Getting char from string
		if (c != ' ' && was_space == 1) {
			words ++;
		}
		
		was_space = (c == ' ');
	}
	
	return words;
}

size_t cntbr (char *string, size_t length) {
	size_t offset = 0, words = 0;
    int was_space = 0;

	// Aligning src pointer to multiplicity of 16
    while ((size_t) (string + offset) % 16 != 0) {
        char c = *(string + offset);
        if (was_space && c != ' ') {
            words++;
        }

        was_space = (c == ' ');
        offset ++;
    }
    

	// Check if it was space char at the begining
	if (*string != ' ') { words ++; }
	
	// Check if next is not a space
	if (was_space 
			&& *(string + offset) != ' ' 
			&& offset != 0) {
		words ++;
	}

	// Getting the end of aligned by 16 addresses space
	size_t rest = length - ((length - offset) & 15) - 16;
	
	// 128-bytes xmm registers constants
	__m128i spaces_mask = _mm_set1_epi8 (32);
	__m128i accumulator = _mm_set1_epi8 (0);
	
	__m128i cmp_result; // String compare mask in previous step
	
	__m128i *start_block = (__m128i *) (string + offset); // Compare mask in current step 
	__m128i cmp_spaces = _mm_cmpeq_epi8 (_mm_load_si128 (start_block), spaces_mask);
    
    
    for (size_t i = offset; i < rest; i += 16){
		// Getting next block of 16 bytes from given string
        __m128i currentStroke = _mm_load_si128 ((__m128i *) (string + i + 16));
        
        cmp_result = cmp_spaces;
        cmp_spaces = _mm_cmpeq_epi8 (currentStroke, spaces_mask); // Compare with a space mask
		
        __m128i shifted_block = _mm_alignr_epi8 (cmp_spaces, cmp_result, 1);   // Move register to one byte
        __m128i trimmed_block = _mm_andnot_si128 (shifted_block, cmp_result);  // Make AND-NOT on shifted block
		
		// Get the mask of result as comparence with 0 and then storage it in accumulator register
        accumulator = _mm_adds_epu8 (_mm_subs_epi8 (_mm_set1_epi8 (0), trimmed_block), accumulator);

		// Check for ovewflow of accumulator
		// Or the end of processing string
		int high = 0, low = 0; __m128i tmp;
        if(_mm_movemask_epi8 (accumulator) != 0 || i + 16 >= rest) {
			// Splitting 16-bytes register on 2 8-bytes registers
			// (To get then minor and major parts)
            tmp = _mm_sad_epu8 (_mm_set1_epi8 (0), accumulator);
            low = _mm_cvtsi128_si32(tmp); // Getting minor part

			// Moving major part to minor area (like a AH -> AL)
            tmp = (__m128i) _mm_movehl_ps (_mm_castsi128_ps (tmp), _mm_castsi128_ps (tmp));
            high = _mm_cvtsi128_si32 (tmp); // Getting major part

            words += high + low; // Storaging results
            accumulator = _mm_set1_epi8 (0);
        }

    }
    
    offset = rest; // Moving to the end of aligned block
    if (*(string + offset - 1) == ' ' 
			&& *(string + offset) != ' '){
        words --; // Check if previous char was space but current is a new word
		          // If not to do this, last word will be counted twice
    }

	// Counting words in not aligned rest of string
    was_space = *(string + offset - 1) == ' ';
    for (size_t i = offset; i < length; i++){
		char c = *(string + i);
        if (was_space && c != ' '){
            words ++;
        }
		
        was_space = (c == ' ');
    }
	
	return words;
}