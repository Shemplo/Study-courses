#ifndef TRAMPOLINE_H
#define TRAMPOLINE_H

#include "types.h"
#include "memmgr.h"

#include <iostream>
#include <memory>
#include <unistd.h>
#include <string>
#include <array>

//const std::string je_imm  = "\x74";
const std::array <char, 1> je_imm = {'\x74'};
//const std::string jmp_imm = "\xEB";
const std::array <char, 1> jmp_imm = {'\xEB'};
//const std::string ret     = "\xC3";
const std::array <char, 1> ret = {'\xC3'};

//const std::string push_r9     = "\x41\x51";
const std::array <char, 2> push_r9     = {'\x41', '\x51'};
//const std::string jmp_rax     = "\xFF\xE0";
const std::array <char, 2> jmp_rax     = {'\xFF', '\xE0'};
//const std::string mov_rdi_imm = "\x48\xBF";
const std::array <char, 2> mov_rdi_imm = {'\x48', '\xBF'};
//const std::string mov_rax_imm = "\x48\xB8";
const std::array <char, 2> mov_rax_imm = {'\x48', '\xB8'};
//const std::string add_rax_imm = "\x48\x05";
const std::array <char, 2> add_rax_imm = {'\x48', '\x05'};
//const std::string call_rax    = "\xFF\xD0";
const std::array <char, 2> call_rax   = {'\xFF', '\xD0'};
//const std::string pop_r9      = "\x41\x59";
const std::array <char, 2> pop_r9     = {'\x41', '\x59'};

//const std::string add_rsp_imm = "\x48\x81\xC4";
const std::array <char, 3> add_rsp_imm = {'\x48', '\x81', '\xC4'};
//const std::string cmp_rax_rsp = "\x48\x39\xE0";
const std::array <char, 3> cmp_rax_rsp = {'\x48', '\x39', '\xE0'};
//const std::string mov_rax_rsp = "\x48\x89\xE0";
const std::array <char, 3> mov_rax_rsp = {'\x48', '\x89', '\xE0'};
//const std::string sub_rsp_imm = "\x48\x81\xEC";
const std::array <char, 3> sub_rsp_imm = {'\x48', '\x81', '\xEC'};

//const std::string mov_val_r11_rsp     = "\x4C\x8B\x1C\x24"; // r11 <- [rsp]
const std::array <char, 4> mov_val_r11_rsp     = {'\x4C', '\x8B', '\x1C', '\x24'};
//const std::string mov_val_rdi_rsp     = "\x48\x8B\x3C\x24"; // rdi <- [rsp]
const std::array <char, 4> mov_val_rdi_rsp     = {'\x48', '\x8B', '\x3C', '\x24'};
//const std::string mov_val_rsp_r11     = "\x4C\x89\x1C\x24"; // [rsp] <- r11
const std::array <char, 4> mov_val_rsp_r11     = {'\x4C', '\x89', '\x1C', '\x24'};
//const std::string mov_val_r11_rsp_imm = "\x4C\x8B\x9C\x24"; // [r11] <- [rsp+imm]
const std::array <char, 4> mov_val_r11_rsp_imm = {'\x4C', '\x8B', '\x9C', '\x24'};

//const std::string mov_val_rsp_rdi = "\x48\x89\x7C\x24\xF8"; // [rsp-0x8] <- rdi
const std::array <char, 5> mov_val_rsp_rdi = {'\x48', '\x89', '\x7C', '\x24', '\xF8'};

/**
 * Trampoline.
 * 
 * (And everythig is said by this)
 * 
 */
template <typename Return>
struct Trampoline;

template <typename Return>
struct Trampoline {
	template <typename F>
	Trampoline (F func) {}
	
	Return *get () const;
};

template <typename Return, typename ... Args>
struct Trampoline <Return (Args ...)> {
	
	/**
	 * Public constructor of Trampoline.
	 * 
	 * Creates the Trampoline for given function
	 * 
	 * @param function to execute
	 * 
	 * @note It works!
	 * 
	 */
	template <typename F>
	Trampoline (F func) : func_obj (new F (std::move (func))), deleter (my_deleter <F>) {
		code = mem_mgr::alloc ();
		ptr = (char *) code;
		
		/*
		
		Base trampoline:
		
		trampoline:
		mov rdi -> rsi - shift argument by 1
		mov imm -> rdi - put pointer to object
		mov imm -> rax - put pointer to call instruction
		jmp rax        - do call
		
		do_call:
		push rbp
		mov rsp -> rbp
		sub 0x10 from rsp
		mov rdi -> [rbp - 0x8]
		mov esi -> [rbp - 0xc]
		mov [rbp - 0xc] -> edx
		mov [rbp - 0x8] -> rax
		mov edi -> esi
		mov rax -> rdi
		call ptr
		ret
		
		*/
		
		if (types <Args ...>::reg < 6) {
			// Default situation when the int arguments less than 6
			// Then all this arguments can be placed in registers
			// (According to calling conventions)

			/*
				1. Shift arguments by 1 position
				2. Put funtion object to RDI as first argument
				3. Put pointer to caller in RAX
				4. Call for the function
			*/
			
			mov_regs (types <Args ...>::reg - 1, 0);
			add8 (mov_rdi_imm, func_obj);
			add8 (mov_rax_imm, (void *) &do_call <F>);
			add1 (jmp_rax);
		} else {
			/*
				1. Store last argument
				2. Move in cycle integer arguments
				3. Set up return address in right position
				4. Move in cycle arguments in stack
				5. Set up last argument from prev. regs
				6. Prepare stack to call and call function object
				7. Make up stack due to calling-convs.
			*/
			
			// Saving return address from top of stack
			add1 (mov_val_r11_rsp);
			// Move all int arguments by 1 (last on stack)
			mov_regs (5, 0);
			// Store current top of stack to rax
			add1 (mov_rax_rsp);

			// Count the required size of stack
			int stack_size = 8 * (types <Args ...>::reg - 6 
									+ std::max (types <Args ...>::sse - 8, 0));
			// Put rax as last argument in stack
			add4 (add_rax_imm, stack_size + 8);
			// Put rsp as return address
			add4 (add_rsp_imm, 8);

			// Making loop to shift arguments in stack

			// Creating a label for loop
			char *label = (char *) get_ptr ();
			// Checking if arguments're copied
			add1 (cmp_rax_rsp);
			// JumpEqual condition after comparation
			add1 (je_imm); // Go to outside of cycle
			// 1 byte for label address
			char *label2 = (char *) reserve (1);
			// Get value of next argument
			add4 (add_rsp_imm, 8); // Move stack pointer
			add1 (mov_val_rdi_rsp); // Copy value to rdi
			add1 (mov_val_rsp_rdi); // Put from rdi to rsp-0x8
			
			// Storing relative addresses of labels
			add1 (jmp_imm); // Jump to first label   
			// 1 byte for label address
			char *label3 = (char *) reserve (1);
			// Store in reserved space offset from label
			*label3 = (char) (label - (char *) get_ptr ());
			// Store in reserved place offset from label 2
			*label2 = (char) ((char *) get_ptr () - label2 - 1);

			// Loop ended

			// Set up saved return address to stack
			add1 (mov_val_rsp_r11);
			// Transfer rsp to top of stack
			add4 (sub_rsp_imm, stack_size + 8);
			// Put function object to rdi
			add8 (mov_rdi_imm, func_obj);
			// Put call address to rax
			add8 (mov_rax_imm, (void *) &do_call<F>);

			// Finish line
			
			// Calling function
			add1 (call_rax);
			// Turning stack to pre-call condition
			// Removing 6th argument from stack
			add1 (pop_r9);
			// Normalize stack size
			add4 (mov_val_r11_rsp_imm, stack_size);
			// Restore rsp value
			add1 (mov_val_rsp_r11);
			// Return - end function call
			add1 (ret);
		}
	}
	
	/**
	 * Call for a function.
	 * 
	 * This function will call for another
	 * that is passed throw the arguments with
	 * set context given as second argument
	 * 
	 * @param obj function to call
	 * @param args array of arguments for call
	 * @return the result of given fucntion with given context
	 * 
	 * @note strange that it's not a private
	 * 
	 */
	template <typename F>
	static Return do_call (void *obj, Args ... args) {
		return (*(F *) obj) (args ...);
	}
	
	/**
	 * Get the pointer to function
	 */
	Return (*get () const) (Args ... args) {
		return (Return (*)(Args ... args)) code;
	}
	
	~Trampoline () {
		if (func_obj) { deleter (func_obj); }
		mem_mgr::remove (code);
	}
	
	private:
		template <typename F>
		static void my_deleter (void *func_obj) {
			delete static_cast <F *> (func_obj);
		}
		
		// const char [] mov_rdi_imm = {'\x48', '\xBF'};

		// const std::array <char, 2> mov_rdi_imm = {'\x48', '\xBF'};

		// Adding instructions by 1 byte
		template <size_t N>
		void add1 (const std::array <char, N> &oper) {
			for (size_t i = 0; i < oper.size (); ++ i) {
				*(ptr ++) = oper [i];
			}
		}
		
		// Adding instructions by 1 byte and also 4 bytes of data
		template <size_t N>
		void add4 (const std::array <char, N> &oper, int32_t data) {
			add1 (oper);
			
			*(int32_t *) ptr = data;
			ptr += 4;
		}
		
		// Adding instructions by 1 byte and also 8 bytes of data
		template <size_t N>
		void add8 (const std::array <char, N> &oper, void *data) {
			add1 (oper);
			
			*(void **) ptr = data;
			ptr += 8;
		}

		// Skip space for a num value
		void *reserve (size_t num) {
			void *start = ptr;
			ptr += num;
			return start;
		}

		// Return current pointer position
		void *get_ptr () {
			return ptr;
		}
		
		// Moving default arguments registers
		void mov_regs (int from, int to) {
			static const char *shcemes [] = {
				"\x48\x89\xFE", // rdi -> rsi
				"\x48\x89\xF2", // rsi -> rdx
				"\x48\x89\xD1", // rdx -> rcx
				"\x49\x89\xC8", // rcx -> r8
				"\x4D\x89\xC1", // r8 -> r9
				"\x41\x51"      // push r9
			};
			
			for (int i = from; i >= to; i --) {
				// WARNING: This is a weak place for C strings
				for (const char *j = shcemes [i]; *j; j ++) {
					* (ptr ++) = *j;
				}
			}
		}
		
		void *func_obj;
		void *code;
		char *ptr;
		
		void (*deleter) (void *);
};

#endif // TRAMPOLINE_H