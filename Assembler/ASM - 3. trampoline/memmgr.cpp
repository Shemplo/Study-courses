#include "memmgr.h"

#include <stdlib.h>
#include <sys/mman.h>

namespace {
	
	const int SIZE = (1 << 7);
	void **ptr = nullptr;
	
	void alloc_mem () {
		void *mem = mmap (nullptr, 4096,
							PROT_EXEC | PROT_READ | PROT_WRITE,
							MAP_PRIVATE | MAP_ANONYMOUS,
							-1, 0);
		ptr = (void **) mem;
		if (mem != nullptr) {
			for (int i = 0; i < 4096; i += SIZE) {
				char *tmp = ((char *) mem) + i;
				*(void **) tmp = 0;
				
				if (i) { *(void **) (tmp - SIZE) = tmp; }
			}
		}
	}
	
}

void *mem_mgr::alloc () {
	if (ptr == nullptr) {
		alloc_mem ();
		
		if (ptr == nullptr) {
			return nullptr;
		}
	}
	
	void *addr = ptr;
	ptr = (void **) *ptr;
	return addr;
}

void mem_mgr::remove (void *del) {
	*(void **) del = ptr;
	ptr = (void **) del;
}