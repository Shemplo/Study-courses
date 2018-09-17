#ifndef MEM_MGR_H
#define MEM_MGR_H

namespace mem_mgr {
	
	/**
	 * Allocate memory for fucntion
	 */
	void *alloc ();
	
	/**
	 * Clear space of function
	 */
	void remove (void *);
}

#endif // MEM_MGR_H