package lock_free_hashtable;

import java.util.concurrent.atomic.AtomicIntegerArray;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Int-to-Int hash map with open addressing and linear probes.
 *
 */
public class IntIntHashMap {

	private static final int MAGIC = 0x9E3779B9; // golden ratio
	private static final int INITIAL_CAPACITY = 2; // !!! DO NOT CHANGE INITIAL CAPACITY !!!
	private static final int MAX_PROBES = 8; // max number of probes to find an item

	private static final int NULL_KEY = 0; // missing key (initial value)
	private static final int NULL_VALUE = 0; // missing value (initial value)
	private static final int DEL_VALUE = Integer.MAX_VALUE; // mark for removed value
	private static final int NEEDS_REHASH = -1; // returned by putInternal to indicate that rehash is needed
	private static final int MOVED_VALUE = -2; // This value was moved to new core already

	// Checks is the value is in the range of allowed values
	private static boolean isValue (int value) {
		return value > 0 && value < DEL_VALUE; // the range or allowed values
	}

	// Converts internal value to the public results of the methods
	private static int toValue (int value) {
		return isValue (value) ? value : 0;
	}

	private AtomicReference <Core> core = new AtomicReference <> (new Core (INITIAL_CAPACITY));
	
	/**
	 * Returns value for the corresponding key or zero if this key is not present.
	 *
	 * @param key
	 *            a positive key.
	 * @return value for the corresponding or zero if this key is not present.
	 * @throws IllegalArgumentException
	 *             if key is not positive.
	 */
	public int get (int key) {
		if (key <= 0)
			throw new IllegalArgumentException ("Key must be positive: " + key);
		return toValue (core.get ().getInternal (key));
		//return toValue (core.getInternal (key));
	}

	/**
	 * Changes value for the corresponding key and returns old value or zero if key
	 * was not present.
	 *
	 * @param key
	 *            a positive key.
	 * @param value
	 *            a positive value.
	 * @return old value or zero if this key was not present.
	 * @throws IllegalArgumentException
	 *             if key or value are not positive, or value is equal to
	 *             {@link Integer#MAX_VALUE} which is reserved.
	 */
	public int put (int key, int value) {
		if (key <= 0)
			throw new IllegalArgumentException ("Key must be positive: " + key);
		if (!isValue (value))
			throw new IllegalArgumentException ("Invalid value: " + value);
		return toValue (putAndRehashWhileNeeded (key, value));
	}

	/**
	 * Removes value for the corresponding key and returns old value or zero if key
	 * was not present.
	 *
	 * @param key
	 *            a positive key.
	 * @return old value or zero if this key was not present.
	 * @throws IllegalArgumentException
	 *             if key is not positive.
	 */
	public int remove (int key) {
		if (key <= 0)
			throw new IllegalArgumentException ("Key must be positive: " + key);
		return toValue (putAndRehashWhileNeeded (key, DEL_VALUE));
	}

	private int putAndRehashWhileNeeded (int key, int value) {
		while (true) {
			int oldValue = core.get ().putInternal (key, value);
			if (oldValue != NEEDS_REHASH) {
				return oldValue;
			}
			
			Core current = core.get (), 
					rehashed = core.get ().rehash ();
			// Not important to check 
			//  -> just one thread will do this
			core.compareAndSet (current, rehashed);
		}
	}

	private static class Core {
		
		final AtomicIntegerArray map;
		AtomicReference <Core> next;
		final int shift;

		/**
		 * Creates new core with a given capacity for (key, value) pair. The actual size
		 * of the map is twice as big.
		 */
		Core (int capacity) {
			this.map = new AtomicIntegerArray (2 * capacity);
			this.next = new AtomicReference <> (null);
			
			int mask = capacity - 1;
			assert mask > 0 && (mask & capacity) == 0 : "Capacity must be power of 2: " + capacity;
			shift = 32 - Integer.bitCount (mask);
		}

		int getInternal (int key) {
			int index = index (key), tries = 0;
			while (map.get (index) != key) {
				if (map.get (index) == NULL_KEY) {
					return NULL_VALUE;
				}
				
				if (++tries >= MAX_PROBES) { return NULL_VALUE; }
				if (index == 0) { index = map.length (); }
				
				index -= 2;
			}
			
			int cell = map.get (index + 1);
			if (cell < 0) {
				delegate (index);
				return next.get ().getInternal (key);
			}
			
			return cell;
		}

		int putInternal (int key, int value) {
			int index = index (key), tries = 0;
			while (map.get (index) != key) {
				if (map.get (index) == NULL_KEY) {
					if (value == DEL_VALUE) {
						return NULL_VALUE;
					}
					
					if (map.compareAndSet (index, NULL_VALUE, key)) {
						break;
					}
				}
				
				if (map.get (index) == key) { break; };
				if (++tries >= MAX_PROBES) { return NEEDS_REHASH; }
				if (index == 0) { index = map.length (); }
				
				index -= 2;
			}
			
			int cell;
			
			do {
				cell = map.get (index + 1);
				if (cell < 0) {
					delegate (index);
					return next.get ().putInternal (key, value);
				}
			} while (!map.compareAndSet (index + 1, cell, value));
			
			return cell;
		}
		
		private void delegate (int index) {
			int v = map.get (index + 1);
			if (v != MOVED_VALUE) {
				int nKey = map.get (index);
				int stub = Integer.MAX_VALUE;
				next.get ().reserveEmpty (nKey, v & stub);
			}
			
			map.set (index + 1, MOVED_VALUE);
		}
		
		private int reserveEmpty (int key, int value) {
			int index = index (key), tries = 0;
			while (map.get (index) != key) {
				if (map.compareAndSet (index, NULL_KEY, key)) {
					break;
				}
				
				if (map.get (index) == key) { break; }
				if (++tries > MAX_PROBES) { return NEEDS_REHASH; }
				if (index == 0) { index = map.length (); }
				
				index -= 2;
			}
			
			map.compareAndSet (index + 1, NULL_VALUE, value);
			return NULL_VALUE; // B/c here was NULL_VALUE
		}

		Core rehash () {
			next.compareAndSet (null, new Core (map.length ()));
			for (int i = 0, value; i < map.length (); i += 2) {
				if ((value = map.get (i + 1)) == MOVED_VALUE) {
					continue;
				}
				
				while (true) {
					if (value == DEL_VALUE) {
						if (map.compareAndSet (i + 1, value, MOVED_VALUE)) {
							break;
						}
					}
					
					if (value < 0) { break; }
					if (value != DEL_VALUE) {
						int stub = Integer.MIN_VALUE;
						if (map.compareAndSet (i + 1, value, value | stub)) {
							break;
						}
					}
					
					value = map.get (i + 1);
				}
				
				delegate (i);
			}
			
			return next.get ();
		}

		/**
		 * Returns an initial index in map to look for a given key.
		 */
		int index (int key) {
			return ((key * MAGIC) >>> shift) * 2;
		}

	}
}
