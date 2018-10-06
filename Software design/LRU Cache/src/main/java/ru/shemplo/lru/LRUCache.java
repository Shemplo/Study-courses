package ru.shemplo.lru;

public interface LRUCache <K, V> {

	/**
	 * Add new key-value pair in a {@link LRUCache}.
	 * If number of such pairs is more than {@link #getCapacity() capacity}
	 * of cache then the oldest pairs will be removed from cache.
	 * 
	 * This operation takes O(1) computation time.
	 * 
	 * <pre>
	 * Contract:
	 * 
	 *  put (key, value)
	 *  
	 *  pre:
	 *   1. key != null
	 *   2. size <= capacity
	 *   3. !cache.containsKey(key)
	 *  
	 *  post:
	 *   1. cache'[0] == &lt;key, value&gt;
	 *   2. size' <= capacity
	 * </pre>
	 * 
	 * @param key some value that is used for getting access
	 * @param value something associated with key
	 * 
	 * @throws IllegalArgumentException if key is NULL value
	 * or if pair with such key already exists in cache
	 * 
	 * @see #getCapacity() getCapacity
	 * 
	 */
	public void put (K key, V value);
	
	/**
	 * Returns value from cache associated with given key if it exists.
	 * This method can return <b>null</b> if no pairs with such key
	 * were added to cache or if pair was removed from cache 
	 * (see {@link #put(Object, Object) put}).
	 * 
	 * This operation takes O(1) computation time.
	 * 
	 * <pre>
	 * Contract:
	 * 
	 *  value get (key)
	 *  
	 *  pre:
	 *   1. key != null
	 *   2. size <= capacity
	 *  
	 *  post:
	 *   1. value = null | &lt;key, value&gt;.second
	 *   2. (?value != null) cache'[0] = &lt;key, value&gt;
	 *   3. size' = size
	 * </pre>
	 * 
	 * @param key some value for getting access to value
	 * 
	 * @return value associated with this key if it exists; 
	 * null otherwise
	 * 
	 * @see #put(Object, Object) put
	 * 
	 */
	public V get (K key);
	
	/**
	 * Returns constant value that refers to maximum number
	 * of elements in cache line before some pair will be
	 * removed from it in {@link #put(Object, Object) put} method.
	 * 
	 * This operation takes O(1) computation time.
	 * 
	 * @return maximum number of entries in cache line
	 * 
	 */
	public int getCapacity ();
	
	/**
	 * Returns current number of elements in cache line.
	 * This value can only growth or stay the same. If
	 * cache {@link #getCapacity() capacity} limit was reached 
	 * then this value won't change in future at all.
	 * Until limit reached value will grow after each 
	 * {@link #put(Object, Object) put} method call.
	 * 
	 * This operation takes O(1) computation time.
	 * 
	 * @return number of elements in cache line
	 * 
	 * @see #getCapacity() getCapacity
	 * 
	 */
	public int getSize ();
	
}
