package ru.shemplo.lru;

public interface LRUCache <K, V> {

	/**
	 * ...
	 * 
	 * @param key
	 * @param value
	 * 
	 */
	public void put (K key, V value);
	
	/**
	 * ...
	 * 
	 * @param key
	 * @return
	 * 
	 */
	public V get (K key);
	
	/**
	 * ...
	 * 
	 * @return maximum number of entries in cache line
	 * 
	 */
	public int getCapacity ();
	
	/**
	 * ...
	 * 
	 * @return
	 * 
	 */
	public int getSize ();
	
}
