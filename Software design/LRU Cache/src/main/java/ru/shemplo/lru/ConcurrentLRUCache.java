package ru.shemplo.lru;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;

public class ConcurrentLRUCache <K, V> implements LRUCache <K, V> {
	
	protected final ConcurrentMap <K, Node> 
		VALUES = new ConcurrentHashMap <> ();
	
	protected final AtomicReference <Node>
		HEAD = new AtomicReference <> (new Node (null, null)),
		TAIL = new AtomicReference <> (new Node (null, null)),
		CASN = new AtomicReference <> (null);
	
	protected final AtomicInteger 
		SIZE = new AtomicInteger (0);
	
	protected final AtomicLong TIME = new AtomicLong (0L);
	
	protected final int CAPACITY;
	
	protected class Node {
		
		protected final AtomicReference <Node> 
			previous = new AtomicReference <> (null), 
			next     = new AtomicReference <> (null);
		
		public final AtomicBoolean FLAG = new AtomicBoolean (false);
		public long time = 0;
		
		protected volatile boolean isLocked = false;
		
		public final V VALUE;
		public final K KEY;
		
		public Node (K key, V value) {
			this.VALUE = value;
			this.KEY = key;
		}
		
		public boolean isLocked () {
			return isLocked;
		}
		
	}
	
	public ConcurrentLRUCache (int capacity) {
		assert capacity > 0;
		if (capacity <= 0) {
			String text = "Capacity can't be non-positive";
			throw new IllegalArgumentException (text);
		}
		
		this.CAPACITY = capacity;
		
		TAIL.get ().previous.set (HEAD.get ());
		HEAD.get ().next.set (TAIL.get ());		
	}
	
	public String toString () {
		StringBuilder sb = new StringBuilder (HEAD.get ().KEY 
							+ " " + TAIL.get ().KEY + " {\n");
		String nodeFormat = "%4d <- [%4d - %-4s] -> %-4d\n";
		
		Node current = HEAD.get ();
		int lines = 0;
		while (current != null) {
			Node prev = current.previous.get (),
				 next = current.next.get ();
			sb.append (String.format (nodeFormat, prev != null ? prev.KEY : -1, 
					current.KEY, current.VALUE, next != null ? next.KEY : -1));
			
			current = next;
			lines += 1;
		}
		
		sb.append ("} ");
		sb.append (lines);
		sb.append (" / ");
		sb.append (SIZE.get ());
		return sb.toString ();
	}
	
	@Override
	public void put (K key, V value) {
		if (key == null) {
			String text = "Key can't have NULL value";
			throw new IllegalArgumentException (text);
		}
		
		Node newNode = new Node (key, value);
		//VALUES.put (key, newNode);
		moveToHead (newNode);
	}
	
	protected void moveToHead (Node node) {
		if (node == null) { return; } 
		
		while (true) {
			Node prev = node.previous.get (), 
				 next = node.next.get ();
			if (node.FLAG.compareAndSet (false, true)) {
				if (prev != null) {
					if (!prev.FLAG.compareAndSet (false, true)) {
						node.FLAG.set (false);
					}
				} else if (next != null) {
					// This node is already head
					node.FLAG.set (false);
					break;
				} else {
					// This is new node in cache
					synchronized (HEAD) {
						VALUES.putIfAbsent (node.KEY, node);
						SIZE.incrementAndGet ();
						
						HEAD.get ().previous.set (node);
						node.next.set (HEAD.get ());
						HEAD.set (node);
						
						node.time = TIME.getAndIncrement ();
						
						node.FLAG.set (false);
						break;
					}
				}
				
				if (next != null) {
					if (!next.FLAG.compareAndSet (false, true)) {
						if (prev != null) { prev.FLAG.set (false); }
						node.FLAG.set (false);
					}
					
					if (prev != null) {
						// PREV --[NODE]--> NEXT
						prev.next.set (next);
					}
					
					// PREV <--[NODE]-- NEXT
					next.previous.set (prev);
				} else if (prev != null) {
					// This node is tail
					synchronized (TAIL) {
						TAIL.set (prev);
					}
				}
				
				synchronized (HEAD) {
					HEAD.get ().previous.set (node);
					node.next.set (HEAD.get ());
					HEAD.set (node);
					
					node.time = TIME.getAndIncrement ();
					
					if (prev != null) { prev.FLAG.set (false); }
					if (next != null) { next.FLAG.set (false); }
					node.FLAG.set (false);
					break;
				}
			}
		}
	}
	
	@Override
	public V get (K key) {
		Node node = VALUES.get (key);
		if (node == null || (node.previous.get () == null 
							 && node.next.get () == null)) {
			return null;
		} else if (TIME.get () - node.time > CAPACITY ) {
			while (!node.FLAG.compareAndSet (false, true)) {}
			synchronized (TAIL) {
				TAIL.set (node.previous.get ());
				assert TAIL.get () != null;
				SIZE.decrementAndGet ();
			}
			
			return null;
		}
		
		moveToHead (node);
		return node.VALUE;
	}

	@Override
	public int getCapacity () {
		return CAPACITY;
	}

	@Override
	public int getSize () {
		return Math.min (CAPACITY, SIZE.get ());
	}
	
}
