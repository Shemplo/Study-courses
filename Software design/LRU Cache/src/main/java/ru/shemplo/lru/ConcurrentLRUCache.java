package ru.shemplo.lru;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

public class ConcurrentLRUCache <K, V> implements LRUCache <K, V> {
	
	protected final ConcurrentMap <K, Node> 
		VALUES = new ConcurrentHashMap <> ();
	
	private final Node DUMMY = new Node (null, null);
	
	protected final AtomicReference <Node>
		HEAD = new AtomicReference <> (DUMMY),
		TAIL = new AtomicReference <> (DUMMY),
		CASN = new AtomicReference <> (null);
	
	protected final AtomicInteger 
		SIZE   = new AtomicInteger (0);
	
	protected final int CAPACITY;
	
	protected class Node {
		
		protected final AtomicReference <Node> 
			previous = new AtomicReference <> (null), 
			next     = new AtomicReference <> (null);
		
		public final V VALUE;
		public final K KEY;
		
		public Node (K key, V value) {
			this.VALUE = value;
			this.KEY = key;
		}
		
	}
	
	public ConcurrentLRUCache (int capacity) {
		assert capacity > 0;
		if (capacity <= 0) {
			String text = "Capacity can't be non-positive";
			throw new IllegalArgumentException (text);
		}
		
		this.CAPACITY = capacity;
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
		assert key != null;
		if (key == null) {
			String text = "Key can't have NULL value";
			throw new IllegalArgumentException (text);
		}
		
		Node newNode = new Node (key, value);
		moveToHead (newNode);
	}
	
	protected void moveToHead (Node node) {
		if (node == null) { return; } 
		int attempt = 0;
		
		while (true) {
			attempt += 1;
			
			// Each thread tries to add it's own node to processing
			boolean voted = CASN.compareAndSet (null, node);
			if (!voted && attempt > 1) { continue; }
			
			Node currentNode = CASN.get ();
			if (currentNode == null) {
				continue;
			}
			
			Node prev = currentNode.previous.get (),
				 next = currentNode.next.get ();
			
			if (prev != null) {
				prev.next.compareAndSet (currentNode, next);
			}
			
			if (next != null) {
				next.previous.compareAndSet (currentNode, prev);
			}
			
			HEAD.get ().previous.compareAndSet (null, currentNode);
			
			if (voted) {
				// Here can be just one thread
				VALUES.putIfAbsent (currentNode.KEY, currentNode);
				if (SIZE.get () >= CAPACITY) {
					Node tail = TAIL.get ();
					if (tail.KEY != null && tail.KEY != currentNode.KEY) {
						VALUES.remove (tail.KEY, tail);
					}
					
					TAIL.set (tail.previous.get ());
					if (TAIL.get () == null) {
						System.out.println (tail.KEY);
					}
					
					assert TAIL.get () != null;
					TAIL.get ().next.set (null);
				} else {
					SIZE.incrementAndGet ();
				}
				
				currentNode.next.set (HEAD.get ());
				currentNode.previous.set (null);
				HEAD.set (currentNode);
				
				CASN.set (null);
				break;
			} 
		}
	}
	
	@Override
	public V get (K key) {
		Node node = VALUES.get (key);
		if (node == null) {
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
		return SIZE.get ();
	}
	
}
