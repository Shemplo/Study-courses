package ru.shemplo.lru;

import java.util.HashMap;
import java.util.Map;

public class SimpleLRUCache <K, V> implements LRUCache <K, V> {

	protected final Map <K, Node> VALUES = new HashMap <> ();
	
	protected final int CAPACITY;
	
	protected Node head, tail;
	
	protected class Node {
		
		protected Node previous, next;
		
		public final V VALUE;
		public final K KEY;
		
		public Node (K key, V value) {
			this.VALUE = value;
			this.KEY = key;
		}
		
	}
	
	public SimpleLRUCache (int capacity) {
		if (capacity <= 0) {
			String text = "Capacity can't be non-positive";
			throw new IllegalArgumentException (text);
		}
		
		this.CAPACITY = capacity;
	}
	
	@Override
	public void put (K key, V value) {
		if (key == null) {
			String text = "Key can't have NULL value";
			throw new IllegalArgumentException (text);
		}
		
		if (VALUES.containsKey (key)) {
			String text = "Key already exists with value: " + VALUES.get (key).VALUE;
			throw new IllegalStateException (text);
		}
		
		if (VALUES.size () >= CAPACITY) {
			assert VALUES.remove (tail.KEY) != null;
			tail = tail.previous;
			
			assert VALUES.size () < CAPACITY;
		}
		
		Node newNode = new Node (key, value);
		assert VALUES.put (key, newNode) == null;
		moveToHead (newNode);
	}
	
	protected void moveToHead (Node node) {
		Node prev = node.previous, next = node.next;
		if (prev != null && next != null) {
			node.previous.next = next;
			node.next.previous = prev;
		} else if (prev != null && next == null) {
			assert tail == node;
			tail = tail.previous;
		} else if (prev == null && next == null) {
			if (head == null && tail == null) {
				head = tail = node;
				return;
			} else {
				assert head != null && tail != null;
			}
		} else {
			// node is already head
			assert head == node;
			return;
		}
		
		head.previous = node;
		node.next = head;
		head = node;
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
		return VALUES.size ();
	}
	
}
