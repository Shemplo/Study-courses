package ru.shemplo.lru;

import java.util.HashMap;
import java.util.Map;

public class SimpleLRUCache <K, V> implements LRUCache <K, V> {

	protected final Map <K, Node> VALUES = new HashMap <> ();
	
	protected int currentSize = 0;
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
		assert capacity > 0;
		if (capacity <= 0) {
			String text = "Capacity can't be non-positive";
			throw new IllegalArgumentException (text);
		}
		
		this.CAPACITY = capacity;
	}
	
	@Override
	public void put (K key, V value) {
		assert key != null;
		if (key == null) {
			String text = "Key can't have NULL value";
			throw new IllegalArgumentException (text);
		}
		
		if (currentSize >= CAPACITY) {
			int toRemove = currentSize + 1 - CAPACITY;
			removeLast (toRemove);
		}
		
		Node newNode = new Node (key, value);
		VALUES.put (key, newNode);
		moveToHead (newNode);
		
		currentSize += 1;
	}
	
	protected void moveToHead (Node node) {
		if (node.previous == null 
			&& node.next == null) { // new node
			
			if (head == null) {
				head = tail = node;
			} else {
				head.previous = node;
				node.next = head;
				head = node;
			}
			
			return;
		}
		
		// for some existing node:
		
		if (node.previous != null) {
			// [previous] <-> [node] <-> ???
			node.previous.next = node.next;
		}
		
		if (node.next != null) {
			// ??? <-> [node] <-> [next]
			node.next.previous = node.previous;
		}
		
		head = node;
	}
	
	protected void removeLast (int number) {
		while (number > 0 && tail != null) {
			VALUES.remove (tail.KEY);
			tail.next = null;
			number -= 1;
			
			tail = tail.previous;
			currentSize -= 1;
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
		return currentSize;
	}
	
}
