package main.java.stack;

import java.util.EmptyStackException;
import java.util.concurrent.atomic.AtomicReference;

public class StackImpl implements Stack {

	private AtomicReference <Node> head = new AtomicReference <> ();

	private class Node {
		public volatile Node next;
		public volatile int x;

		Node (int x) {
			// Top constructor
			this.x = x;
		}
	}

	@Override
	public void push (int x) {
		Node node = new Node (x);
		
		do {
			// Just set link to next node
			node.next = head.get ();
		} while (!head.compareAndSet (node.next, node));
	}

	@Override
	public int pop () {
		Node current = head.get ();
		while (current != null) {
			if (head.compareAndSet (current, current.next)) {
				// Return value from head node if not null
				return current.x;
			}
			
			current = head.get ();
		}
		
		// Stack is empty :(
		throw new EmptyStackException ();
	}
}
