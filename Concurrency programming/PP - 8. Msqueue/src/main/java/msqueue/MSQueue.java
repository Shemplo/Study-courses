package msqueue;

import java.util.concurrent.atomic.AtomicReference;

public class MSQueue implements Queue {

	private class Node {

		final int x;
		public AtomicReference <Node> N;

		Node (int x, Node next) {
			this.N = new AtomicReference <> (next);
			this.x = x;
		}
		
	}
	
	private AtomicReference <Node> head, tail;

	public MSQueue () {
		Node dummy = new Node (0, null);
		head = new AtomicReference <> (dummy);
		tail = new AtomicReference <> (dummy);
	}

	@Override
	public void enqueue (int x) {
		Node nTail = new Node (x, null);
		while (true) {
			Node cTail = tail.get ();
			
			if (tail.get ().N.compareAndSet (null, nTail)) {
				tail.compareAndSet (cTail, nTail);
				return;
			} else {
				tail.compareAndSet (cTail, cTail.N.get ());
			}
		}
	}

	@Override
	public int dequeue () {
		Node cHead, cTail, next;
		int res = 0;
		
		while (true) {
			cHead = head.get ();
			cTail = tail.get ();
			next = cHead.N.get ();
			
			if (cHead == cTail) {
				if (next == null) {
					throw new EmptyQueueException ();
				}
				
				tail.compareAndSet (cTail, next);
			} else {
				res = next.x;
				if (head.compareAndSet (cHead, next)) {
					break;
				}
			}
		}
		
		return res;
	}

	@Override
	public int peek () {
		Node cHead, cTail, next;
		
		while (true) {
			cHead = head.get ();
			cTail = tail.get ();
			next = cHead.N.get ();
			
			if (cHead == cTail) {
				if (next == null) {
					throw new EmptyQueueException ();
				}
				
				tail.compareAndSet (cTail, next);
			} else {
				return next.x;
			}
		}
	}
	
}
