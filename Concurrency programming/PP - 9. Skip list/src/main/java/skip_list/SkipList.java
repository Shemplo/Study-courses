package skip_list;

import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.atomic.AtomicMarkableReference;

public class SkipList implements Set {

	private final int MAX_LEVEL = 30;
	private final Node head;

	public SkipList () {
		this.head = new Node (Integer.MIN_VALUE, MAX_LEVEL);
		Node tail = new Node (Integer.MAX_VALUE, MAX_LEVEL);
		for (int i = 0; i < head.N.length; i++) {
			head.N [i] = new AtomicMarkableReference <> (tail, false);
		}
	}

	private class Node {

		final int x, topLevel;
		final AtomicMarkableReference <Node> [] N;

		@SuppressWarnings ("unchecked")
		public Node (int x, int height) {
			this.topLevel = height;
			this.x = x;

			N = new AtomicMarkableReference [height + 1];
			for (int i = 0; i < N.length; i++) {
				N [i] = new AtomicMarkableReference <> (null, false);
			}
		}

		@SuppressWarnings ("unchecked")
		public Node (int x, int height, Node [] succs) {
			this.topLevel = height;
			this.x = x;

			this.N = new AtomicMarkableReference [height + 1];
			for (int i = 0; i < N.length; i++) {
				N [i] = new AtomicMarkableReference <> (succs [i], false);
			}
		}

	}

	private class Window {

		boolean found = false;
		Node [] preds = new Node [MAX_LEVEL + 1];
		Node [] succs = new Node [MAX_LEVEL + 1];

	}

	/**
	 * Returns the {@link Window}, where {@code preds[l].x < x <= succs[l].x} for
	 * every level {@code l}
	 */
	private Window findWindow (int x) {
		boolean [] removed = new boolean [] {false};
		boolean found = false;

		loop: while (true) {
			Window w = new Window ();
			Node pred = head;
			for (int l = MAX_LEVEL; l >= 0; l--) {
				Node cur = pred.N [l].getReference ();
				while (true) {
					Node succ = cur.N [l].get (removed);

					while (removed [0]) {
						if (!pred.N [l].compareAndSet (cur, succ, false, false)) {
							continue loop;
						}

						cur = pred.N [l].getReference ();
						succ = cur.N [l].get (removed);
					}

					if (cur.x < x) {
						pred = cur;
						cur = succ;
					} else {
						break;
					}
				}

				w.preds [l] = pred;
				w.succs [l] = cur;

				found = cur.x == x;
			}

			w.found = found;
			return w;
		}
	}

	@Override
	public boolean add (int x) {
		int topLevel = randomLevel ();

		while (true) {
			Window w = findWindow (x);

			if (w.found)
				return false;

			Node newNode = new Node (x, topLevel, w.succs);

			if (!w.preds [0].N [0].compareAndSet (w.succs [0], newNode, false, false)) {
				continue;
			}

			for (int l = 1; l <= topLevel; l++) {
				while (true) {
					if (w.preds [l].N [l].compareAndSet (w.succs [l], newNode, false, false)) {
						break;
					}

					w = findWindow (x);
				}
			}

			return true;
		}
	}

	private int randomLevel () {
		return ThreadLocalRandom.current ().nextInt (MAX_LEVEL);
	}

	@Override
	public boolean remove (int x) {
		Window w = findWindow (x);
		if (!w.found) {
			return false;
		}

		Node oldNode = w.succs [0];
		for (int l = oldNode.topLevel; l >= 1; l--) {
			boolean [] removed = {false};

			Node next = oldNode.N [l].get (removed);

			while (!removed [0]) {
				oldNode.N [l].attemptMark (next, true);
				next = oldNode.N [l].get (removed);
			}
		}

		boolean [] removed = {false};
		Node next = oldNode.N [0].get (removed);
		while (true) {
			boolean isMarked = oldNode.N [0].compareAndSet (next, next, false, true);
			if (isMarked) {
				findWindow (x);
				return true;
			}

			next = w.succs [0].N [0].get (removed);
			if (removed [0])
				return false;
		}
	}

	@Override
	public boolean contains (int x) {
		boolean [] removed = {false};
		Node pred = head, cur = null;

		for (int l = MAX_LEVEL; l >= 0; l--) {
			cur = pred.N [l].getReference ();

			while (true) {
				Node next = cur.N [l].get (removed);

				if (removed [0]) {
					cur = next;
				} else if (cur.x < x) {
					pred = cur;
					cur = next;
				} else {
					break;
				}
			}
		}

		return cur.x == x;
	}

}
