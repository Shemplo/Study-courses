package ru.shemplo.hw.src.arrayset;

import java.util.*;

/**
 * Unmodifiable Sorted Set
 * 
 * (This is second version that was written not by Shemplo)
 * 
 * @author egormkn (https://github.com/egormkn)
 *
 * @param <T> the type of elements maintained by this set
 * 
 * @see SortedSet
 * @see NavigableSet
 * 
 */

public class ArraySetM <T> extends AbstractSet <T> implements NavigableSet <T> {

	private final List <T> items;
	private final Comparator <? super T> comparator;
	private NavigableSet <T> descending = null;

	public ArraySetM () {
		this (Collections.emptyList (), null);
	}

	public ArraySetM (Collection <T> data) {
		this (data, null);
	}

	public ArraySetM (Collection <T> data, Comparator <? super T> cmp) {
		comparator = cmp;
		TreeSet <T> treeSet = new TreeSet <> (comparator);
		treeSet.addAll (data);
		items = Collections.unmodifiableList (new ArrayList <> (treeSet));
	}

	private ArraySetM (List <T> sortedList, Comparator <? super T> cmp, boolean sorted) {
		comparator = cmp;
		items = sortedList;
	}

	@Override
	public T first () {
		if (isEmpty ()) { throw new NoSuchElementException ("ArraySet is empty"); }
		return items.get (0);
	}

	@Override
	public T last () {
		if (isEmpty ()) { throw new NoSuchElementException ("ArraySet is empty"); }
		return items.get (items.size () - 1);
	}

	@Override
	public int size () {
		return items.size ();
	}

	@Override
	@SuppressWarnings ("unchecked")
	public boolean contains (Object o) {
		return Collections.binarySearch (items, (T) o, comparator) >= 0;
	}

	@Override
	public Iterator <T> iterator () {
		return items.listIterator ();
	}

	@Override
	public Comparator <? super T> comparator () {
		return comparator;
	}

	@Override
	public SortedSet <T> subSet (T fromElement, T toElement) {
		return subSet (fromElement, true, toElement, false);
	}

	@Override
	public SortedSet <T> headSet (T toElement) {
		return headSet (toElement, false);
	}

	@Override
	public SortedSet <T> tailSet (T fromElement) {
		return tailSet (fromElement, true);
	}

	@Override
	public T pollFirst () {
		throw new UnsupportedOperationException ("ArraySet is immutable");
	}

	@Override
	public T pollLast () {
		throw new UnsupportedOperationException ("ArraySet is immutable");
	}

	@Override
	public NavigableSet <T> descendingSet () {
		if (descending == null) {
			descending = new ArraySetM <> (items, Collections.reverseOrder (comparator));
		}
		return descending;
	}

	@Override
	public Iterator <T> descendingIterator () {
		return descendingSet ().iterator ();
	}

	/**
	 * Returns the greatest element in this set strictly less than the given
	 * element, or {@code null} if there is no such element.
	 */
	@Override
	public T lower (T e) {
		int index = lessThan (e, false);
		return index < 0 ? null : items.get (index);
	}

	/**
	 * Returns the greatest element in this set less than or equal to the given
	 * element, or {@code null} if there is no such element.
	 */
	@Override
	public T floor (T e) {
		int index = lessThan (e, true);
		return index < 0 ? null : items.get (index);
	}

	/**
	 * Returns the least element in this set greater than or equal to the given
	 * element, or {@code null} if there is no such element.
	 */
	@Override
	public T ceiling (T e) {
		int index = greaterThan (e, true);
		return index < items.size () ? items.get (index) : null;
	}

	/**
	 * Returns the least element in this set strictly greater than the given
	 * element, or {@code null} if there is no such element.
	 */
	@Override
	public T higher (T e) {
		int index = greaterThan (e, false);
		return index < items.size () ? items.get (index) : null;
	}

	/**
	 * Returns a view of the portion of this set whose elements range from
	 * {@code fromElement} to {@code toElement}. If {@code fromElement} and
	 * {@code toElement} are equal, the returned set is empty unless {@code
	 * fromInclusive} and {@code toInclusive} are both true. The returned set is
	 * backed by this set, so changes in the returned set are reflected in this set,
	 * and vice-versa. The returned set supports all optional set operations that
	 * this set supports.
	 *
	 * <p>
	 * The returned set will throw an {@code IllegalArgumentException} on an attempt
	 * to insert an element outside its range.
	 *
	 * @param fromElement
	 *            low endpoint of the returned set
	 * @param fromInclusive
	 *            {@code true} if the low endpoint is to be included in the returned
	 *            view
	 * @param toElement
	 *            high endpoint of the returned set
	 * @param toInclusive
	 *            {@code true} if the high endpoint is to be included in the
	 *            returned view
	 * @return a view of the portion of this set whose elements range from
	 *         {@code fromElement}, inclusive, to {@code toElement}, exclusive
	 * @throws ClassCastException
	 *             if {@code fromElement} and {@code toElement} cannot be compared
	 *             to one another using this set's comparator (or, if the set has no
	 *             comparator, using natural ordering). Implementations may, but are
	 *             not required to, throw this exception if {@code fromElement} or
	 *             {@code toElement} cannot be compared to elements currently in the
	 *             set.
	 * @throws NullPointerException
	 *             if {@code fromElement} or {@code toElement} is null and this set
	 *             does not permit null elements
	 * @throws IllegalArgumentException
	 *             if {@code fromElement} is greater than {@code toElement}; or if
	 *             this set itself has a restricted range, and {@code fromElement}
	 *             or {@code toElement} lies outside the bounds of the range.
	 */
	@Override
	public NavigableSet <T> subSet (T fromElement, boolean fromInclusive, T toElement, boolean toInclusive) {
		int l = greaterThan (fromElement, fromInclusive);
		int r = lessThan (toElement, toInclusive) + 1;
		return new ArraySetM <> (items.subList (l, r < l ? l : r), comparator, true);
	}

	@Override
	public NavigableSet <T> headSet (T toElement, boolean inclusive) {
		return new ArraySetM <> (items.subList (0, lessThan (toElement, inclusive) + 1), comparator, true);
	}

	@Override
	public NavigableSet <T> tailSet (T fromElement, boolean inclusive) {
		return new ArraySetM <> (items.subList (greaterThan (fromElement, inclusive), items.size ()), comparator, true);
	}

	/**
	 * @return The index of the least element in this set greater than (or equal) to
	 *         the given element
	 */
	private int greaterThan (T element, boolean orEqual) {
		int index = Collections.binarySearch (items, element, comparator);
		return index < 0 ? ~index // Insertion point: the index of first element greater than the key
				: orEqual ? index : index + 1;
	}

	/**
	 * @return The index of the greatest element in this set less than (or equal) to
	 *         the given element
	 */
	private int lessThan (T element, boolean orEqual) {
		int index = Collections.binarySearch (items, element, comparator);
		return index < 0 ? ~index - 1 // The index of last element less than the key
				: orEqual ? index : index - 1;
	}
}
