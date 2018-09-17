package ru.shemplo.hw.src.arrayset;

import static java.util.Collections.binarySearch;
import static java.util.Collections.unmodifiableList;

import java.util.AbstractSet;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.NavigableSet;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.function.Predicate;

/**
 * 
 * This is implementation of {@link NavigableSet} interface.
 * 
 * The feature of this variant of set is that it stores
 * only one copy of data. By the contract this set is immutable
 * that means that array of data can't be changed and no reasons
 * to clone data on each calling of methods such as 
 * {@link ArraySetS#subSet(Object, Object) subSet}. The solution
 * is to pass pointer to an array to the new instance and also
 * pass two parameters that bound available part of array from left and right.
 * 
 * @author Shemplo
 *
 * @param <T> type of contained elements
 * 
 */
public class ArraySetS <T> extends AbstractSet <T> implements NavigableSet <T> {

	public static final String CLASS_NAME;
	static {
		// Assigning name of class to variable
		CLASS_NAME = ArrayList.class.getSimpleName ();
	}
	
	// This is an object that allows to compare elements in set
	private Comparator <? super T> COMPARATOR;
	// This is a field to mark that array must be reversed
	private boolean reversed = false;
	// Bounds of available part of array
	private int LEFT = 0, RIGHT = 1;
	// Instance of array that stores elements
	private final List <T> ARRAY;
	
	private static enum Attitude {
		LOWER, UPPER;
	}
	
	private static enum Location {
		START, END;
	}
	
	/**
	 * Default constructor of empty {@link ArraySetS}
	 * 
	 */
	public ArraySetS () {
		this.ARRAY = unmodifiableList (new ArrayList <> ());
		this.RIGHT = 0;
	}
	
	// Constructor that allows to create new instance without cloning data
	private ArraySetS (int l, int r, List <T> array, boolean reversed) {
		this.reversed = reversed;
		this.ARRAY = array;
		this.RIGHT = r;
		this.LEFT  = l;
	}
	
	/**
	 * This constructor allows to create an {@link ArraySetS} from {@link Collection}
	 * and also set a {@link Comparator} for elements. 
	 * (If comparator will be null then will be checked 
	 * if elements are {@link Comparable}... 
	 * if not then will be used default attitude of equivalences)
	 * 
	 * @param array
	 * @param comparator
	 * 
	 */
	public ArraySetS (Collection <T> array, 
						Comparator <? super T> comparator) {
		this.COMPARATOR = comparator;
		
		if (this.COMPARATOR == null) {
			this.COMPARATOR = (a, b) -> compare (a, b);
		}
		
		Set <T> ordCollection = new TreeSet <> (this.COMPARATOR);
		ordCollection.addAll (array);
		
		List <T> modList = new ArrayList <> (ordCollection);
		this.ARRAY = unmodifiableList (modList);
	}
	
	// This method compares to elements if it's possible
	private <C extends T> int compare (C obj1, C obj2) {
		if (this.COMPARATOR != null) { // Try to do this with a comparator
			return COMPARATOR.compare (obj1, obj2);
		}
		
		if (obj1 != null && (obj1 instanceof Comparable)) {
			// Try to compare objects if they implements interface Comparable
			@SuppressWarnings ("unchecked") // This is for compiler
			Comparable <T> comparableObj = (Comparable <T>) obj1;
			return comparableObj.compareTo (obj2);
		}
		
		// No ways more to compare objects
		// Saying that they are equal
		return 0;
	}
	
	@Override
	public Comparator <? super T> comparator () {
		return COMPARATOR;
	}

	@Override
	public T first () {
		// Getting the first (lowest) element
		return get (0);
	}

	@Override
	public T last () {
		// Getting the last (highest) element
		return get (size () - 1);
	}
	
	/**
	 * Returns value by the index.
	 * 
	 * @param index
	 * @return
	 * 
	 */
	private final T get (int index) {
		if (isEmpty ()) {
			String message = CLASS_NAME + " is empty";
			throw new NoSuchElementException (message);
		}
		
		index = reversed ? size () - index - 1 : index;
		return ARRAY.get (LEFT + index);
	}

	@Override
	public T ceiling (T match) {
		return getSpecific (match, 0, Attitude.LOWER, inRightBound);
	}
	
	@Override
	public T floor (T match) {
		return getSpecific (match, 1, Attitude.UPPER, inLeftBound);
	}
	
	@Override
	public T higher (T match) {
		return getSpecific (match, 0, Attitude.UPPER, inRightBound);
	}

	@Override
	public T lower (T match) {
		return getSpecific (match, 1, Attitude.LOWER, inLeftBound);
	}
	
	private final Predicate <Integer> inLeftBound = n -> n >= LEFT,
										inRightBound = n -> n < RIGHT;
	
	private final T getSpecific (T match, int offset, Attitude att, 
									Predicate <Integer> p) {
		int index = search (match, att);
		return p.test (index)
				? ARRAY.get (index - offset)
				: null;
	}

	@Override
	public Iterator <T> descendingIterator () {
		return descendingSet ().iterator ();
	}

	private NavigableSet <T> desc = null;
	
	@Override
	public NavigableSet <T> descendingSet () {
		if (desc == null) {
			desc = new ArraySetS <> (LEFT, RIGHT, ARRAY, 
										!reversed);
		}
		
		return desc;
	}

	@Override
	public T pollFirst () {
		String message = "This " + CLASS_NAME + " is unmodifiable";
		throw new UnsupportedOperationException (message);
	}

	@Override
	public T pollLast () {
		String message = "This " + CLASS_NAME + " is unmodifiable";
		throw new UnsupportedOperationException (message);
	}

	@Override
	public SortedSet <T> subSet (T fromElement, T toElement) {
		return subSet (fromElement, true, toElement, false);
	}

	@Override
	public NavigableSet <T> subSet (T fromElement, boolean fromInclusive, 
									T toElement, boolean toInclusive) {
		if (compare (fromElement, toElement) > 0) {
			String message = "Start element is greather then end elemnt";
			throw new IllegalArgumentException (message);
		}
		
		Attitude [] atts = Attitude.values ();
		int from = search (fromElement, atts [fromInclusive ? 0 : 1]),
				to = search (toElement, atts [toInclusive ? 1 : 0]);
		
		from = Math.max (from, LEFT);
		to = Math.min (to, RIGHT - 1);
		
		if (from > to) {
			return new ArraySetS <> ();
		}
		
		return new ArraySetS <> (LEFT, RIGHT, ARRAY, false);
	}
	
	@Override
	public SortedSet <T> headSet (T toElement) {
		return headSet (toElement, reversed ? true : false);
	}
	
	@Override
	public NavigableSet <T> headSet (T toElement, boolean inclusive) {
		if (isEmpty ()) { return this; }
		
		return fetchSubSet (Location.START, toElement, inclusive);
	}

	@Override
	public SortedSet <T> tailSet (T toElement) {
		return tailSet (toElement, reversed ? false : true);
	}

	@Override
	public NavigableSet <T> tailSet (T fromElement, boolean inclusive) {
		if (isEmpty ()) { return this; }
		
		return fetchSubSet (Location.END, fromElement, inclusive);
	}
	
	private final NavigableSet <T> fetchSubSet (Location loc, T match, boolean inclusive) {
		if (LEFT >= search (match, inclusive ? Attitude.UPPER : Attitude.LOWER)
				|| RIGHT <= search (match, inclusive ? Attitude.LOWER : Attitude.UPPER)) {
			return new ArraySetS <> ();
		}
		
		if ((reversed && loc.equals (Location.END))
				|| (!reversed && loc.equals (Location.START))) {
			return subSet (first (), true, match, inclusive);
		}
		
		return subSet (match, inclusive, last (), true);
	}

	@Override
	public Iterator <T> iterator () {
		return ARRAY.iterator ();
	}

	@Override
	public int size () {
		return RIGHT - LEFT;
	}
	
	private final int search (T element, Attitude att) {
		int index = binarySearch (ARRAY, element, comparator ());
		index = index < 0
					? ~index
					: att.equals (Attitude.UPPER)
						? index++
						: index;
		return index;
	}

}
