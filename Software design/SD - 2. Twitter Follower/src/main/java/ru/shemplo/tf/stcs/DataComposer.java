package ru.shemplo.tf.stcs;

import java.util.List;

import ru.shemplo.dsau.stuctures.Pair;

public interface DataComposer <T, N extends Number> extends HasTimePeriod {

	/**
	 * Organize given data to some groups.
	 * 
	 * Formally this in not clear interface because it's
	 * limited a lot in possible types that can be used
	 * as group value. It's done only for counting number
	 * of objects that belongs to some same criteria, and
	 * the identifier of the group must be object of the 
	 * same type as given data.
	 * 
	 * @param data that should be separated on parts
	 * 
	 * @return data organized in groups
	 * 
	 */
	public List <Pair <T, N>> compose (List <T> data);
	
}
