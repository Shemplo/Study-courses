package ru.shemplo.tf.gfx;

import java.util.Date;

import ru.shemplo.tf.stcs.DataComposer;

public interface ResultProducer <T> {
	
	/**
	 * Convert provided data to the final instance.
	 * 
	 * This can be used for representing data in human-accessible view.
	 * For separating data to groups used {@link DataComposer} that can
	 * be used by this method for help (if it's necessary).
	 * 
	 * @param composer instance of class that organize data to groups
	 * 
	 * @return instance of data representation
	 * 
	 */
	public T produce (DataComposer <Date, Integer> composer);
	
}
