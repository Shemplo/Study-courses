package ru.shemplo.tf.stcs;

import java.util.Date;
import java.util.List;

import ru.shemplo.dsau.stuctures.Pair;
import ru.shemplo.dsau.utils.time.TimePeriod;

public interface StatisticsData {

	/**
	 * Converts raw data from response of request to expected.
	 * 
	 * Formally this in not clear interface because it's limited 
	 * a lot in types of expected data and can be used mostly for
	 * particular tasks. For separating data to groups used
	 * {@link DataComposer} that can be used by this method for 
	 * help (if it's necessary).
	 * 
	 * @param composer instance of class that organize data to groups
	 * 
	 * @return list of data from request that is organized to groups
	 * 
	 */
	public List <Pair <Date, Integer>> getUsages (DataComposer <Date, Integer> composer);
	
	/**
	 * 
	 * @return source of data (request service destination)
	 * 
	 */
	public String getSource ();
	
	/**
	 * 
	 * @return key that is used for searching in service
	 * 
	 */
	public String getRequestKey ();
	
	/**
	 * 
	 * @return time period when {@link #getRequestKey () key} would be searched
	 * 
	 */
	public TimePeriod getPeriod ();
	
}
