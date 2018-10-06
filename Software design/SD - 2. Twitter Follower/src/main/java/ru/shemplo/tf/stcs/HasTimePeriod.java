package ru.shemplo.tf.stcs;

import java.util.Date;

import ru.shemplo.dsau.utils.time.TimeDelta;

public interface HasTimePeriod {

	/**
	 * Returns time limits.
	 * 
	 * Result is given in {@link TimeDelta} type and in combination
	 * with {@link Date} can give real time period (from date to date).
	 * 
	 * @return time limits that is set up in current instance
	 * 
	 */
	public TimeDelta getTimeBounds ();
	
}
