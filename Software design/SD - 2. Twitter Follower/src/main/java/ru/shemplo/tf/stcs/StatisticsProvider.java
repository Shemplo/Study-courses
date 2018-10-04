package ru.shemplo.tf.stcs;

import java.util.Date;
import java.util.List;

public interface StatisticsProvider extends HasTimePeriod {

	public List <Integer> getUsages (DataComposer <Date, Integer> composer);
	
	public String getRequestKey ();
	
}
