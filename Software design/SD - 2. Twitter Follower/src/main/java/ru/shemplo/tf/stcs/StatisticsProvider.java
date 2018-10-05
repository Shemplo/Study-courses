package ru.shemplo.tf.stcs;

import java.util.Date;
import java.util.List;

import ru.shemplo.dsau.stuctures.Pair;
import ru.shemplo.tf.TimePeriod;

public interface StatisticsProvider {

	public List <Pair <Date, Integer>> getUsages (DataComposer <Date, Integer> composer);
	
	public String getRequestKey ();
	
	public TimePeriod getPeriod ();
	
}
