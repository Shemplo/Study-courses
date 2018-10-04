package ru.shemplo.tf.stcs;

import java.util.Date;
import java.util.List;

import ru.shemplo.tf.TimePeriod;

public class TweetsStatProvider implements StatisticsProvider {

	private final TimePeriod PERIOD;
	private final String KEY;
	
	public TweetsStatProvider (String key, TimePeriod period, List <Date> usages) {
		this.KEY = key; this.PERIOD = period;
	}
	
	@Override
	public TimePeriod getTimeBounds () {
		return this.PERIOD;
	}

	@Override
	public List <Integer> getUsages (DataComposer <Date, Integer> composer) {
		return null;
	}

	@Override
	public String getRequestKey () {
		return this.KEY;
	}

}
