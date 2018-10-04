package ru.shemplo.tf.stcs;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import ru.shemplo.dsau.utils.time.TimeUtils;
import ru.shemplo.tf.TimePeriod;

public class TweetsStatProvider implements StatisticsProvider {

	private final List <Date> DATES;
	private final TimePeriod PERIOD;
	private final String KEY;
	
	public TweetsStatProvider (String key, TimePeriod period, List <Date> usages) {
		this.PERIOD = TimePeriod.mtp (TimeUtils.floorToHours (period.F), period.S);
		this.DATES = new ArrayList <> (usages);
		this.KEY = key;
	}

	@Override
	public List <Integer> getUsages (DataComposer <Date, Integer> composer) {
		return composer.compose (DATES);
	}

	@Override
	public String getRequestKey () {
		return this.KEY;
	}
	
	@Override
	public TimePeriod getPeriod () {
		return this.PERIOD;
	}

}
