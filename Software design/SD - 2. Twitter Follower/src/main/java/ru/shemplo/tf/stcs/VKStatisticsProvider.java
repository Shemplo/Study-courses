package ru.shemplo.tf.stcs;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.vk.api.sdk.objects.wall.WallPostFull;

import ru.shemplo.dsau.stuctures.Pair;
import ru.shemplo.dsau.utils.time.TimePeriod;
import ru.shemplo.dsau.utils.time.TimeUtils;

public class VKStatisticsProvider implements StatisticsProvider {

	@SuppressWarnings ("unused")
	private final List <WallPostFull> DATES;
	private final TimePeriod PERIOD;
	private final String KEY;
	
	public VKStatisticsProvider (String key, TimePeriod period, List <WallPostFull> wallPosts) {
		if (key == null || period == null || wallPosts == null) {
			String text = "Argumens of " + this.getClass ().getSimpleName () + " can't be NULL";
			throw new IllegalArgumentException (text);
		}
		
		this.PERIOD = TimePeriod.mtp (TimeUtils.floorToHours (period.F), period.S);
		this.DATES = new ArrayList <> (wallPosts);
		this.KEY = key;
	}

	@Override
	public List <Pair <Date, Integer>> getUsages (DataComposer <Date, Integer> composer) {
		//return composer.compose (DATES);
		return null;
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
