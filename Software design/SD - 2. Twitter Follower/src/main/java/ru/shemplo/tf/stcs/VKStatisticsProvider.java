package ru.shemplo.tf.stcs;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import java.util.function.Function;

import com.google.gson.JsonObject;

import ru.shemplo.dsau.stuctures.Pair;
import ru.shemplo.dsau.utils.time.TimePeriod;
import ru.shemplo.dsau.utils.time.TimeUtils;

public class VKStatisticsProvider implements StatisticsProvider {

	private final List <JsonObject> POSTS;
	private final TimePeriod PERIOD;
	private final String KEY;
	
	public VKStatisticsProvider (String key, TimePeriod period, List <JsonObject> wallPosts) {
		if (key == null || period == null || wallPosts == null) {
			String text = "Argumens of " + this.getClass ().getSimpleName () + " can't be NULL";
			throw new IllegalArgumentException (text);
		}
		
		this.PERIOD = TimePeriod.mtp (TimeUtils.floorToHours (period.F), period.S);
		this.POSTS = new ArrayList <> (wallPosts);
		this.KEY = key;
	}

	@Override
	public List <Pair <Date, Integer>> getUsages (DataComposer <Date, Integer> composer) {
		Function <JsonObject, Date> toDate = 
			p -> new Date (p.getAsJsonPrimitive ("date").getAsLong () * 1000);
		List <Date> dates = POSTS.stream ().map (toDate)
						  . collect (Collectors.toList ());
		return composer.compose (dates);
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
