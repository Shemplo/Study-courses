package ru.shemplo.tf.stcs;

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import ru.shemplo.dsau.utils.time.TimeDelta;
import ru.shemplo.dsau.utils.time.TimeDelta.TDUnit;
import ru.shemplo.dsau.utils.time.TimeUtils;

public class HoursComposer implements DataComposer <Date, Integer> {

	@Override
	public List <Integer> compose (List <Date> data) {
		Map <Long, Integer> entries = new HashMap <> ();
		for (Date date : data) {
			long key = TimeUtils.floorToHours (date).getTime ();
			
			entries.putIfAbsent (key, 0);
			entries.compute (key, (k, v) -> v + 1);
		}
		
		return entries.keySet ().stream ()
			 . sorted ()
			 . map (entries::get)
			 . collect (Collectors.toList ());
	}

	@Override
	public TimeDelta getTimeBounds () {
		return TimeDelta.valueOf (TDUnit.HWR, 1);
	}
	
}
