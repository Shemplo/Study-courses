package ru.shemplo.tf.gfx;

import java.util.Date;
import java.util.List;

import java.awt.image.BufferedImage;

import ru.shemplo.dsau.utils.time.TimeDelta;
import ru.shemplo.dsau.utils.time.TimeUtils;
import ru.shemplo.tf.TimePeriod;
import ru.shemplo.tf.stcs.DataComposer;
import ru.shemplo.tf.stcs.StatisticsProvider;

public class ImageResultProducer implements ResultProducer <BufferedImage> {

	private final StatisticsProvider PROVIDER;
	
	public ImageResultProducer (StatisticsProvider provider) {
		this.PROVIDER = provider;
	}
	
	@Override
	public BufferedImage produce (DataComposer <Date, Integer> composer) {
		List <Integer> usages = PROVIDER.getUsages (composer);
		
		TimePeriod period = PROVIDER.getPeriod ();
		Date from = period.F, to = TimeUtils.floorToHours (period.S),
			 fromDay = TimeUtils.floorToDays (from);
		TimeDelta delta = TimeDelta.deltaOf (from, to), skip = TimeDelta.deltaOf (fromDay, from);
		System.out.println (from + " " + to + "\n"  + delta + "\n" + fromDay + "\n" + skip);
		return null;
	}

}
