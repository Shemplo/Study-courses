package ru.shemplo.tf.ntwk;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Random;

import java.io.IOException;

import ru.shemplo.dsau.utils.TimeDelta;
import ru.shemplo.tf.TimePeriod;
import ru.shemplo.tf.stcs.StatisticsProvider;
import ru.shemplo.tf.stcs.TweetsStatProvider;

public class LocalNetworkSession implements NetworkSession {

	@Override
	public boolean isConnected () {
		return false;
	}

	@Override
	public void tryConnect () throws IOException {
		
	}

	@Override
	public StatisticsProvider sendRequest (String key, TimePeriod period) throws IOException {
		/*
		DateFormat format = new SimpleDateFormat ("EEE MMM dd HH:mm:ss Z YYYY", Locale.ENGLISH);
		List <Pair <Date, Integer>> usages = null;
		try {
			usages = new ArrayList <> (Arrays.asList (
				Pair.mp (format.parse ("Thu Feb 01 16:33:04 +0000 2018"), 23),
				Pair.mp (format.parse ("Fri Feb 02 18:15:27 +0000 2018"), 9),
				Pair.mp (format.parse ("Thu Feb 01 06:45:52 +0000 2018"), 64),
				Pair.mp (format.parse ("Sun Feb 04 09:20:38 +0000 2018"), 119)
			));
		} catch (ParseException pe) {
			pe.printStackTrace ();
		}
		*/
		
		long periodLength = TimeDelta.deltaOfPeriod (period).getLength ();
		List <Date> usages = new ArrayList <> ();
		Random random = new Random ();
		
		for (int i = 0; i < periodLength / 1000; i ++) {
			int delta = random.nextInt ((int) periodLength);
			usages.add (new Date (period.F.getTime () + delta));
		}
		
		return new TweetsStatProvider (key, period, usages);
	}

}
