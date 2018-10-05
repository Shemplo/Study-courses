package ru.shemplo.tf.test;

import static org.junit.jupiter.api.Assertions.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Random;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.RepeatedTest;
import org.junit.jupiter.api.Test;

import ru.shemplo.dsau.stuctures.Pair;
import ru.shemplo.dsau.utils.time.TimeDelta;
import ru.shemplo.dsau.utils.time.TimeDelta.TDUnit;
import ru.shemplo.dsau.utils.time.TimeUtils;
import ru.shemplo.tf.TimePeriod;
import ru.shemplo.tf.stcs.HoursComposer;
import ru.shemplo.tf.stcs.StatisticsProvider;
import ru.shemplo.tf.stcs.TweetsStatProvider;

public class TestUnit {

	private final Random RANDOM = new Random ();
	
	@Nested
	public class TestTimeLibrary {
		
		@Nested
		public class TestTimeUtils {
			
			@RepeatedTest (16)
			public void testTimeFloor () {
				long hourLength = 1000 * 60 * 60, 
					 hours = RANDOM.nextInt (10000) * hourLength;
				
				Date date = new Date (hours + RANDOM.nextInt ((int) hourLength));
				assertEquals (hours, TimeUtils.floorToHours (date).getTime ());
				
				date = new Date (hours + (1000 * 60 * 60 - 1));
				assertEquals (hours, TimeUtils.floorToHours (date).getTime ());
				
				date = new Date (hours + 1000 * 60 * 60);
				assertEquals (hours + hourLength, TimeUtils.floorToHours (date).getTime ());
			}
			
		}
		
		@Nested
		public class TestTimeDelta {
			
			@RepeatedTest (8)
			public void testTimeDeltaInit () {
				long delta = 0;
				assertEquals (Math.abs (delta = RANDOM.nextLong ()), 
							TimeDelta.valueOf (delta).getLength ());
				
				int days = 1 + RANDOM.nextInt (5);
				TimeDelta dt = TimeDelta.valueOf (TDUnit.DAY, days);
				assertEquals (days * 24 * 60 * 60 * 1000, dt.getLength ());
			}
			
			@RepeatedTest (8)
			public void testTimeDeltaInitFromDate () {
				Date from = new Date (1000 + RANDOM.nextInt (10000)),
					 to = new Date (from.getTime () + 45364);
				TimeDelta delta = TimeDelta.deltaOf (from, to);
				assertEquals (45364, delta.getLength ());
				
				int randomDelta = RANDOM.nextInt ();
				from = new Date (1000 + RANDOM.nextInt (10000));
				to = new Date (from.getTime () + randomDelta);
				delta = TimeDelta.deltaOfPeriod (Pair.mp (from, to));
				
				assertEquals (Math.abs (randomDelta), delta.getLength ());
			}
			
			@RepeatedTest (32)
			public void testTimeDeltaFloorGet () {
				int random = RANDOM.nextInt ();
				TimeDelta deltaMLS = TimeDelta.valueOf (TDUnit.MLS, random);
				assertEquals (random, deltaMLS.floorTo (TDUnit.MLS));
				
				random = RANDOM.nextInt ();
				TimeDelta deltaSEC = TimeDelta.valueOf (TDUnit.SEC, random);
				assertEquals (random, deltaSEC.floorTo (TDUnit.SEC));
				
				random = RANDOM.nextInt ();
				TimeDelta deltaMIN = TimeDelta.valueOf (TDUnit.MIN, random);
				assertEquals (random, deltaMIN.floorTo (TDUnit.MIN));
				
				random = RANDOM.nextInt ();
				TimeDelta deltaHWR = TimeDelta.valueOf (TDUnit.HWR, random);
				assertEquals (random, deltaHWR.floorTo (TDUnit.HWR));
				
				random = RANDOM.nextInt ();
				TimeDelta deltaDAY = TimeDelta.valueOf (TDUnit.DAY, random);
				assertEquals (random, deltaDAY.floorTo (TDUnit.DAY));
			}
			
			@RepeatedTest (8)
			public void testTimeDeltaGet () {
				int random = RANDOM.nextInt ();
				TimeDelta deltaMLS = TimeDelta.valueOf (TDUnit.MLS, random);
				assertEquals (random % TDUnit.MLS.CAPACITY, deltaMLS.get (TDUnit.MLS));
				
				random = RANDOM.nextInt ();
				TimeDelta deltaSEC = TimeDelta.valueOf (TDUnit.SEC, random);
				assertEquals (random % TDUnit.SEC.CAPACITY, deltaSEC.get (TDUnit.SEC));
				
				random = RANDOM.nextInt ();
				TimeDelta deltaMIN = TimeDelta.valueOf (TDUnit.MIN, random);
				assertEquals (random % TDUnit.MIN.CAPACITY, deltaMIN.get (TDUnit.MIN));
				
				random = RANDOM.nextInt ();
				TimeDelta deltaHWR = TimeDelta.valueOf (TDUnit.HWR, random);
				assertEquals (random % TDUnit.HWR.CAPACITY, deltaHWR.get (TDUnit.HWR));
			}
			
			@RepeatedTest (16)
			public void testTimeDeltaAdd () {
				TimeDelta delta = new TimeDelta ();
				long length = delta.getLength ();
				
				int dt = RANDOM.nextInt (4536756);
				delta.add (TDUnit.MLS, dt);
				assertEquals (dt, delta.getLength () - length);
				
				TimeDelta delta2 = new TimeDelta (RANDOM.nextInt ());
				delta.add (delta2, false);
				delta.add (delta2);
				
				assertEquals (dt, delta.getLength () - length);
				
				int dt2 = RANDOM.nextInt (92319832);
				delta.add (TDUnit.MLS, dt2);
				assertEquals (dt + dt2, delta.getLength () - length);
			}
			
			@Test
			public void testTimeDeltaToString () {
				TimeDelta delta = new TimeDelta (0);
				delta.add (TDUnit.MLS, 56).add (TDUnit.SEC, 55).add (TDUnit.MIN, 19)
					 .add (TDUnit.HWR, 6).add (TDUnit.DAY, 567);
				
				assertEquals ("dt = +56 MLS, 55 SEC, 19 MIN, 6 HWR, 567 DAY", delta.toString ());
			}
			
			@RepeatedTest (16)
			public void testTimeDeltaToPeriod () {
				TimeDelta delta = TimeDelta.valueOf (RANDOM.nextInt ());
				
				int dateTime = RANDOM.nextInt ();
				Date date = new Date (dateTime);
				
				TimePeriod period = delta.getPeriodFor (date);
				
				long periodLength = TimeDelta.deltaOfPeriod (period).getLength ();
				assertEquals (delta.getLength (), periodLength);
			}
			
			@Test
			public void testTimeDeltaOnErrors () {
				TimeDelta delta = TimeDelta.valueOf (RANDOM.nextInt ());
				try {
					delta.floorTo (null);
				} catch (Exception | AssertionError e) {}
				
				try {
					delta.add (null, 0);
				} catch (Exception | AssertionError e) {
					return;
				}
				
				fail ("Exception was expected on null argument");
			}
			
		}
		
		@Nested
		public class TestTimePeriod {
			
			@Test
			public void testTimePeriodSwap () {
				Date from = new Date (RANDOM.nextInt (4567)), 
					 to = new Date (320300 + RANDOM.nextInt (56766456));
				
				TimePeriod period = TimePeriod.mtp (from, to), swap = period.swap ().swap ();
				assertEquals (period.F, swap.F); assertEquals (period.S, swap.S);
			}
			
		}
		
	}
	
	@Nested
	public class TestProvider {
		
		@Nested
		public class TestComposer {
			
			@Test
			public void testHoursComposer () {
				HoursComposer composer = new HoursComposer ();
				
				long hours = composer.getTimeBounds ().get (TDUnit.HWR);
				assertEquals (1, hours);
				
				long hourLength = 1000 * 60 * 60;
				
				List <Date> usages = new ArrayList <> (Arrays.asList (
					new Date (54), new Date (hourLength + 657), new Date (hourLength * 2 + 454),
					new Date (hourLength * 3 + 5657), new Date (934), new Date (hourLength),
					new Date (hourLength + 567), new Date (76), new Date (hourLength * 2)
				));
				List <Pair <Date, Integer>> list = composer.compose (usages);
				assertEquals (4, list.size ());
				
				assertEquals (3, list.get (0).S.intValue ());
				assertEquals (3, list.get (1).S.intValue ());
				assertEquals (2, list.get (2).S.intValue ());
				assertEquals (1, list.get (3).S.intValue ());
			}
			
		}
		
		@Test
		public void testProviderGetKey () {
			TimePeriod period = new TimePeriod (new Date (), new Date ());
			
			StatisticsProvider provider = new TweetsStatProvider ("Some key", period, new ArrayList <> ());
			assertEquals ("Some key", provider.getRequestKey ());
		}
		
		@Test
		public void testProviderGetPeriod () {
			TimePeriod period = new TimePeriod (new Date (), new Date ());
			Date from = TimeUtils.floorToHours (period.F);
			
			StatisticsProvider provider = new TweetsStatProvider ("Some key", period, new ArrayList <> ());
			TimePeriod period2 = provider.getPeriod ();
			
			assertEquals (from, period2.F);
			assertEquals (period.S, period2.S);
		}
		
	}
	
}
