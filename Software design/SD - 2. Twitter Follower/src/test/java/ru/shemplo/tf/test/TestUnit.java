package ru.shemplo.tf.test;

import static org.junit.jupiter.api.Assertions.*;

import java.util.Date;
import java.util.Random;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.RepeatedTest;

import ru.shemplo.dsau.utils.time.TimeDelta;
import ru.shemplo.dsau.utils.time.TimeDelta.TDUnit;
import ru.shemplo.dsau.utils.time.TimeUtils;

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
			
			@RepeatedTest (4)
			public void testTimeDeltaInitFromDate () {
				
			}
			
			@RepeatedTest (8)
			public void testTimeDeltaGet () {
				
			}
			
		}
		
	}
	
}
