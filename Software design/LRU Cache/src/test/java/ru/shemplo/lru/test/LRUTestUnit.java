package ru.shemplo.lru.test;

import static org.junit.jupiter.api.Assertions.*;

import java.util.HashSet;
import java.util.Random;
import java.util.Set;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.RepeatedTest;
import org.junit.jupiter.api.Test;

import ru.shemplo.lru.LRUCache;
import ru.shemplo.lru.SimpleLRUCache;

public class LRUTestUnit {

	private final Random random = new Random ();
	
	private static <K, V> LRUCache <K, V> getInstance (int capacity) {
		return new SimpleLRUCache <> (capacity);
	}
	
	@Nested
	@DisplayName ("Test cache on instantiation")
	public class InitializationTestUnit {
	
		@Test
		@DisplayName ("Test cache on instantiation")
		public void testSuccessInit () {
			assertNotNull (getInstance (10));
		}
		
		@Test
		@DisplayName ("Test cache on invalid values of constructor")
		public void testUnsuccessInit () {
			try {
				@SuppressWarnings ("unused")
				LRUCache <Integer, String> cache = getInstance (0);
				cache = getInstance (-1);
			} catch (Exception | AssertionError e) {
				return; // It's OK (expected behavior)
			}
			
			fail ("Created instance with non-positive capacity");
		}
		
		@Test
		@DisplayName ("Test cache on valid value of capacity")
		public void testCapacity () {
			int capacity = 1 + random.nextInt (1000);
			
			LRUCache <Integer, String> cache = getInstance (capacity);
			assertEquals (cache.getCapacity (), capacity);
		}
		
	}
	
	@Nested
	@DisplayName ("Test cache on insert")
	public class InsertTestUnit {
		
		@Test
		@DisplayName ("Test cache on signle PUT")
		public void testNormalSingleInsert () {
			LRUCache <Integer, String> cache = getInstance (100);
			cache.put (32, "Single insert");
			
			assertEquals (1, cache.getSize ());
		}
		
		@Test
		@DisplayName ("Test cache on PUT null value with normal key")
		public void testNullValueInsert () {
			LRUCache <Integer, String> cache = getInstance (100);
			cache.put (63, null);
			
			assertEquals (1, cache.getSize ());
		}
		
		@Test
		@DisplayName ("Test cache on PUT value with NULL key")
		public void testNullKeyInsert () {
			LRUCache <Integer, String> cache = getInstance (100);
			try {
				cache.put (null, "Insert with null key");
			} catch (Exception | AssertionError e) {
				// It's OK (expected behavior)
				return;
			}
			
			fail ("Inserted value with null key");
		}
		
		@RepeatedTest (4)
		@DisplayName ("Test cache on insert of several values")
		public void testSeveralInserts () {
			int capacity = 10 + random.nextInt (10000),
				toInsert = 5 + random.nextInt (capacity / 2);
			LRUCache <Integer, String> cache = getInstance (capacity);
			for (int i = 0; i < toInsert; i++) {
				int value = random.nextInt (capacity * 2);
				cache.put (i, "" + value);
			}
			
			assertEquals (toInsert, cache.getSize ());
		}
		
		@RepeatedTest (4)
		@DisplayName ("Test cache on huge insert")
		public void testOverCapacityInserts () {
			int capacity = 10 + random.nextInt (10000),
				toInsert = capacity + 1 + random.nextInt (capacity);
			LRUCache <Integer, String> cache = getInstance (capacity);
			for (int i = 0; i < toInsert; i++) {
				int value = random.nextInt (toInsert);
				cache.put (i, "" + value);
			}
			
			assertEquals (capacity, cache.getSize ());
		}
		
		@RepeatedTest (4)
		@DisplayName ("Test PUT on repeating keys")
		public void testRepeatedKey () {
			LRUCache <Integer, String> cache = getInstance (1000);
			int insert = 10 + random.nextInt (10000);
			Set <Integer> keys = new HashSet <> ();
			
			for (int i = 0; i < insert; i++) {
				int key = random.nextInt (cache.getCapacity ());
				if (keys.contains (key)) {
					try {
						cache.put (key, "" + key);
					} catch (Exception | AssertionError e) {
						continue;
					}
					
					fail ("Inserted value with pereated key");
				} else {
					keys.add (key); cache.put (key, key + "");
					assertEquals (keys.size (), cache.getSize ());
				}
			}
		}
		
	}
	
	@Nested
	@DisplayName ("Test cache on real operations")
	public class FunctionalityTestUnit {
		
		@Test
		@DisplayName ("Test GET from empty cache by random key")
		public void testGetFromEpty () {
			LRUCache <Integer, String> cache = getInstance (100);
			assertNull (cache.get (random.nextInt ()));
		}
		
		@Test
		@DisplayName ("Test GET after single PUT")
		public void testInsertAndGet () {
			LRUCache <Integer, String> cache = getInstance (100);
			
			int key = random.nextInt ();
			String value = "" + (key * 2);
			cache.put (key, value);
			
			String answer = cache.get (key);
			assertEquals (value, answer);
		}
		
		@RepeatedTest (8)
		@DisplayName ("Test cache on correct GET requests")
		public void testInsertAndSeveralGet () {
			LRUCache <Integer, String> cache = getInstance (10000);
			int range = 10 + random.nextInt (1000);
			
			for (int i = 0; i < cache.getCapacity () + range; i++) {
				cache.put (i, "value-" + i);
			}
			
			int missed = 0;
			for (int i = cache.getCapacity () + range - 1; i >= 0; i--) {
				missed += (cache.get (i) == null ? 1 : 0);
			}
			
			assert missed == range;
		}
		
		@RepeatedTest (4)
		@DisplayName ("Test cache on correct GET requests from tail")
		public void testInsertAndGetFromTail () {
			int capacity = 10 + random.nextInt (10000);
			LRUCache <Integer, String> cache = getInstance (capacity);
			for (int i = 0; i < cache.getCapacity (); i++) {
				cache.put (i, "value-" + i);
				
				if (i > 0) { assert cache.get (i - 1) != null; }
			}
			
			cache.put (-1, "stub");
			assertNull (cache.get (1));
		}
		
	}
	
}
