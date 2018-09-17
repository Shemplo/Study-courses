package ru.shemplo.hw.test.walk;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.Reader;
import java.util.HashMap;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Random;
import java.util.StringTokenizer;
import java.util.stream.Stream;

import ru.shemplo.hw.src.walk.WalkNR;

public class WalkTest {

	private static final String TEST_DIR = "walk_test_dir";
	
	private static Map <String, String> files;
	
	public static void main (String... args) {
		long start = System.currentTimeMillis ();
		
		File runDirectory = new File (".");
		if (!runDirectory.canWrite ()) {
			System.err.println ("Test can write in this directory");
			System.err.println ("Run it with a root rights or move to another directory");
			System.exit (1);
		}

		files = new HashMap <> ();
		String [][] nameArgs = {
			null,
			{},
			{"___input___"},
			{null, null},
			{null, "output"},
			{"___input___", null},
			{"___input___", "output"},
		};
		
		for (int i = 0; i < nameArgs.length; i ++) {
			String [] names = nameArgs [i];
			if (names != null && names.length >= 1 && names [0] != null) {
				File tmp = new File (runDirectory, names [0]);
				tmp.delete ();
			}
			
			System.out.print ("Call test " + i + " - ");
			try {
				// TODO: user class
				WalkNR.main (nameArgs [i]);
				System.out.println ();
			} catch (Exception e) {
				System.out.println (e.toString ());
			}
			
			if (names != null && names.length >= 2 && names [1] != null) {
				File tmp = new File (runDirectory, names [1]);
				tmp.delete ();
			}
		}
		
		System.out.println ("(If some of previous ^ has no error message - FAIL)");
		System.out.println ();
		
		int [][] presets = {
			// Depth folders files weight
			{0,      0,      1,    0},
			{0,      0,      17,   0},
			{0,      0,      1,    155460},
			{0,      0,      1,    4356},
			{0,      0,      10,   340},
			{1,      1,      1,    189},
			{2,      2,      2,    382},
			{2,      10,     2,    4520},
			{2,      3,      8,    0},
			{2,      3,      8,    2145},
			{2,      20,     20,   23},
			{5,      2,      5,    53},
			{10,     2,      3,    521},
			{20,     1,      2,    124}
		};
		
		test_loop:
		for (int i = 0; i < presets.length; i ++) {
			System.out.print ("Walk test " + i + " - ");
			
			files.clear ();
			makeTestDir (runDirectory, presets [i]);
			makeInputFile (runDirectory, presets [i]);

			try {
				// TODO: user class
				WalkNR.main ("files", "hashes");
			} catch (Exception e) {
				System.out.println ("Invocation exception: " + e.toString ());
				continue test_loop;
			}
			
			File result = new File (runDirectory, "hashes");
			if (!result.exists ()) {
				System.out.println ("Illegal state: output file not found");
				continue test_loop;
			}
			
			String line;
			int row = 0;
			
			try (
				InputStream is = new FileInputStream (result);
				Reader r = new InputStreamReader (is, "UTF-8");
				BufferedReader br = new BufferedReader (r);
			) {
				while ((line = br.readLine ()) != null) {
					StringTokenizer st = new StringTokenizer (line);
					String hash = st.nextToken ();
					String path = st.nextToken ();
					
					if (!files.containsKey (path)) {
						System.out.println ("Illegal state: extra file found " + path + " at " + row);
						continue test_loop;
					}
					
					if (!files.get (path).equals (hash)) {
						System.out.println ("Wrong answer: wrong hash " + hash + " at " + row);
						continue test_loop;
					}
					
					files.remove (path);
					row ++;
				}
			} catch (NoSuchElementException nsee) {
				System.out.println ("Illegal state: wrong output at line " + row);
				continue test_loop;
			} catch (IOException ioe) {}
			
			if (files.size () > 5) {
				System.out.println ("Missed file: not found files number " + files.size ());
				continue test_loop;
			} else if (files.size () > 0) {
				System.out.println ("Missed file:");
				for (String path : files.keySet ()) {
					System.out.println ("file " + path + " missed");
				}
				
				continue test_loop;
			}
			
			System.out.println ("OK");
		}
		
		System.out.println ();
		
		long end = System.currentTimeMillis ();
		System.out.println ("Test finished (time: " + (end - start) + " ms)");
	}

	private static void makeTestDir (File parent, int... presets) {
		File testDir = new File (parent, TEST_DIR);
		FSWalker.crawlFS (testDir, d -> true, f -> true, 
							d -> d.delete (), f -> f.delete ());
		testDir.mkdir ();
		generateTree (testDir, presets [0], presets [1], 
						presets [2], presets [3]);
	}
	
	private static void makeInputFile (File parent, int... presets) {
		File testDir = new File (parent, "files");
		testDir.delete ();
		
		try {
			testDir.createNewFile ();
			
			PrintWriter pw = new PrintWriter (testDir);
			pw.println (TEST_DIR);
			
			int fakeFiles = presets [0] * presets [1] + presets [2];
			for (int i = 0; i < fakeFiles; i ++) {
				String name = generateFakeFileName (presets [0]);
				files.put (name, "00000000");
				pw.println (name);
			}
			
			pw.close ();
		} catch (IOException e) {}
	}
	
	private static final Random RANDOM = new Random ();
	
	private static void generateTree (File dir, int depth, int width, int files, int weight) {
		generateFiles (dir, files, weight);
		if (depth == 0) { return; }
		
		for (int i = 0; i < width; i ++) {
			File nextDir = new File (dir, randomString (8));
			nextDir.mkdir ();
			
			generateTree (nextDir, depth - 1, width, files, weight);
		}
	}
	
	private static void generateFiles (File dir, int files, int weight) {
		for (int i = 0; i < files; i ++) {
			generateFile (new File (dir, randomString (16)), weight);
		}
	}
	
	private static String randomString (int length) {
		StringBuilder sb = new StringBuilder ();
		int [][] ranges = new int [][] {
			{'a', 'z' - 'a'}, {'A', 'Z' - 'A'},
			{'0', '9' - '0'}, {'^', '`' - '^'},
			{'ゔ', 'ゞ' - 'ゔ'}, {'а', 'я' - 'а'}, 
			{'А', 'Я' - 'А'}, {'㌀', '㍔' - '㌀'}
		};
		
		Stream.<char []> generate (() -> {
			int [] range = ranges [RANDOM.nextInt (ranges.length)];
			int index = range [0] + RANDOM.nextInt (range [1]);
			return Character.toChars (index);
		}).limit (length).forEach (c -> sb.append (c));
		
		return sb.toString ();
	}
	
	private static final int INITIAL_HASH = 0x811c9dc5;
    private static final int PRIME_NUMBER = 0x01000193;
	
	private static void generateFile (File file, int weight) {
		try {
			file.createNewFile ();
			int hash = INITIAL_HASH;
			
			if (weight > 0) {
				OutputStream os = new FileOutputStream (file);
				byte [] buffer = new byte [1 << 10];
				
				int size = weight / 3 + RANDOM.nextInt (weight / 4);
				for (int i = 0; i < size - buffer.length; i += buffer.length) {
					RANDOM.nextBytes (buffer);
					os.write (buffer);
					
					for (int j = 0; j < buffer.length; j ++) {
						hash = (hash * PRIME_NUMBER) ^ (buffer [j] & 0xff);
					}
				}
				
				int rest = size % buffer.length;
				for (int i = 0; i < rest; i ++) {
					int c = RANDOM.nextInt (256);
					hash = (hash * PRIME_NUMBER) ^ (c & 0xff);
					os.write (c);
				}
				
				os.close ();
			}
			
			files.put (file.getPath ().substring (2), String.format ("%08x", hash));
		} catch (Exception e) {e.printStackTrace ();}
	}
	
	private static String generateFakeFileName (int depth) {
		File tmp = new File (randomString (16));
		for (int i = 0; i < depth; i ++) {
			File folder = new File (randomString (8));
			tmp = new File (folder, tmp.toString ());
		}
		
		return tmp.getPath ();
	}

}
