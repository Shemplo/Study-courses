package ru.shemplo.tasks;

import java.util.Arrays;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

public class PropertiesLoader {

	public static void load (String path) {
		try (
			BufferedReader br = Files.newBufferedReader (Paths.get (path));
		) {
			String line;
			while ((line = br.readLine ()) != null) {
				if (line.length () == 0) { continue; }
				
				String [] parts = line.split ("=", 2);
                if (parts.length == 2) {
                    Arrays.asList (parts).forEach (String::trim);
                    System.setProperty (parts [0], parts [1]);
                }
			}
		} catch (IOException ioe) {
			ioe.printStackTrace ();
			System.exit (1);
		}
	}
	
}
