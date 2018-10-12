package ru.shemplo.shop;

import java.util.StringTokenizer;

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
				
				StringTokenizer st = new StringTokenizer (line);
				System.setProperty (st.nextToken (), st.nextToken ());
			}
		} catch (IOException ioe) {
			ioe.printStackTrace ();
			System.exit (1);
		}
	}
	
}
