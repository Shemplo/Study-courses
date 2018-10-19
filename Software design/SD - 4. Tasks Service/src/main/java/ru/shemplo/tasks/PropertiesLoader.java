package ru.shemplo.tasks;

import java.util.Arrays;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class PropertiesLoader {

    public static void load (InputStream is) throws IOException {
        try (
            Reader r = new InputStreamReader (is, StandardCharsets.UTF_8);
            BufferedReader br = new BufferedReader (r);
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
        }
    }
    
	public static void load (String stringPath) throws IOException {
	    Path path = Paths.get (stringPath);
		try (
		    InputStream is = Files.newInputStream (path);
		) {
			load (is);
		}
	}
	
}
