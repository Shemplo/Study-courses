package me.shemplo.mathlogic.homework3;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Compressor {

	private static final String INPUT = "support/template.proof";
	private static final String OUTPUT = "template2.proof";
	
	private static final char [] ALPHABET = {
		'a', 'b', 'c', 'd', 'e', 'f',
		'g', 'h', 'i', 'j', 'k', 'l',
		'm', 'n', 'o', 'p', 'q', 'r',
		's', 't', 'u', 'v', 'w', 'x',
		'y', 'z'
	};
	
	private static BufferedReader br;
	private static PrintWriter pw;
	
	public static void main (String [] args) throws Exception {
		File input = new File (INPUT);
		File output = new File (OUTPUT);
		br = new BufferedReader (
				new InputStreamReader (
					new FileInputStream (input), "UTF-8"));
		pw = new PrintWriter (
				new OutputStreamWriter (
					new FileOutputStream (output), "UTF-8"));
		/////////////////////////////////////////////////////
		System.out.println ("[LOG] Reading file `" + INPUT + "`");
		
		String line = "tmp stub";
		List <String> lines = new ArrayList <> ();
		while ((line = br.readLine ()) != null) { lines.add (line); }
		System.out.println ("[LOG] Read " + lines.size () + " lines");
		
		String regexp = "[rstxyz]{1}[0-9]{8,10}";
		Pattern pattern = Pattern.compile (regexp);
		
		Map <String, String> vars = new HashMap <> ();
		lines.stream ().forEach (s -> {
			Matcher matcher = pattern.matcher (s);
			while (matcher.find ()) {
				String tmp = s.substring (matcher.start (), matcher.end ());
				if (!vars.containsKey (tmp)) { vars.put (tmp, nextVarName ()); }
			}
		});
		System.out.println ("[LOG] Matched " + vars.size () + " variables");
		
		lines.stream ().forEach (s -> {
			Matcher matcher = pattern.matcher (s);
			while (matcher.find ()) {
				String tmp = s.substring (matcher.start (), matcher.end ());
				s = s.replaceAll (tmp, vars.get (tmp));
				matcher = pattern.matcher (s);
			}
			
			pw.println (s);
			pw.flush ();
		});
		
		System.out.println ("[LOG] Compressing done");
	}
	
	private static int index = 0;
	
	private static String nextVarName () {
		int length = ALPHABET.length;
		String out = ALPHABET [index % length] + "" + (index / length + 10);
		
		index ++;
		return out;
	}
	
}
