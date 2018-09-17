package ru.shemplo.hw.src.implementor;

import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Utils {

	public static final List <String> fetchClasses (String string) {
		List <String> classes = new ArrayList <> ();
		if (string == null || string.length () == 0) {
			return classes;
		}
		
		Pattern classPattern = Pattern.compile ("([\\w]+(\\.[\\w]+)*(\\$[\\w]+)?|\\?)", 
												Pattern.UNICODE_CHARACTER_CLASS);
		Pattern duplication = Pattern.compile ("(.+)\\.\\1.*", 
												Pattern.UNICODE_CHARACTER_CLASS);
		
		Matcher matcher = classPattern.matcher (string);
		while (matcher.find ()) {
			String name = matcher.group (0);
			Matcher md = duplication.matcher (name);
			if (md.find ()) {
				String repeat = md.group (1) + ".";
				name = name.replaceFirst (repeat, "");
			}
			
			classes.add (name);
		}
		
		return classes;
	}
	
	public static final String fetchClass (String string) {
		List <String> classes = fetchClasses (string);
		if (classes.size () == 0) { return null; }
		return classes.get (0);
	}
	
	public static final List <String> fetchParameters (String string) {
		if (string == null) { return new ArrayList <> (); }
		int openIndex = string.indexOf ('<'),
				closeIndex = string.lastIndexOf ('>');
		List <String> params = new ArrayList <> ();
		/*
		if (openIndex == -1) { openIndex = 0; }
		if (closeIndex == -1) {
			closeIndex = string.length () - 1; 
		}
		*/
		if (openIndex == -1 || closeIndex == -1) {
			return params;
		}
		
		String strCorpus = string.substring (openIndex + 1, closeIndex);
		char [] corpus = strCorpus.toCharArray ();
		int balance = 0, start = 0;
		
		for (int i = 0; i < corpus.length; i++) {
			if (corpus [i] == ',' && balance == 0) {
				params.add (strCorpus.substring (start, i).trim ());
				start = i + 1;
			} else if (corpus [i] == '<') { balance++; }
			else if (corpus [i] == '>') { balance--; }
		}
		
		String rest = strCorpus.substring (start);
		if (rest != null && rest.length () > 0) {
			params.add (rest);
		}
		
		return params;
	}
	
	private static final Map <String, Class <?>> PRIMITIVES;
	static {
		PRIMITIVES = new HashMap <> ();
		
		PRIMITIVES.put (boolean.class.getName (), boolean.class);
		PRIMITIVES.put (byte.class.getName (),    byte.class);
		PRIMITIVES.put (char.class.getName (),    char.class);
		PRIMITIVES.put (short.class.getName (),   short.class);
		PRIMITIVES.put (int.class.getName (),     int.class);
		PRIMITIVES.put (float.class.getName (),   float.class);
		PRIMITIVES.put (double.class.getName (),  double.class);
		PRIMITIVES.put (long.class.getName (),    long.class);
		
		PRIMITIVES.put (void.class.getName (),    void.class);
	}
	
	public static final Class <?> getClass (String name) {
		try {
			return Class.forName (name);
		} catch (ClassNotFoundException cnfe) {
			return PRIMITIVES.get (name);
		}
	}
	
	public static final String boundsToString (TypeVariable <?> var) {
		StringBuilder sb = new StringBuilder ();
		Type [] bounds = var.getBounds ();
		
		int current = 0;
		for (Type type : bounds) {
			sb.append (type.getTypeName ());
			if (current < bounds.length - 1) {
				sb.append (" & ");
			}
			
			current ++;
		}
		
		return sb.toString ();
	}
	
}
