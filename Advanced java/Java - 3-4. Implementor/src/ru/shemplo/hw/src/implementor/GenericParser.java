package ru.shemplo.hw.src.implementor;

import java.util.ArrayList;
import java.util.List;

import javafx.util.Pair;

public class GenericParser {
	
	private static List <String> TOKENIZED_STRING;
	private static int INDEX;
	
	public static final Generic parseGeneric (String string) {
		TOKENIZED_STRING = splitOnTokens (string); INDEX = 0;
		//System.out.println (TOKENIZED_STRING);
		return parseGeneric ();
	}
	
	public static final Pair <String, List <String>> simplifyString (String string) {
		List <String> tokens = Utils.fetchClasses (string);
		List <String> imports = new ArrayList <> ();
		
		for (int i = 0; i < tokens.size () - 1; i++) {
			String token = tokens.get (i);
			
			Class <?> type = Utils.getClass (token);
			if (type == null) { continue; }
			
			string = string.replace (token, type.getSimpleName ());
			imports.add (type.getName ());
		}
		
		return new Pair <String, List <String>> (string, imports);
	}
	
	private static final Generic parseGeneric () {
		String current = TOKENIZED_STRING.get (INDEX);
		
		if (current.equals ("extends")) {INDEX++; return parseTuple ("extends", '&'); }
		if (current.equals ("super")) {INDEX++; return parseTuple ("super", '&'); }
		if (current.equals ("<")) {INDEX++; return parseTuple ("tuple", ','); }
		
		String className = Utils.fetchClass (current);
		if (className != null) {
			String next = TOKENIZED_STRING.get (++INDEX);
			if (next.equals ("<") || next.equals ("extends") 
					|| next.equals ("super")) {
				Generic generic = parseGeneric ();
				
				current = TOKENIZED_STRING.get (INDEX);
				return new Generic (className, generic);
			}
			
			return new Generic (className);
		}
		
		return null;
	}
	
	private static final Generic parseTuple (String word, char stop) {
		List <Generic> generics = new ArrayList <> ();
		String current = TOKENIZED_STRING.get (INDEX);
		final String s = "" + stop;
		
		do {
			generics.add (parseGeneric ());
			current = TOKENIZED_STRING.get (INDEX);
			if (!current.equals (s)) { break; }
			INDEX++;
		} while (true);
		
		if (current.equals (">")) {INDEX++;}
		return new Generic (word, stop, generics);
	}
	
	private static final List <String> splitOnTokens (String string) {
		List <String> tokens = new ArrayList <> ();
		char [] chars = string.toCharArray ();
		
		StringBuilder buffer = new StringBuilder ();
		for (int i = 0; i < chars.length; i ++) {
			if (isStopCharacter (chars [i])) {
				if (buffer.length () > 0) {
					tokens.add (buffer.toString ());
					buffer = new StringBuilder ();
				}
				
				if (!Character.isWhitespace (chars [i])) {
					tokens.add ("" +chars [i]);	
				}
			} else {
				buffer.append (chars [i]);
			}
		}
		
		if (buffer.length () > 0) {
			tokens.add (buffer.toString ());
		}
		
		tokens.add ("#");
		return tokens;
	}
	
	private static final boolean isStopCharacter (char c) {
		if (Character.isWhitespace (c)) { return true; }
		if (c == '<' || c == '>') { return true; }
		if (c == '&' || c == ',') { return true; }
		if (c == '?') { return true; }
		return false;
	}
	
}
