package me.shemplo.parser;

import com.sun.org.apache.xerces.internal.impl.xpath.regex.ParseException;

public class Parser {

	private static String processingString;
	private static Token processingToken;
	private static char processingChar;
	private static int offset;
	
	public static Node parse (String input) {
		processingString = input + "$";
		processingToken = null;
		processingChar = 0;
		offset = 0;
		
		nextToken ();
		return fetchEnterPoint ();
	}
	
	private static boolean isBlank (char symbol) {
		return Character.isWhitespace (symbol);
	}
	
	private static boolean isControl (char symbol) {
		return symbol == ',' || symbol == ';'
				|| symbol == '[' || symbol == ']';
	}
	
	private static boolean hasMore () {
		return offset < processingString.length ();
	}
	
	///////////////////////////////////////////////
	
	private static void nextChar () {
		processingChar = processingString.charAt (offset ++);
	}
	
	private enum Token {
		ASTERISK,      // *
		COMMA,         // ,
		SEMICOLON,     // ;
		NAME,          // a-z
		LEFT_BRACKET,  // [
		RIGHT_BRACKET, // ]
		END            // $
	}
	
	private static void nextToken () {
		while (hasMore () && isBlank (processingChar)) {
			nextChar (); // Skipping blank spaces
		}
		
		if (isBlank (processingChar)) {
			throw new ParseException ("End of string reached at ", offset);
		}
		
		switch (processingChar) {
			case '*':
				processingToken = Token.ASTERISK;
				nextChar ();
				break;
			case ',':
				processingToken = Token.COMMA;
				nextChar ();
				break;
			case ';':
				processingToken = Token.SEMICOLON;
				nextChar ();
				break;
			case '$':
				processingToken = Token.END;
				break;
			case '[':
				processingToken = Token.LEFT_BRACKET;
				nextChar ();
				break;
			case ']':
				processingToken = Token.RIGHT_BRACKET;
				nextChar ();
				break;
			
			default:
				while (hasMore () 
						&& !isBlank (processingChar)
						&& !isControl (processingChar)) {
					nextChar ();
				}
				
				processingToken = Token.NAME;
		}
	}
	
	///////////////////////////////////////////////////////////////////////
	
	private static Node fetchEnterPoint () {
		String method = "Enter point (S)";
		switch (processingToken) {
			case NAME:
				return new Node (method, fetchVariableDeclaration ());
			case END:
				return new Node (method, new Node ("$"));
				
			default:
				throw new ParseException ("Enter point not found at ", offset);
		}
	}
	
	private static Node fetchVariableDeclaration () {
		String method = "Variable declaration (D)";
		switch (processingToken) {
			case NAME:
				Node type = fetchVariableType ();
				Node skip = fetchMassive ();
				return new Node (method, type, skip, fetchVariableDeclaration ());
			case END:
				return new Node (method, new Node ("$"));
				
			default:
				throw new ParseException ("Variable declaration not found at ", offset);
		}
	}
	
	private static Node fetchVariableType () {
		String method = "Variable type (T)";
		switch (processingToken) {
			case NAME:
				nextToken ();
				return new Node (method, new Node ("a"));
				
			default:
				throw new ParseException ("Variable type not found at ", offset);
		}
	}
	
	private static Node fetchMassive () {
		String method = "Variable massive type (M)";
		switch (processingToken) {
			case LEFT_BRACKET:
				nextToken ();
				if (processingToken != Token.RIGHT_BRACKET) {
					throw new ParseException ("Close of massive not found at ", offset);
				}
				
				nextToken ();
				return new Node (method, fetchMassive ());
				
			default:
				return fetchVariableSkiper ();
		}
	}
	
	private static Node fetchVariableSkiper () {
		String method = "Variable skiper (K)";
		switch (processingToken) {
			case ASTERISK:
			case NAME:
				return new Node (method, fetchVariableEnterance ());
				
			default:
				throw new ParseException ("Variable skiper not found at ", offset);
		}
	}
	
	private static Node fetchVariableEnterance () {
		String method = "Variable enterance (F)";
		switch (processingToken) {
			case ASTERISK:
			case NAME:
				Node variable = fetchVariable ();
				Node next = fetchNextVariable ();
				return new Node (method, variable, next);
				
			default:
				throw new ParseException ("Variable enterance not found at ", offset);
		}
	}
	
	private static Node fetchVariable () {
		String method = "Variable (V)";
		switch (processingToken) {
			case ASTERISK:
				nextToken ();
				Node variable = fetchVariable ();
				return new Node (method, new Node ("*"), variable);
			case NAME:
				nextToken ();
				return new Node (method, new Node ("a"));
				
			default:
				throw new ParseException ("Variable not found at ", offset);
		}
	}
	
	private static Node fetchNextVariable () {
		String method = "Next variable (N)";
		switch (processingToken) {
			case COMMA:
				nextToken ();
				Node variable = fetchVariable ();
				Node next = fetchNextVariable ();
				return new Node (method, new Node (","), variable, next);
			case SEMICOLON:
				nextToken ();
				return new Node (method);
				
			default:
				throw new ParseException ("Variable not found at ", offset);
		}
	}
	
}
