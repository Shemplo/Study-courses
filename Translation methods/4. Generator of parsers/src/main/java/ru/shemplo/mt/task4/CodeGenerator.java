package ru.shemplo.mt.task4;

import java.io.IOException;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.TokenStream;

import ru.shemplo.mt.task4.antlr.GrammarLexer;
import ru.shemplo.mt.task4.antlr.GrammarParser;

public interface CodeGenerator {

	public static GrammarParser getParser (final String fileName) {
		try {
			CharStream cs = CharStreams.fromFileName (fileName);
			GrammarLexer lexer = new GrammarLexer (cs);
			
			TokenStream ts = new CommonTokenStream (lexer);
			return new GrammarParser (ts);
		} catch (IOException ioe) {
			System.err.println ("Exception occured:");
			System.err.println (ioe.toString ());
		}
		
		return null;
	}
	
	public static String replaceSnippets (String input) {
		if (input == null) { return ""; }
		
		return input.replaceAll ("\\$empty", "")
			 . replaceAll ("\\$s", " ")
			 . replaceAll ("\\$space", " ")
			 . replace ("$dq", "\"");
	}
	
	public static enum Repeat {
		SINGLE, ZERO_OR_ONE, ZERO_OR_MORE, ONE_OR_MORE
	}
	
	public void generate () throws IOException;
	
}
