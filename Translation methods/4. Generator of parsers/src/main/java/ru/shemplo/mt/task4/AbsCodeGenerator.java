package ru.shemplo.mt.task4;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.antlr.v4.runtime.tree.ParseTreeWalker;

import ru.shemplo.mt.task4.antlr.GrammarParser;

public abstract class AbsCodeGenerator implements CodeGenerator {

	protected final GrammarCollector COLLECTOR;
	
	protected final String CLASS_NAME;
	protected final Path FILE;
	
	public AbsCodeGenerator (final GrammarParser parser, final GrammarCollector collector, 
			String destinationDirectory, String className) throws IOException {
		TreeWalkListener listener = new TreeWalkListener (this.COLLECTOR = collector);
		new ParseTreeWalker ().walk (listener, parser.grammar_file ());
		
		Path dir = Paths.get (destinationDirectory);
		Files.createDirectories (dir);
		
		this.CLASS_NAME = className;
		
		FILE = dir.resolve (className + ".java");
		if (!Files.exists (FILE)) {
			Files.createFile (FILE);
		}
	}

}
