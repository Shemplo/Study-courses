package ru.shemplo.mt.task4;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import ru.shemplo.mt.task4.antlr.GrammarParser;

public class DefaultCodeGenerator extends AbsCodeGenerator {

	public DefaultCodeGenerator (GrammarParser parser, String destFile, String className) throws IOException {
		super (parser, new GrammarCollector (), destFile, className);
	}

	@Override
	public void generate () throws IOException {
		Path dummyFile = Paths.get ("src/main/resources/template.dmy");
		try (
			BufferedReader br = Files.newBufferedReader (dummyFile, 
										StandardCharsets.UTF_8);
			OutputStream os = Files.newOutputStream (FILE);
			Writer w = new OutputStreamWriter (os, 
							StandardCharsets.UTF_8);
			PrintWriter pw = new PrintWriter (w);
		) {
			String line = "";
			while ((line = br.readLine ()) != null) {
				line = line.replace ("%CLASS_PACKAGE%", "")
					 . replaceAll ("%CLASS_NAME%", CLASS_NAME)
					 . replaceAll ("%CONSUMER%", COLLECTOR.getTopNonTerminal () + "_context");
				
				if (line.contains ("%CLASS_CONTEXTS%")) {
					line = line.replace ("%CLASS_CONTEXTS%", "");
					pw.println (COLLECTOR.generateContexts ());
				}
				
				pw.println (line);
			}
			
			pw.flush ();
		}
	}

}
