package ru.shemplo.mt.task4;

import ru.shemplo.mt.task4.antlr.GrammarParser;

public class Run {

	public static void main (String... args) throws Exception {
		//GrammarParser parser = CodeGenerator.getParser ("pascal2c.gr");
		GrammarParser parser = CodeGenerator.getParser ("declaration.gr");
		//new DefaultCodeGenerator (parser, "src/test/java/", "Pascal2c").generate ();
		new DefaultCodeGenerator (parser, "src/test/java/", "CVariables").generate ();
	}
	
}
