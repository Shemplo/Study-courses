

import java.io.PrintWriter;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;

public class Main {

	public static void main (String... args) throws Exception {
		CharStream input = CharStreams.fromFileName ("program.pas");
		CaseChangingCharStream lower = new CaseChangingCharStream (input, false);
		pascalLexer lexer = new pascalLexer (lower);
		CommonTokenStream tokens = new CommonTokenStream (lexer);
		pascalParser parser = new pascalParser (tokens);
		parser.setBuildParseTree (false);
		//System.out.println (parser.program ().code);
		System.out.println (">> File parsed <<");
		
		PrintWriter pw = new PrintWriter ("program.c");
		pw.write (parser.program ().code);
		pw.close ();
	}
	
}
