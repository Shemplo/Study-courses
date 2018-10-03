package ru.shemplo.mt.task4.dmy;

import static ru.shemplo.mt.task4.GrammarCollector.*;

import java.util.ArrayList;
import java.util.List;

import ru.shemplo.dsau.stuctures.Pair;
import ru.shemplo.dsau.stuctures.Trio;
import ru.shemplo.mt.task4.CodeGenerator.Repeat;
import ru.shemplo.mt.task4.CodeProducer;

public class NonTerminal implements CodeProducer {

	public final String CONTEXT_NAME = "";
	public final String NAME;
	
	private final List <List <Trio <String, Repeat, String>>> 
		BRANCHES = new ArrayList <> ();
	private final List <Trio <String, String, String>>
		VARIABLES = new ArrayList <> ();
	
	public NonTerminal (String name) {
		this.NAME = name;
	}
	
	public void addAlternamtive (List <Trio <String, Repeat, String>> branch) {
		this.BRANCHES.add (branch);
	}
	
	public int getNumberOfBranches () {
		return BRANCHES.size ();
	}
	
	public void addLocalVariable (String type, String name, String initialValue) {
		this.VARIABLES.add (Trio.mt (type, name, initialValue));
	}
	
	public List <List <Trio <String, Repeat, String>>> getBranches () {
		return this.BRANCHES;
	}
	
	public String generateContext (List <Pair <String, String>> methods) {
		return new StringBuilder ()
				 . append ("// generated class\n")
				 . append ("\tprivate class ").append (NAME).append ("_context extends AbsNonTerminalProducer {\n")
				 . append (generateVariables ())
				 . append ("\t\tpublic ").append (NAME).append ("_context () {\n")
				 . append (generateBranches ())
				 . append ("\t\t}\n\n")
				 . append (generateMethods (methods))
				 . append ("\t}\n")
				 . toString ();
	}
	
	private String generateVariables () {
		StringBuilder sb = new StringBuilder ();
		for (int i = 0; i < VARIABLES.size (); i++) {
			Trio <String, String, String> var = VARIABLES.get (i);
			sb.append ("\n\t\tpublic ").append (var.F).append (" ").append (var.S)
			  .append (" = ").append (var.T.length () == 0 ? "\"" + var.T + "\"" : var.T).append (";\n");
		}
		
		return sb.append ("\n").toString ();
	}
	
	private String generateBranches () {
		StringBuilder sb = new StringBuilder ("\t\t\tsuper (");
		for (int i = 0; i < BRANCHES.size (); i++) {
			sb.append ("new ArrayList <> ()");
			if (i < BRANCHES.size () - 1) {
				sb.append (", ");
			}
		}
		sb.append (");\n\n");
		
		for (int i = 0; i < BRANCHES.size (); i++) {
			for (int j = 0; j < BRANCHES.get (i).size (); j++) {
				Trio <String, Repeat, String> temp = BRANCHES.get (i).get (j);
				String varName = "_var" + getNextID ();
				
				String repeat = temp.S == Repeat.SINGLE 
								? "Times.S"
								: temp.S == Repeat.ZERO_OR_ONE
									? "Times.ZOS"
									: temp.S == Repeat.ZERO_OR_MORE
										? "Times.ZOM"
										: temp.S == Repeat.ONE_OR_MORE
											? "Times.SOM"
										    : "";
				
				sb.append ("\t\t\t").append (temp.F).append ("_context ").append (varName)
				  .append (" = new ").append (temp.F).append ("_context ();\n")
				  .append ("\t\t\tBRANCHES [").append (i).append ("]")
				  .append (".add (Trio.mt (").append (varName).append (", ")
				  .append (repeat).append (", \"").append (temp.F).append ("\"));")
				  .append ("\n\n");
			}
		}
		
		return sb
			 . append ("\t\t\treset ();\n")
			 . toString ();
	}
	
	private String generateMethods (List <Pair <String, String>> methods) {
		if (methods == null) { return ""; }
		StringBuilder sb = new StringBuilder ();
		
		sb.append ("\t\tpublic void apply (ParseTreeProducer context) {\n");
		for (int i = 0; i < methods.size (); i++) {
			Pair <String, String> method = methods.get (i);
			String code = method.S.replace ("#", method.F + "_context");
			
			sb.append ("\t\t\tif (context instanceof ").append (method.F).append ("_context) {\n");
			sb.append ("\t\t\t\t").append (code).append (";\n");
			sb.append ("\t\t\t}\n");
		}
		sb.append ("\t\t}\n\n");
		
		if (VARIABLES.size () > 0) {
			sb.append ("\t\t@Override\n");
			sb.append ("\t\tpublic void reset () {\n");
			sb.append ("\t\t\tsuper.reset ();\n");
			for (int i = 0; i < VARIABLES.size (); i++) {
				Trio <String, String, String> var = VARIABLES.get (i);
				sb.append ("\t\t\t").append (var.S).append (" = ")
				  .append (var.T.length () == 0 ? "\"" + var.T + "\"" : var.T)
				  .append (";\n");
			}
			sb.append ("\t\t}\n\n");
		}
		
		return sb.toString ();
	}
	
}
