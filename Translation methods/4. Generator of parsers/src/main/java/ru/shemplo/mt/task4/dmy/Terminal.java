package ru.shemplo.mt.task4.dmy;

import java.util.List;

import ru.shemplo.dsau.stuctures.Pair;
import ru.shemplo.mt.task4.CodeProducer;

public class Terminal implements CodeProducer {

	public final String NAME, PATTERN;
	public final boolean IS_REGEXP;
	
	public Terminal (String name, String pattern, boolean isRegexp) {
		this.PATTERN = "\"" + (pattern.equals ("\"") ? "\\\"" : pattern) + "\"";
		this.NAME = name; this.IS_REGEXP = isRegexp;
	}
	
	public String generateContext (List <Pair <String, String>> methods) {
		return new StringBuilder ()
				 . append ("// generated class\n")
				 . append ("\tprivate class ").append (NAME).append ("_context extends AbsTerminalProducer {\n")
				 //. append (generateNodes ())
				 . append ("\n\t\tpublic ").append (NAME).append ("_context () {\n")
				 . append ("\t\t\tsuper (").append (PATTERN).append (", ")
				 . append (IS_REGEXP).append (");\n")
				 . append ("\t\t}\n\n")
				 . append (generateMethods (methods))
				 . append ("\t}\n")
				 . toString ();
	}
	
	@SuppressWarnings ("unused")
	private String generateNodes () {
		return new StringBuilder ()
			 . append ("\n\t\tpublic class ").append (NAME).append ("_node").append (" extends TreeNode {\n")
			 . append ("\n\t\t\tpublic ").append (NAME).append ("_node (String value) {\n")
			 . append ("\t\t\t\tsuper (value);\n")
			 . append ("\t\t\t}\n\n")
			 . append ("\t\t}\n")
			 . toString ();
	}
	
	private String generateMethods (List <Pair <String, String>> methods) {
		if (methods == null) { return ""; }
		StringBuilder sb = new StringBuilder ();
		
		sb.append ("\t\tpublic void apply (TreeNode node, ")
		  .append("ParseTreeProducer context) {\n");
		for (int i = 0; i < methods.size (); i++) {
			Pair <String, String> method = methods.get (i);
			String code = method.S.replace ("#", method.F + "_context");
			
			if (i == 0) {
				sb.append ("\t\t\tif (context instanceof ").append (method.F).append ("_context) {\n");
				sb.append ("\t\t\t\t").append (code).append (";\n");
				sb.append ("\t\t\t}\n");
			}
		}
		sb.append ("\t\t}\n\n");
		
		return sb.toString ();
	}
	
}
