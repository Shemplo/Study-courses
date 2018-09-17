package me.shemplo.mathlogic.homework2;

import me.shemplo.mathlogic.homework2.expression.Expression;

public class Axioms {

	private static String [] proposAxiomsList = {
			"a->b->a",
            "(a->b)->(a->b->c)->(a->c)",
            "a->b->a&b",
            "a&b->a",
            "a&b->b",
            "a->a|b",
            "b->a|b",
            "(a->c)->(b->c)->(a|b->c)",
            "(a->b)->(a->!b)->!a",
            "!!a->a"
	};
	
	private static String [] formalAxiomsList = {
			"a=b->a'=b'",
            "(a=b)->(a=c)->(b=c)",
            "a'=b'->a=b",
            "!a'=0",
            "a+b'=(a+b)'",
            "a+0=a",
            "a*0=0",
            "a*b'=a*b+a"
	};
	
	////////////////////////////////////////////////
	
	public static Expression [] proposAxioms = {};
	public static Expression [] formalAxioms = {};
	
	public static void parseAxioms (Parser parser) {
		int index = 0;
		
		proposAxioms = new Expression [proposAxiomsList.length];
		for (String axiom : proposAxiomsList) { proposAxioms [index ++] = parser.parse (axiom); }
		
		//System.out.println ("Parsed proposal axioms:"); /* DEBUG */
		//for (Expression axiom : proposAxioms) { System.out.println (axiom.toString ()); }
		
		index = 0;
		formalAxioms = new Expression [formalAxiomsList.length];
		for (String axiom : formalAxiomsList) { formalAxioms [index ++] = parser.parse (axiom); }
		
		//System.out.println ("\nParsed formal axioms:"); /* DEBUG */
		//for (Expression axiom : formalAxioms) { System.out.println (axiom.toString ()); }
	}
	
}
