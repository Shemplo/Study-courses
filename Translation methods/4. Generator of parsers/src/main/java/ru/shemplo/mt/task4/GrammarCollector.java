package ru.shemplo.mt.task4;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;

import ru.shemplo.dsau.stuctures.Pair;
import ru.shemplo.dsau.stuctures.Trio;
import ru.shemplo.mt.task4.CodeGenerator.Repeat;
import ru.shemplo.mt.task4.dmy.NonTerminal;
import ru.shemplo.mt.task4.dmy.Terminal;

public class GrammarCollector {
	
	private static int branches = 0;
	
	public static int getNextID () {
		return branches++;
	}
	
	////////////////////////////////
	
	private String topNonTerminal = "";
	
	private final Map <String, Terminal> 
		TERMINALS = new HashMap <> ();
	
	private final Map <String, NonTerminal>
		NON_TERMINALS = new HashMap <> ();
	
	private final Map <String, List <Pair <String, String>>>
		METHODS = new HashMap<> ();

	public void addTerminal (Terminal terminal) {
		if (terminal == null) { return; }
		
		TERMINALS.put (terminal.NAME, terminal);
	}
	
	public Terminal getTerminal (String name) {
		return TERMINALS.get (name);
	}
	
	public void removeTerminal (Terminal terminal) {
		TERMINALS.remove (terminal.NAME);
	}
	
	public void addNonTerminal (NonTerminal nonTerminal) {
		if (nonTerminal == null) { return; }
		
		if (topNonTerminal.length () == 0) {
			topNonTerminal = nonTerminal.NAME;
		}
		
		NON_TERMINALS.put (nonTerminal.NAME, nonTerminal);
	}
	
	public NonTerminal getNonTerminal (String name) {
		return NON_TERMINALS.get (name);
	}
	
	public String getTopNonTerminal () {
		return topNonTerminal;
	}
	
	private void checkLeftRecursion (NonTerminal nonTerminal) {
		Queue <List <Trio <String, Repeat, String>>> 
			queue = new LinkedList <> (nonTerminal.getBranches ());
		Set <String> checked = new HashSet <> ();
		
		while (!queue.isEmpty ()) {
			List <Trio <String, Repeat, String>> terms = queue.poll ();
			for (int j = 0; j < terms.size (); j++) {
				Trio <String, Repeat, String> term = terms.get (j);
				if (term.F.equals (nonTerminal.NAME)) {
					throw new IllegalStateException ("Left requrson for " + nonTerminal.NAME);
				} else if (NON_TERMINALS.containsKey (term.F) && !checked.contains (term.F)) {
					queue.addAll (NON_TERMINALS.get (term.F).getBranches ());
					checked.add (term.F);
				}
			}
		}
	}
	
	public String generateContexts () {
		for (NonTerminal nonTerminal : NON_TERMINALS.values ()) {
			checkLeftRecursion (nonTerminal);
		}

		METHODS.clear ();
		
		for (NonTerminal nonTerminal : NON_TERMINALS.values ()) {
			List <List <Trio <String, Repeat, String>>> branches = nonTerminal.getBranches ();
			for (int i = 0; i < branches.size (); i++) {
				for (int j = 0; j < branches.get (i).size (); j++) {
					Trio <String, Repeat, String> term = branches.get (i).get (j);
					METHODS.putIfAbsent (term.F, new ArrayList <> ());
					
					String owner = nonTerminal.NAME;
					if (term.T.length () > 0) {
						METHODS.get (term.F).add (Pair.mp (owner, term.T));
					}
				}
			}
		}
		
		StringBuilder sb = new StringBuilder ();
		for (Terminal terminal : TERMINALS.values ()) {
			List <Pair <String, String>> methods = METHODS.get (terminal.NAME);
			sb.append (terminal.generateContext (methods)).append ("\n\t");
		}
		
		for (NonTerminal nonTerminal : NON_TERMINALS.values ()) {
			List <Pair <String, String>> methods = METHODS.get (nonTerminal.NAME);
			sb.append (nonTerminal.generateContext (methods)).append ("\n\t");
		}
		
		return sb.toString ();
	}
	
}
