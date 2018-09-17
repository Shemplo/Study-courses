package me.shemplo.mathlogic.homework2;

import static me.shemplo.mathlogic.homework2.Axioms.parseAxioms;
import static me.shemplo.mathlogic.homework2.Support.*;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javafx.util.Pair;
import me.shemplo.mathlogic.homework2.expression.Expression;

public class Main {

	private static File input = new File ("proof.in");
	private static BufferedReader br = null;

	private static File outputC = new File ("proof.out");
	private static File outputP = new File ("deduction.out");
	private static PrintWriter pwC = null, pwP = null;
	private static int lineNumber = 1;

	private static List <String> lines = new ArrayList<> ();
	private static List <Expression> expressions = new ArrayList<> ();

	private static Matcher matcher;
	private static Parser parser;

	public static void main (String [] args) throws Exception {
		br = new BufferedReader (new InputStreamReader (new FileInputStream (input), "UTF-8"));
		pwC = new PrintWriter (new OutputStreamWriter (new FileOutputStream (outputC), "UTF-8"));
		pwP = new PrintWriter (new OutputStreamWriter (new FileOutputStream (outputP), "UTF-8"));

		matcher = new Matcher ();
		parser = new Parser ();
		parseAxioms (parser);
		
		String line = br.readLine ();
		checkProof (line);
		
		if (line.indexOf ("|-") != -1 && !supposes.isEmpty ()) {
			makeDeduction (line);
		}

		br.close ();
		pwC.close ();
		pwP.close ();
	}
	
	private static void makeDeduction (String first) {
		Deductor deductor = new Deductor ();
		deductor.deduct (first, lines, matcher, pwP);
	}
	
	private static void checkProof (String firstLine) throws Exception {
		firstLine = firstLine.replaceAll ("\\s", "");
		Map <String, Integer> sups = getSuppositions (firstLine, parser);
		
		// If it's just a proof
		if (firstLine.indexOf ("|-") == -1) {
			Expression exp = parser.parse (firstLine);
			Expression back = parser.parse (firstLine);
			
			int axiom = matcher.matchProposalAxiom (exp, Axioms.proposAxioms);
			if (axiom > 0) {
				pwC.println ("[" + (lineNumber ++) + "] " + firstLine + " (Сх. акс. " + axiom + ")");
				expressions.add (back.getInstance ());
			}
			
			axiom = matcher.matchFormalAxiom (exp, Axioms.formalAxioms);
			if (axiom > 0) {
				int offset = Axioms.proposAxioms.length + 2 + axiom;
				pwC.println ("[" + (lineNumber ++) + "] " + firstLine + " (Сх. акс. " + offset + ")");
				expressions.add (exp);
			}
			
			if (matcher.finalCheck (exp)) {
				pwC.println ("[" + (lineNumber ++) + "] " + firstLine + " (Сх. акс. #)");
				expressions.add (exp);
			}
		}
		
		String str = br.readLine ();
		while (str != null) {
			str = str.replaceAll ("\\s", "");
			lines.add (str);
			
			//System.out.println ("Input: " + str);
			
			if (sups.containsKey (str)) {
				int index = sups.get (str);
				pwC.println ("[" + (lineNumber ++) + "] " + str + " (Предп. " + (index + 1) + ")");
				expressions.add (supposes.get (index));
				str = br.readLine ();
				continue;
			}
			
			Expression exp = parser.parse (str);
			//System.out.println (exp);
			matcher.change = false;
			
			boolean flag = false;
			for (int i = 0; i < supposes.size (); i ++) {
				if (supposes.get (0) != null
							&& matcher.deepEqual (supposes.get (i), exp)) {
					pwC.println ("[" + (lineNumber ++) + "] " + str + " (Предп. " + (i + 1) + ")");
					expressions.add (supposes.get (i));
					flag = true;
					break;
				}
			}
			if (flag) { str = br.readLine (); continue; }
			Expression back = parser.parse (str);
			
			int axiom = matcher.matchProposalAxiom (exp, Axioms.proposAxioms);
			if (axiom > 0) {
				pwC.println ("[" + (lineNumber ++) + "] " + str + " (Сх. акс. " + axiom + ")");
				expressions.add (back.getInstance ());
				str = br.readLine ();
				continue;
			}
			
			axiom = matcher.matchFormalAxiom (exp, Axioms.formalAxioms);
			if (axiom > 0) {
				int offset = Axioms.proposAxioms.length + 2 + axiom;
				pwC.println ("[" + (lineNumber ++) + "] " + str + " (Сх. акс. " + offset + ")");
				expressions.add (exp);
				str = br.readLine ();
				continue;
			}
			
			if (matcher.finalCheck (exp)) {
				pwC.println ("[" + (lineNumber ++) + "] " + str + " (Сх. акс. #)");
				expressions.add (exp);
				str = br.readLine ();
				continue;
			}
			
			matcher.any = false;
			matcher.anySub = false;
			if (matcher.matchAny (exp, expressions, supposes) > -1) {
				pwC.println ("[" + (lineNumber ++) + "] " + str + " (Введение квантора всеобщности)");
				expressions.add (exp);
				str = br.readLine ();
				continue;
			}
			exp = back.getInstance ();
			
			matcher.ext = false;
			matcher.extSub = false;
			//System.out.println (exp);
			int ind = matcher.matchExist (exp, expressions, supposes);
			//System.out.println (lineNumber + " Exist: " + ind);
			if (ind > -1) {
				pwC.println ("[" + (lineNumber ++) + "] " + str + " (Введение квантора существования)");
				expressions.add (exp);
				str = br.readLine ();
				continue;
			}
			//System.out.println ("Not exist");
			exp = back.getInstance ();
			
			Pair <Integer, Integer> mp = checkMP (exp, expressions, matcher);
			if (mp.getKey () > -1 && mp.getValue () > -1) {
				int f = mp.getKey () + 1, s = mp.getValue () + 1;
				pwC.println ("[" + (lineNumber ++) + "] " + str + " (M.P. " + f + ", " + s +")");
				expressions.add (exp);
				str = br.readLine ();
				continue;
			}
			
			pwC.print ("[" + (lineNumber ++) + "] " + str + " Вывод некорректен, начиная с формулы № " + (lineNumber - 1));
			checkForSpecial (exp, str, matcher, pwC);
			return;
		}
	}

}
