package me.shemplo.mathlogic.homework2;

import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import me.shemplo.mathlogic.homework2.expression.Expression;
import me.shemplo.mathlogic.homework2.expression.Operations.Any;
import me.shemplo.mathlogic.homework2.expression.Operations.Exist;
import me.shemplo.mathlogic.homework2.expression.Operations.Implication;

public class Deductor {

	private List <Expression> exps, sups = new ArrayList<> ();
	private List <String> inds, head = new ArrayList<> ();

	private Parser parser = new Parser ();

	public void deduct (String first, List <String> proof, Matcher matcher, PrintWriter pw) {
		exps = new ArrayList<> ();
		inds = new ArrayList<> ();

		parseSups (first);
		String leftImplication = head.remove (head.size () - 1);

		for (String line : proof) {
			Expression exp = parser.parse (line);
			Expression a = parser.parse (leftImplication);
			Expression tmp = parser.parse (line);

			if (matcher.deepEqual (exp, a)) {
				exps.add (tmp);
				inds.add (line);
				String mask = "(" + leftImplication + ")->(" + leftImplication + ")->(" + leftImplication + ")";
				pw.println (mask);

				pw.println ("(" + mask + ")->((" + leftImplication + ")->(((" + leftImplication + ")->("
						+ leftImplication + "))->(" + leftImplication + ")))->((" + leftImplication + ")->("
						+ leftImplication + "))");
				pw.println ("((" + leftImplication + ")->(((" + leftImplication + ")->(" + leftImplication + "))->("
						+ leftImplication + ")))->((" + leftImplication + ")->(" + leftImplication + "))");
				pw.println ("((" + leftImplication + ")->(((" + leftImplication + ")->(" + leftImplication + "))->("
						+ leftImplication + ")))");
				pw.println ("(" + leftImplication + ")->(" + leftImplication + ")");
				continue;
			}

			boolean flag = false;
			for (Expression sup : sups) {
				if (sups.get (0) != null && matcher.deepEqual (sup, exp)) {
					pw.println (line);
					exps.add (sup);
					inds.add (line);

					pw.println ("(" + line + ")->(" + leftImplication + ")->(" + line + ")");
					pw.println ("(" + leftImplication + ")->(" + line + ")");
					flag = true;
					break;
				}
			}

			if (flag) {
				continue;
			}

			int axiom = matcher.matchProposalAxiom (exp, Axioms.proposAxioms);
			axiom = axiom > 0 ? axiom : matcher.matchFormalAxiom (exp, Axioms.formalAxioms);
			if (axiom > 0 || matcher.finalCheck (exp)) {
				exps.add (tmp);
				inds.add (line);
				pw.println (line);

				pw.println ("(" + line + ")->(" + leftImplication + ")->(" + line + ")");
				pw.println ("(" + leftImplication + ")->(" + line + ")");
				continue;
			}

			if (matcher.matchAny (exp, exps, sups) >= 0) {
				List <String> args = new ArrayList<> ();
				Implication iexp = (Implication) exp;

				args.add (leftImplication);
				args.add (iexp.left.toString ());
				args.add (((Any) iexp.right).expression.toString ());
				args.add (((Any) iexp.right).variable.toString ());
				List <String> proofs = Support.replace ("quantors/any.proof", args);

				exps.add (tmp);
				inds.add (line);

				proofs.stream ().forEach (x -> pw.println (x));
				continue;
			}
			exp = tmp.getInstance ();

			if (matcher.matchExist (exp, exps, sups) >= 0) {
				ArrayList <String> args = new ArrayList<> ();
				Implication iexp = (Implication) exp;

				args.add (leftImplication);
				args.add (((Exist) iexp.left).expression.toString ());
				args.add ((iexp.right).toString ());
				args.add (((Exist) iexp.left).variable.toString ());
				List <String> proofs = Support.replace ("quantors/exist.proof", args);

				exps.add (tmp);
				inds.add (line);

				proofs.stream ().forEach (x -> pw.println (x));
				continue;
			}
			exp = tmp.getInstance ();

			int [] mp = checkMP (exp, matcher);
			if (mp [0] > -1 && mp [1] > -1) {
				exps.add (tmp);
				inds.add (line);
				pw.println ("((" + leftImplication + ")->(" + inds.get (mp [0]) + "))->(((" 
								+ leftImplication + ")->((" + inds.get (mp [0])
								+ ")->(" + line + ")))->((" + leftImplication + ")->(" + line + ")))");
				pw.println ("(((" + leftImplication + ")->((" + inds.get (mp [0]) + ")->(" 
								+ line + ")))->((" + leftImplication + ")->("
								+ line + ")))");
				pw.println ("(" + leftImplication + ")->(" + line + ")");
				continue;
			}

			exps.add (tmp);
			inds.add (line);
			pw.println (line);
		}
	}

	private int [] checkMP (Expression e, Matcher matcher) {
		int indTo = -1, indFrom = -1;
		for (int i = exps.size () - 1; i >= 0; i --) {
			Expression e1 = exps.get (i);
			if (e1 instanceof Implication && matcher.deepEqual (e, ((Implication) e1).right)) {
				for (int j = exps.size () - 1; j >= 0; j --) {
					if (matcher.deepEqual (((Implication) e1).left, exps.get (j))) {
						indFrom = i;
						indTo = j;
						break;
					}
				}

				if (indTo > -1)
					break;
			}
		}

		return new int [] {indTo, indFrom};
	}

	private Map <String, Integer> parseSups (String str) {
		Map <String, Integer> map = new HashMap<> ();
		int index = 0, number = 0, balance = 0;
		String cur = "";

		while (index < str.length ()) {
			while (str.charAt (index) == ' ')
				index ++;
			while (index < str.length () && (str.charAt (index) != ',' || balance != 0) && index < str.length ()
					&& str.charAt (index) != '|') {
				if (str.charAt (index) != ' ') {
					cur += str.charAt (index);
				}
				if (str.charAt (index) == '(') {
					balance ++;
				}
				if (str.charAt (index) == ')') {
					balance --;
				}
				index ++;
			}

			number ++;
			map.put (cur, number);
			head.add (cur);
			sups.add (parser.parse (cur));

			cur = "";
			if (index < str.length () && str.charAt (index) == ',') {
				index ++;
			}
			while (index < str.length () && str.charAt (index) == ' ') {
				index ++;
			}
			if (index < str.length () && str.charAt (index) == '|') {
				index += 2;

				while (str.charAt (index) == ' ') {
					index ++;
				}
				
				index = str.length ();
			}
		}

		return map;
	}

}
