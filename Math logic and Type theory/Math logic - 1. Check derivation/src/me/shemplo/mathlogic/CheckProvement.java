package me.shemplo.mathlogic;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashMap;

public class CheckProvement {

	private static BufferedReader br = null;
	private static PrintWriter    pw = null;
	
	private static ArrayList <String> linesInput;
	private static String [] hypsInput;
	private static int hypsNumber = 0;
	
	private static String [] lines;
	private static boolean [] proved;
	
	private static HashMap <String, String> vars = new HashMap <> ();
	private static HashMap <String, Integer> mpNeed;
	
	public static void main (String [] args) throws FileNotFoundException, IOException {
		br = new BufferedReader (new FileReader ("input.txt"));
		pw = new PrintWriter    (new FileWriter ("output.txt"));
		/*= TIMER =*/ long timeStart = System.currentTimeMillis ();
		
		Node [] axioms = new Node [] {
		/* 0 */ generateTree (generateStack (compressString ("A->B->A"))),
		/* 1 */	generateTree (generateStack (compressString ("(A->B)->(A->B->C)->(A->C)"))),
		/* 2 */	generateTree (generateStack (compressString ("A->B->A&B"))),
		/* 3 */	generateTree (generateStack (compressString ("A&B->A"))),
		/* 4 */	generateTree (generateStack (compressString ("A&B->B"))),
		/* 5 */	generateTree (generateStack (compressString ("A->A|B"))),
		/* 6 */	generateTree (generateStack (compressString ("B->A|B"))),
		/* 7 */	generateTree (generateStack (compressString ("(A->C)->(B->C)->(A|B->C)"))),
		/* 8 */	generateTree (generateStack (compressString ("(A-B)->(A->!B)->!A"))),
		/* 9 */	generateTree (generateStack (compressString ("!!A->A")))
		};
		
		linesInput = new ArrayList <> ();
		mpNeed     = new HashMap   <> ();
		
		String first = br.readLine ();
		int split = first.indexOf ("|-");
		
		String hypsStr = clearString (first.substring (0, split));
		hypsInput = hypsStr.split (",");
		hypsNumber = hypsStr.equals ("") ? 0 : hypsInput.length;
		
		String read;
		while ((read = br.readLine ()) != null) {
			linesInput.add (read);
		}
		
		lines  = new String  [linesInput.size ()];
		proved = new boolean [linesInput.size ()];
		
		for (int i = 0; i < linesInput.size (); i ++) {
			lines [i] = linesInput.get (i);
			proved [i] = false;
		}
		
		/*= TIMER =*/  long timeEnd = System.currentTimeMillis ();
		/*= OUTPUT =*/ System.out.println ("Read file time: " + (timeEnd - timeStart) + " ms");
		
		/*= TIMER =*/ timeStart = System.currentTimeMillis ();
		
		Node [] hyps  = new Node [hypsInput.length];
		Node [] trees = new Node [linesInput.size ()];
		
		for (int i = 0; i < hypsInput.length; i ++) {
			hypsInput [i] = clearString (hypsInput [i]);
			hyps [i] = generateTree (generateStack (compressString (hypsInput [i])));
		}
		
		/*= TIMER =*/  timeEnd = System.currentTimeMillis ();
		/*= OUTPUT =*/ System.out.println ("Hypothesis parse time: " + (timeEnd - timeStart) + " ms");
		
		/*= TIMER =*/ timeStart = System.currentTimeMillis ();
		
		pw.println (first);
		for (int i = 0; i < lines.length; i ++) {
			boolean flag = true;
			lines [i] = clearString (lines [i]);
			trees [i] = generateTree (generateStack (compressString (lines [i])));
			/*= OUTPUT =*/ pw.print ("(" + (i + 1) + ") " + lines [i] + " (");
			
			if (trees [i].operator == '-') {
				int axi = 0;
				boolean equal = false;
				
				for (int j = 0; j < axioms.length; j ++) {
					equal = safeApply (trees [i], axioms [j]);
					axi = j + 1;
					
					if (equal) {
						proved [i] = true;
						flag = false;
						
						/*= OUTPUT =*/ pw.println ("Сх. акс. " + axi + ")");
						break;
					}
				}
			}
			
			if (flag && hypsNumber > 0) {
				int hyp = 0;
				boolean equal = false;
				
				for (int j = 0; j < hyps.length; j ++) {
					equal = safeApply (trees [i], hyps [j]);
					hyp = j + 1;
				}
				
				if (equal) {
					proved [i] = true;
					flag = false;
					
					/*= OUTPUT =*/ pw.println ("Предп. " + hyp + ")");
				}
			}
			
			if (flag) {
				String p = "", q = "";
				int last = 0;
				
				for (int j = 0; j < i; j ++) {
					if (proved [j] 
							&& trees [j].operator == '-'
							&& trees [j].right != null
							&& trees [j].right.value.equals (trees [i].value)) {
						mpNeed.put (trees [j].left.value, j + 1);
						last = j;
					}
				}
				
				for (int j = 0; j < last; j ++) {
					if (proved [j] && mpNeed.containsKey (trees [j].value)) {
						p = j + 1 + "";
						q = mpNeed.get (trees [j].value).toString ();
						
						flag = false;
						proved [i] = true;
					}
				}
				
				if (!flag) { /*= OUTPUT =*/ pw.println ("M. P. " + p + ", " + q + ")"); }
				mpNeed.clear ();
			}
			
			if (flag) { /*= OUTPUT =*/ pw.println ("He доказано)"); }
		}
		
		/*= TIMER =*/ timeEnd = System.currentTimeMillis ();
		System.out.println ("Check time: " + (timeEnd - timeStart) + " ms");
		
		br.close (); 
		pw.close();
	}
	
	private static boolean isOperator (char c) {
		return (c == '-') || (c == '|') || (c == '&') || (c == '!');
	}
	
	private static boolean isLetterOrDigit (char c) {
		return !isOperator (c) && (c != '(') && (c != ')');
	}
	
	private static int getPriority (char oper) {
		switch (oper) {
			case '-': return 1;
			case '|': return 2;
			case '&': return 3;
			case '!': return 4;
			default : return 0;
		}
	}
	
	private static boolean isLeftHand (char c) {
		return (c == '&') || (c == '|');
	}
	
	private static String compressString (String input) {
		StringBuilder st = new StringBuilder ();
		
		for (int i = 0; i < input.length (); i ++) {
			char current = input.charAt (i);
			if (current != ' ' && current != '>') {
				st.append (current);
			}
		}
		
		return st.toString ();
	}
	
	private static String clearString (String input) {
		StringBuilder st = new StringBuilder ();
		
		for (int i = 0; i < input.length (); i ++) {
			char current = input.charAt (i);
			if (current != ' ') {
				st.append (current);
			}
		}
		
		return st.toString ();
	}
	
	private static ArrayList <String> generateStack (String input) {
		ArrayList <String>    expressions = new ArrayList <> ();
		ArrayList <Character> operators   = new ArrayList <> ();
		
		for (int i = 0; i < input.length (); i ++) {
			char current = input.charAt (i);
			
			if (isLetterOrDigit (current)) {
				StringBuilder st = new StringBuilder ();
				while (i < input.length () && isLetterOrDigit (input.charAt (i))) {
					st.append (input.charAt (i));
					i ++;
				}
				
				i --;
				expressions.add (st.toString ());
			} else if (isOperator (current)) {
				boolean flag = true;
				
				while (flag && !operators.isEmpty ()) {
					char last = operators.get (operators.size () - 1);
					
					if (last != '(' 
							&& (isLeftHand (current) && (getPriority (current) <= getPriority (last)) 
							|| (!isLeftHand (current) && (getPriority (current) < getPriority (last))
					))) {
						expressions.add (last + "");
						operators.remove (operators.size () - 1);
					} else { flag = false; }
				}
				
				operators.add (current);
			} else if (current == '(') {
				operators.add (current);
			} else if (current == ')') {
				boolean flag = true;
				
				while (flag && !operators.isEmpty ()) {
					char last = operators.get (operators.size () - 1);
					
					if (last == '(') { 
						flag = false;
					} else {
						expressions.add (last + "");
						operators.remove (operators.size () - 1);
					}
				}
				
				operators.remove (operators.size () - 1);
			}
		}
		
		while (!operators.isEmpty ()) {
			char last = operators.get (operators.size () - 1);
			
			expressions.add (last + "");
			operators.remove (operators.size () - 1);
		}
		
		return expressions;
	}
	
	private static Node generateTree (ArrayList <String> express) {
		//Reversing expressions
		ArrayList <String> revExp = new ArrayList <> ();
		for (int i = 0; i < express.size (); i ++) {
			revExp.add (express.get (express.size () - i - 1));
		}
		
		Node tree = new Node ();
		tree.parent = null;
		
		while (!revExp.isEmpty ()) {
			String last = revExp.remove (revExp.size () - 1);
			
			char current = last.charAt (0);
			if (isLetterOrDigit (current)) {
				if (revExp.isEmpty ()) {
					tree.value = last;
				} else if (tree.right == null && tree.left == null) {
					Node child = new Node ();
					child.parent = tree;
					child.value = last;
					
					tree.left = child;
				} else if (tree.right == null) {
					Node child = new Node ();
					child.parent = tree;
					child.value = last;
					
					tree.right = child;
				} else if (tree.left != null && tree.right != null) {
					Node tmp = new Node ();
					tmp.left = tree.right;
					tmp.parent = tree;
					tmp.left.parent = tmp;
					tree.right = tmp;
					
					tmp.right = new Node ();
					tmp.right.parent = tmp;
					tmp.right.value = last;
					tree = tmp;
				}
			} else if (isOperator (current)) {
				tree.operator = current;
				
				if (current == '!') {
					if (tree.left != null && tree.right != null) {
						Node tmp = new Node ();
						tmp.left = tree.right;
						tmp.right = null;
						tmp.parent = tree;
						tree.right = tmp;
						tmp.value = last + "(" + tmp.left.value + ")";
						tmp.operator = '!';
						
						continue;
					}
					
					tree.value = last + "(" + tree.left.value + ")";
				} else {
					tree.value = "(" + tree.left.value + ")" + last;
					if (tree.right != null) {
						tree.value += "(" + tree.right.value +  ")";
					}
				}
				
				if (tree.parent != null) {
					tree = tree.parent;
				} else if (!revExp.isEmpty ()) {
					tree.parent = new Node ();
					tree.parent.left = tree;
					tree = tree.parent;
					
					tree.right = null;
					tree.parent = null;
				}
			}
		}
		
		return tree;
	}
	
	private static boolean safeApply (Node graph, Node model) {
		vars.clear ();
		
		return apply (graph, model);
	}
	
	private static boolean apply (Node graph, Node model) {
		boolean equal = true;
		
		if (model.operator != 0 && model.operator != graph.operator) { return false; }
		
		if (model.left == null && model.right == null) {
			if (vars.containsKey (model.value)) {
				if (!vars.get (model.value).equals (graph.value)) {
					equal = false;
				}
			} else { vars.put (model.value, graph.value); }
		} else {
			if (equal && model.left != null) {
				if (graph.left != null) {
					equal = apply (graph.left, model.left);
				} else { equal = false; }
			}
			
			if (equal && model.right != null) {
				if (graph.right != null) {
					equal = apply (graph.right, model.right);
				} else { equal = false; }
			}
		}
			
		return equal;
	}
	
}
