package ru.shemplo.mt.task4.dmy;

import java.util.ArrayList;
import java.util.List;

import java.io.IOException;
import java.io.Reader;

import ru.shemplo.dsau.stuctures.Trio;

public class DummyParser {
	
	private static enum Times {
		// Single - Zero or Single - Zero or more - Single or more
		S,          ZOS,             ZOM,           SOM
	}
	
	private static interface ParseTreeProducer {

		public int consume (int start) throws IOException;
		
		public void apply (ParseTreeProducer producer);
		
		public TreeNode getNode ();
		
		public void reset ();
		
	}
	
	private final ParseTreeProducer TOP_LEVEL_CONSUMER;
	
	public DummyParser () {
		List <Trio <ParseTreeProducer, Times, Runnable>> branchPROG = new ArrayList <> ();
		PROGRAM_context pc = new PROGRAM_context ();
		branchPROG.add (Trio.mt (pc, Times.S, () -> {}));
		
		identifier_context ic = new identifier_context ("[a-zA-Z0-9]+");
		branchPROG.add (Trio.mt (ic, Times.S, () -> {}));
		
		List <Trio <ParseTreeProducer, Times, Runnable>> branchDECVARS = new ArrayList <> ();
		VAR_context Vc = new VAR_context ();
		branchDECVARS.add (Trio.mt (Vc, Times.S, () -> {}));
		
		identifier_context ic2 = new identifier_context ("[a-zA-Z0-9]+");
		branchDECVARS.add (Trio.mt (ic2, Times.S, () -> {}));
		
		COLON_context cc = new COLON_context ();
		branchDECVARS.add (Trio.mt (cc, Times.S, () -> {}));
		
		List <Trio <ParseTreeProducer, Times, Runnable>> branchINT_VARS = new ArrayList <> ();
		INTEGER_context inc = new INTEGER_context ();
		branchINT_VARS.add (Trio.mt (inc, Times.S, () -> {}));
		
		List <Trio <ParseTreeProducer, Times, Runnable>> branchSTR_VARS = new ArrayList <> ();
		STRING_context sc = new STRING_context ();
		branchSTR_VARS.add (Trio.mt (sc, Times.S, () -> {}));
		
		variable_type_context vtc = new variable_type_context (branchINT_VARS, branchSTR_VARS);
		branchDECVARS.add (Trio.mt (vtc, Times.S, () -> {}));
		
		List <Trio <ParseTreeProducer, Times, Runnable>> branchVARS = new ArrayList <> ();
		var_declaration_context vdc = new var_declaration_context (branchDECVARS);
		branchVARS.add (Trio.mt (vdc, Times.ZOM, () -> {}));
		
		List <Trio <ParseTreeProducer, Times, Runnable>> branchMS = new ArrayList <> ();
		BEGIN_context bc = new BEGIN_context ();
		branchMS.add (Trio.mt (bc, Times.S, () -> {}));
		
		END_context ec = new END_context ();
		branchMS.add (Trio.mt (ec, Times.S, () -> {}));
		
		DOT_context dc = new DOT_context ();
		branchMS.add (Trio.mt (dc, Times.S, () -> {}));
		
		List <Trio <ParseTreeProducer, Times, Runnable>> branch = new ArrayList <> ();
		header_context hc = new header_context (branchPROG);
		branch.add (Trio.mt (hc, Times.S, () -> {}));
		
		variables_context vc = new variables_context (branchVARS);
		branch.add (Trio.mt (vc, Times.S, () -> {}));
		
		main_scope_context msc = new main_scope_context (branchMS);
		branch.add (Trio.mt (msc, Times.S, () -> {}));
		
		this.TOP_LEVEL_CONSUMER = new pascal_context (branch);
	}
	
	private final List <Integer> buffer = new ArrayList <> ();
	private Reader reader = null;
	
	public void parse (Reader r) throws IOException {
		this.reader = r;
		buffer.clear ();
		
		TOP_LEVEL_CONSUMER.consume (0);
		System.out.println (TOP_LEVEL_CONSUMER.getNode ());
	}
	
	private int getByIndex (int index) throws IOException {
		while (buffer.size () - 1 < index) {
			int code = reader.read ();
			buffer.add (code);
			
			/*
			if (code == -1) {
				String text = "Unexpected end of stream in " + this;
				throw new IllegalStateException (text);
			}
			*/
		}
		
		return buffer.get (index).intValue ();
	}
	
	private abstract class AbsNonTerminalProducer implements ParseTreeProducer {
		
		protected final List <Trio <ParseTreeProducer, Times, Runnable>> [] BRANCHES;
		protected final List <List <TreeNode>> READY_NODES;
		protected final int [][] COUNTERS;
		
		protected TreeNode node;
		
		@SafeVarargs
		public AbsNonTerminalProducer (List <Trio <ParseTreeProducer, Times, Runnable>>... branches) {
			this.COUNTERS = new int [branches.length][];
			this.READY_NODES = new ArrayList <> ();
			
			for (int i = 0; i < branches.length; i++) {
				COUNTERS [i] = new int [branches [i].size ()];
				READY_NODES.add (new ArrayList <> ());
			}
			
			this.BRANCHES = branches;
		}
		
		public String toString () {
			return this.getClass ().getSimpleName () 
				 + "~" + this.hashCode ();
		}
		
		@Override
		public int consume (int start) throws IOException {
			for (int i = 0; i < BRANCHES.length; i++) {
				List <TreeNode> nodes = new ArrayList <> ();
				int used = 0;
				
				try {
					for (int j = 0; j < BRANCHES [i].size (); j++) {
						Trio <ParseTreeProducer, Times, Runnable> 
							parser = BRANCHES [i].get (j);
						
						while (true) {
							int code = getByIndex (start + used);
							if (code == -1) { break; }
							
							parser.F.reset ();
							
							try {
								used += parser.F.consume (start + used);
								TreeNode node = parser.F.getNode ();
								parser.F.apply (this);
								if (node != null) { 
									nodes.add (node); 
								}
								COUNTERS [i][j]++;
								
								if (Times.S.equals (parser.S) 
									|| Times.ZOS.equals (parser.S)) {
									break;
								}
							} catch (IllegalStateException ise) {
								char c = (char) code;
								if (Character.isWhitespace (c)) {
									used += 1; continue;
								}
								
								if (Times.ZOS.equals (parser.S) 
									|| Times.ZOM.equals (parser.S)
									|| COUNTERS [i][j] > 0) {
									break;
								}
								
								// System.out.println (parser);
								
								String text = "Bad character `" + c + "` in " + this + " at " + (start + used);
								throw new IllegalStateException (text);
							}
						}
					}
					
					boolean flag = true;
					for (int j = 0; j < BRANCHES [i].size (); j++) {
						Trio <ParseTreeProducer, Times, Runnable> 
							parser = BRANCHES [i].get (j);
						if (COUNTERS [i][j] == 0 
							&& !Times.ZOS.equals (parser.S) 
							&& !Times.ZOM.equals (parser.S)) {
							flag = false;
						}
					}
					
					if (flag) {
						this.node = new TreeNode (this.getClass ().getSimpleName ());
						node.CHILDREN.addAll (nodes);
					}
					
					return used;
				} catch (IllegalStateException ise) {
					//ise.printStackTrace ();
				}
			}
			
			throw new IllegalStateException ();
		}
		
		@Override
		public void apply (ParseTreeProducer producer) {}
		
		@Override
		public TreeNode getNode () {
			return node;
		}
		
		@Override
		public void reset () {
			for (int i = 0; i < BRANCHES.length; i++) {
				COUNTERS [i] = new int [BRANCHES [i].size ()];
			}
			
			node = null;
		}
		
	}
	
	private abstract class AbsTerminalProducer implements ParseTreeProducer {
		
		protected final boolean IS_REGEXP;
		protected final String REGEXP;
		
		protected StringBuilder accumulator = new StringBuilder ();
		protected int pointer = 0;
		
		protected TreeNode node;
		
		public AbsTerminalProducer (String need, boolean isRegexp) {
			this.IS_REGEXP = isRegexp;
			this.REGEXP = need;
			
			if (!isRegexp) { accumulator.append (need); }
		}
		
		public String toString () {
			return this.getClass ().getSimpleName () 
				 + "~" + this.hashCode ();
		}
		
		@Override
		public int consume (int start) throws IOException {
			if (IS_REGEXP) {
				while (true) {
					char c = (char) getByIndex (start + pointer);
					if ((accumulator.toString () + c).matches (REGEXP)) {
						accumulator.append (c);
					} else {
						break;
					}
					
					pointer += 1;
				}
				
				if (pointer == 0) {
					char c = (char) getByIndex (start + pointer);
					String text = "Unexpected char `" + c + "` in " + this;
					throw new IllegalStateException (text);
				}
			} else {
				while (pointer < accumulator.length ()) {
					char c = (char) getByIndex (start + pointer);
					if (accumulator.charAt (pointer) != c) {
						String text = "Unexpected char `" + c + "` in " + this;
						throw new IllegalStateException (text);
					}
					
					pointer += 1;
				}
			}
			
			System.out.println ("> " + this + " " + accumulator.toString ());
			this.node = new TreeNode (accumulator.toString ());
			return pointer;
		}
		
		@Override
		public void apply (ParseTreeProducer producer) {}
		
		@Override
		public TreeNode getNode () {
			return node;
		}
		
		@Override
		public void reset () {
			if (IS_REGEXP) {
				accumulator.setLength (0);
			}
			
			pointer = 0;
			node = null;
		}
		
	}
	
	private class pascal_context extends AbsNonTerminalProducer {
		
		@SafeVarargs
		public pascal_context (List <Trio <ParseTreeProducer, Times, Runnable>>... branches) {
			super (branches);
		}
		
	}
	
	private class header_context extends AbsNonTerminalProducer {
		
		@SafeVarargs
		public header_context (List <Trio <ParseTreeProducer, Times, Runnable>>... branches) {
			super (branches);
		}
		
	}
	
	private class PROGRAM_context extends AbsTerminalProducer {
		
		public PROGRAM_context () {
			super ("program", false);
		}
		
	}
	
	private class identifier_context extends AbsTerminalProducer {
		
		public identifier_context (String need) {
			super (need, true);
		}
		
	}
	
	private class variables_context extends AbsNonTerminalProducer {
		
		@SafeVarargs
		public variables_context (List <Trio <ParseTreeProducer, Times, Runnable>>... branches) {
			super (branches);
		}
		
	}
	
	private class var_declaration_context extends AbsNonTerminalProducer {
		
		@SafeVarargs
		public var_declaration_context (List <Trio <ParseTreeProducer, Times, Runnable>>... branches) {
			super (branches);
		}
		
	}
	
	private class VAR_context extends AbsTerminalProducer {
		
		public VAR_context () {
			super ("var", false);
		}
		
	}
	
	private class COLON_context extends AbsTerminalProducer {
		
		public COLON_context () {
			super (":", false);
		}
		
	}
	
	private class variable_type_context extends AbsNonTerminalProducer {
		
		@SafeVarargs
		public variable_type_context (List <Trio <ParseTreeProducer, Times, Runnable>>... branches) {
			super (branches);
		}
		
	}
	
	private class INTEGER_context extends AbsTerminalProducer {
		
		public INTEGER_context () {
			super ("Integer", false);
		}
		
	}
	
	private class STRING_context extends AbsTerminalProducer {
		
		public STRING_context () {
			super ("String", false);
		}
		
	}
	
	private class main_scope_context extends AbsNonTerminalProducer {
		
		@SafeVarargs
		public main_scope_context (List <Trio <ParseTreeProducer, Times, Runnable>>... branches) {
			super (branches);
		}
		
	}
	
	private class BEGIN_context extends AbsTerminalProducer {
		
		public BEGIN_context () {
			super ("begin", false);
		}
		
	}
	
	private class END_context extends AbsTerminalProducer {
		
		public END_context () {
			super ("end", false);
		}
		
	}
	
	private class DOT_context extends AbsTerminalProducer {
		
		public DOT_context () {
			super (".", false);
		}
		
	}
	
}
