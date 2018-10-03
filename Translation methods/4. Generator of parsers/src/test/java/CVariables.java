

import java.util.ArrayList;
import java.util.List;

import java.io.IOException;
import java.io.Reader;

import ru.shemplo.dsau.stuctures.Pair;
import ru.shemplo.dsau.stuctures.Trio;
import ru.shemplo.mt.task4.dmy.TreeNode;

public class CVariables {
    
    private static enum Times {
        // Single - Zero or Single - Zero or more - Single or more
        S,          ZOS,             ZOM,           SOM
    }
    
    private static interface ParseTreeProducer {

        public Pair <?, Integer> consume (int start) throws IOException;
        
        public void apply (TreeNode node, ParseTreeProducer producer);
        
        public TreeNode getNode ();
        
        public void reset ();
        
    }
    
    private final ParseTreeProducer TOP_LEVEL_CONSUMER;
    
    public CVariables () {
        this.TOP_LEVEL_CONSUMER = new variables_context ();
    }
    
    private final List <Integer> buffer = new ArrayList <> ();
    private Reader reader = null;
    
    public void parse (Reader r) throws IOException {
        this.reader = r;
        buffer.clear ();
        
        Pair <?, ?> result = TOP_LEVEL_CONSUMER.consume (0);
        if (result.F instanceof TreeNode) {
            ((TreeNode) result.F).printTree ("");
        } else {
            System.out.println (result);
        }
    }
    
    private int getByIndex (int index) throws IOException {
        while (buffer.size () - 1 < index) {
            int code = reader.read ();
            buffer.add (code);
        }
        
        return buffer.get (index).intValue ();
    }
    
    private abstract class AbsNonTerminalProducer implements ParseTreeProducer {
        
        protected final List <Trio <ParseTreeProducer, Times, String>> [] BRANCHES;
        protected final List <List <TreeNode>> READY_NODES;
        protected final int [][] COUNTERS;
        
        protected TreeNode node;
        
        @SafeVarargs
        public AbsNonTerminalProducer (List <Trio <ParseTreeProducer, Times, String>>... branches) {
            this.COUNTERS = new int [branches.length][];
            this.READY_NODES = new ArrayList <> ();
            
            for (int i = 0; i < branches.length; i++) {
                COUNTERS [i] = new int [branches [i].size ()];
                READY_NODES.add (new ArrayList <> ());
            }
            
            this.BRANCHES = branches;
        }
        
        public String toString () {
            return "(" + this.getClass ().getSimpleName () 
                 + "~" + this.hashCode () + ")";
        }
        
        private boolean isTimeEnough (Times time, int counts) {
            if (Times.S.equals (time) && counts != 1) {
                return false;
            }
            
            if (Times.SOM.equals (time) && counts < 1) {
                return false;
            }
            
            /*
            if (Times.ZOS.equals (time) && counts > 1) {
                return false;
            }
            */
            
            return true;
        }
        
        private boolean isBranchReady (int branch) {
            for (int i = 0; i < BRANCHES [branch].size (); i++) {
                Trio <?, Times, ?> term = BRANCHES [branch].get (i);
                if (!isTimeEnough (term.S, COUNTERS [branch][i])) {
                    return false;
                }
            }
            
            return true;
        }
        
        @Override
        public Pair <?, Integer> consume (int start) throws IOException {
            int [] failed = new int [BRANCHES.length];
            for (int i = 0; i < BRANCHES.length; i++) {
                
                TreeNode node = new TreeNode (this.toString ());
                int pointer = start;
                
                for (int j = 0; j < BRANCHES [i].size (); j++) {
                    int code = getByIndex (pointer);
                    if (code == -1) { break; }
                    
                    Trio <ParseTreeProducer, Times, ?> term = BRANCHES [i].get (j);
                    term.F.reset ();
                    
                    Pair <?, Integer> result = term.F.consume (pointer);
                    
                    if (result.F instanceof Exception) {
                        if (Character.isWhitespace (code)) {
                            while (Character.isWhitespace (getByIndex (++pointer))) {}
                            j -=1; continue; // One more attempt without whitespaces
                        }
                        
                        //System.out.println (result);
                        
                        if (isTimeEnough (term.S, COUNTERS [i][j])) {
                            // This term repeated enough times
                            continue;
                        } else {
                            // Failed to continue branch -> drop it
                            failed [i] = pointer;
                            break;
                        }
                    } else {
                        node.CHILDREN.add ((TreeNode) result.F);
                        term.F.apply (node, this);
                        COUNTERS [i][j] += 1;
                        pointer += result.S;
                        
                        if (Times.S.equals (term.S) 
                            || Times.ZOS.equals (term.S)) {
                            continue;
                        } else {
                            // Term can be repeated
                            j -= 1; continue;
                        }
                    }
                }
                
                if (isBranchReady (i)) {
                    return Pair.mp (node, pointer - start);
                }
            }
            
            /*
            System.out.print (this + " ");
            for (int i = 0; i < failed.length; i++) {
                System.out.print (failed [i] + " " + ((char) getByIndex (failed [i]) + "; "));
            }
            System.out.println ();
            */
            
            return Pair.mp (new IllegalStateException ("No branch matched in " + this), start);
        }
        
        @Override
        public void apply (TreeNode node, ParseTreeProducer producer) {}
        
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
        
        public String value = "";
        
        public AbsTerminalProducer (String need, boolean isRegexp) {
            this.IS_REGEXP = isRegexp;
            this.REGEXP = need;
            
            if (!isRegexp) { accumulator.append (need); }
        }
        
        public String toString () {
            return "(" + this.getClass ().getSimpleName () 
                 + " ~ " + this.hashCode () + ")";
        }
        
        @Override
        public Pair <?, Integer> consume (int start) throws IOException {
            //System.out.println ("--> Enter to " + this);
            if (IS_REGEXP) {
                while (true) {
                    char c = (char) getByIndex (start + pointer);
                    if ((accumulator.toString () + c).matches (REGEXP)) {
                        accumulator.append (c);
                    } else {
                        break;
                    }
                    
                    //System.out.println ("Char `" + c + "` in " + this);
                    pointer += 1;
                }
                
                if (pointer == 0) {
                    char c = (char) getByIndex (start + pointer);
                    String text = "Unexpected char `" + c + "` in " + this;
                    return Pair.mp (new IllegalStateException (text), pointer);
                }
            } else {
                while (pointer < accumulator.length ()) {
                    char c = (char) getByIndex (start + pointer);
                    if (accumulator.charAt (pointer) != c) {
                        String text = "Unexpected char `" + c + "` in " + this + " at " + (start + pointer);
                        return Pair.mp (new IllegalStateException (text), pointer);
                    }
                    
                    //System.out.println ("Char `" + c + "` in " + this);
                    pointer += 1;
                }
            }
            
            //System.out.println ("> " + this + " " + accumulator.toString ());
            this.node = new TreeNode (accumulator.toString ());
            value = accumulator.toString ();
            
            return Pair.mp (getNode (), pointer);
        }
        
        @Override
        public void apply (TreeNode node, ParseTreeProducer producer) {}
        
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
            value = "";
        }
        
    }
    
// generated class
	private class COMMA_context extends AbsTerminalProducer {

		public COMMA_context () {
			super (",", false);
		}

		public void apply (TreeNode node, ParseTreeProducer context) {
		}

	}

	// generated class
	private class FLOAT_context extends AbsTerminalProducer {

		public FLOAT_context () {
			super ("float", false);
		}

		public void apply (TreeNode node, ParseTreeProducer context) {
		}

	}

	// generated class
	private class IDENT_context extends AbsTerminalProducer {

		public IDENT_context () {
			super ("[a-zA-Z][a-zA-Z0-9]*", true);
		}

		public void apply (TreeNode node, ParseTreeProducer context) {
			if (context instanceof identifier_context) {
				((identifier_context) context).code=value;
			}
		}

	}

	// generated class
	private class STAR_context extends AbsTerminalProducer {

		public STAR_context () {
			super ("*", false);
		}

		public void apply (TreeNode node, ParseTreeProducer context) {
		}

	}

	// generated class
	private class SEMI_context extends AbsTerminalProducer {

		public SEMI_context () {
			super (";", false);
		}

		public void apply (TreeNode node, ParseTreeProducer context) {
		}

	}

	// generated class
	private class CHAR_context extends AbsTerminalProducer {

		public CHAR_context () {
			super ("char", false);
		}

		public void apply (TreeNode node, ParseTreeProducer context) {
		}

	}

	// generated class
	private class DOUBLE_context extends AbsTerminalProducer {

		public DOUBLE_context () {
			super ("double", false);
		}

		public void apply (TreeNode node, ParseTreeProducer context) {
		}

	}

	// generated class
	private class INT_context extends AbsTerminalProducer {

		public INT_context () {
			super ("int", false);
		}

		public void apply (TreeNode node, ParseTreeProducer context) {
		}

	}

	// generated class
	private class var_line_context extends AbsNonTerminalProducer {

		public var_line_context () {
			super (new ArrayList <> ());

			var_type_context _var0 = new var_type_context ();
			BRANCHES [0].add (Trio.mt (_var0, Times.S, "var_type"));

			STAR_context _var1 = new STAR_context ();
			BRANCHES [0].add (Trio.mt (_var1, Times.ZOM, "STAR"));

			var_name_context _var2 = new var_name_context ();
			BRANCHES [0].add (Trio.mt (_var2, Times.SOM, "var_name"));

			SEMI_context _var3 = new SEMI_context ();
			BRANCHES [0].add (Trio.mt (_var3, Times.S, "SEMI"));

			reset ();
		}

		public void apply (ParseTreeProducer context) {
		}

	}

	// generated class
	private class identifier_context extends AbsNonTerminalProducer {

		public String code = "";

		public identifier_context () {
			super (new ArrayList <> ());

			IDENT_context _var4 = new IDENT_context ();
			BRANCHES [0].add (Trio.mt (_var4, Times.S, "IDENT"));

			reset ();
		}

		public void apply (ParseTreeProducer context) {
		}

		@Override
		public void reset () {
			super.reset ();
			code = "";
		}

	}

	// generated class
	private class variables_context extends AbsNonTerminalProducer {

		public variables_context () {
			super (new ArrayList <> ());

			var_line_context _var5 = new var_line_context ();
			BRANCHES [0].add (Trio.mt (_var5, Times.ZOM, "var_line"));

			reset ();
		}

	}

	// generated class
	private class var_type_context extends AbsNonTerminalProducer {

		public var_type_context () {
			super (new ArrayList <> (), new ArrayList <> (), new ArrayList <> (), new ArrayList <> ());

			INT_context _var6 = new INT_context ();
			BRANCHES [0].add (Trio.mt (_var6, Times.S, "INT"));

			DOUBLE_context _var7 = new DOUBLE_context ();
			BRANCHES [1].add (Trio.mt (_var7, Times.S, "DOUBLE"));

			CHAR_context _var8 = new CHAR_context ();
			BRANCHES [2].add (Trio.mt (_var8, Times.S, "CHAR"));

			FLOAT_context _var9 = new FLOAT_context ();
			BRANCHES [3].add (Trio.mt (_var9, Times.S, "FLOAT"));

			reset ();
		}

		public void apply (ParseTreeProducer context) {
		}

	}

	// generated class
	private class var_name_context extends AbsNonTerminalProducer {

		public var_name_context () {
			super (new ArrayList <> ());

			identifier_context _var10 = new identifier_context ();
			BRANCHES [0].add (Trio.mt (_var10, Times.S, "identifier"));

			COMMA_context _var11 = new COMMA_context ();
			BRANCHES [0].add (Trio.mt (_var11, Times.ZOS, "COMMA"));

			reset ();
		}

		public void apply (ParseTreeProducer context) {
		}

	}

	
    
    
}
