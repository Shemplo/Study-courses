package me.shemplo.mathlogic.homework2.expression;

public class Operations {

	public static enum OperationTypes {
		ADD,         // *
		AND,         // &
		ANY,         // @
		CLOSE,       // )
		COMMA,       // ,
		CONST,       // [0-9]+
		EQUAL,       // =
		EXIST,       // ?
		FUNCTION,    // [a-z]+ (...)
		IMPLICATION, // ->
		INCREMENT,   // '
		MULTIPLY,    // *
		NOT,         // !
		OPEN,        // (
		OR,          // |
		PREDICATE,   // [a-z]+ (...)
		VAR,         // [a-z]+
		
		SKIP,
		END
	}
	
	public static abstract class Binary extends Expression {
		
		public Expression left, right;
		
		public Binary (Expression left, Expression right) {
			this.left = left; this.right = right;
		}
		
	}
	
	public static abstract class Unary extends Expression {
		
		public Expression left;
		
		public Unary (Expression left) {
			this.left = left;
		}
		
	}
	
	public static abstract class Quantor extends Expression {
		
		public Expression expression;
		public Variable variable;
		
		public Quantor (Variable variable, Expression expression) {
			this.variable = variable; this.expression = expression;
		}
		
	}
	
	//////////////////////////////////////////////////////////
	
	public static class Add extends Binary {
		
		public Add (Expression left, Expression right) { super (left, right); }
		
		public Expression getInstance () { return new Add (left.getInstance (), right.getInstance ()); }
		
		public String toString () {
			return "(" + left.toString() + ")+(" + right.toString() + ")";
		}
		
	}
	
	public static class And extends Binary {
		
		public And (Expression left, Expression right) { super (left, right); }
		
		public Expression getInstance () { return new And (left.getInstance (), right.getInstance ()); }
		
		public String toString () {
			return "(" + left.toString() + ")&(" + right.toString() + ")";
		}
		
	}
	
	public static class Any extends Quantor {
		
		public Any (Variable variable, Expression expression) { super (variable, expression); }
		
		public Expression getInstance () { return new Any ((Variable) variable.getInstance (), 
															expression.getInstance ()); }
		
		public String toString () {
			return "@" + variable.toString() + "(" + expression.toString() + ")";
		}
		
	}
	
	public static class Exist extends Quantor {
		
		public Exist (Variable variable, Expression expression) { super (variable, expression); }
		
		public Expression getInstance () { return new Exist ((Variable) variable.getInstance (), 
																expression.getInstance ()); }
		
		public String toString () {
			return "?" + variable.toString() + "(" + expression.toString() + ")";
		}
		
	}
	
	public static class Equal extends Binary {
		
		public Equal (Expression left, Expression right) { super (left, right); }
		
		public Expression getInstance () { return new Equal (left.getInstance (), right.getInstance ()); }
		
		public String toString () {
			return "(" + left.toString() + ")=(" + right.toString() + ")";
		}
		
	}
	
	public static class Implication extends Binary {
		
		public Implication (Expression left, Expression right) { super (left, right); }
		
		public Expression getInstance () { return new Implication (left.getInstance (), right.getInstance ()); }
		
		public String toString () {
			return "(" + left.toString() + ")->(" + right.toString() + ")";
		}
		
	}
	
	public static class Increment extends Unary {
		
		public Increment (Expression expression) { super (expression); }
		
		public Expression getInstance () { return new Increment (left.getInstance ()); }
		
		public String toString () {
			return "(" + left.toString() + ")'";
		}
		
	}
	
	public static class Multiply extends Binary {
		
		public Multiply (Expression left, Expression right) { super (left, right); }
		
		public Expression getInstance () { return new Multiply (left.getInstance (), right.getInstance ()); }
		
		public String toString () {
			return "(" + left.toString() + ")*(" + right.toString() + ")";
		}
		
	}
	
	public static class Not extends Unary {
		
		public Not (Expression expression) { super (expression); }
		
		public Expression getInstance () { return new Not (left.getInstance ()); }
		
		public String toString () {
			return "!(" + left.toString() + ")";
		}
		
	}
	
	public static class Or extends Binary {
		
		public Or (Expression left, Expression right) { super (left, right); }
		
		public Expression getInstance () { return new Or (left.getInstance (), right.getInstance ()); }
		
		public String toString () {
			return "(" + left.toString() + ")|(" + right.toString() + ")";
		}
		
	}
	
}
