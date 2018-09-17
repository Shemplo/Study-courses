package me.shemplo.mathlogic.homework2.expression;

public class Variable extends Expression {

	public String name;
	
	public Variable (String name) { this.name = name; }
	
	public Expression getInstance () { return new Variable (name); }
	
	public String toString () { return name; }
	
	/////////////////////////////////////////
	
	public boolean equals (Object object) {
		return (object instanceof Variable)
					&& ((Variable) object).name.equals (name);
	}
	
}
