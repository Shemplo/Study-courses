package me.shemplo.mathlogic.homework2.expression;

public class Const extends Expression {

	public String name;

	public Const (String name) { this.name = name; }

	public Expression getInstance () { return new Const (name); }
	
	public String toString () { return name; }
	
}
