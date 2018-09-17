package me.shemplo.mathlogic.homework2.expression;

import java.util.ArrayList;

public class Function extends Expression {

	public String name;
	public ArrayList <Expression> vars;
	
	public Function (String name, ArrayList <Expression> list) {
		this.name = name; this.vars = list;
	}
	
	@SuppressWarnings ("unchecked")
	public Expression getInstance () { return new Function (name, (ArrayList <Expression>) vars.clone ()); }
	
	public String toString () {
		String out = name + "(";
		for (int i = 0; i < vars.size (); i ++) {
			Expression var = vars.get (i);
			out += var.toString ();
			
			if (i < vars.size () - 1) { out += ","; }
		}
		
		out += ")";
		return out;
	}
	
}
