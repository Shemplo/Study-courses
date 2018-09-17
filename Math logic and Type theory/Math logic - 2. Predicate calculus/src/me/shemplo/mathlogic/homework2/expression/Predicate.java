package me.shemplo.mathlogic.homework2.expression;

import java.util.ArrayList;

public class Predicate extends Function {

	public String name;
	public ArrayList <Expression> vars;
	
	public Predicate (String name, ArrayList <Expression> list) {
		super (name, list);
		this.name = name; 
		this.vars = list;
	}
	
	@SuppressWarnings ("unchecked")
	public Expression getInstance () { return new Predicate (name, (ArrayList <Expression>) vars.clone ()); }
	
	public String toString () {
		String out = name;
		if (vars.size () > 0) {
			out += "(";
			for (int i = 0; i < vars.size (); i ++) {
				Expression var = vars.get (i);
				out += var.toString ();
				
				if (i < vars.size () - 1) { out += ","; }
			}
			out += ")";
		}
		
		return out;
	}
	
	/////////////////////////////////////////////////////
	
	public boolean equals (Object object) {
		if (!(object instanceof Predicate)) { return false; }
		
		Predicate obj = (Predicate) object;
		if (!obj.name.equals (name)) { return false; }
		if (obj.vars.size () != vars.size ()) { return false; }
		
		for (int i = 0; i < vars.size (); i ++) {
			if (!vars.get (i).toString ().equals (obj.vars.get (i).toString ())) {
				return false;
			}
		}
		
		return true;
	}
	
}
