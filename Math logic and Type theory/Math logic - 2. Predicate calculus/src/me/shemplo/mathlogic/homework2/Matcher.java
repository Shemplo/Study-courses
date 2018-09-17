package me.shemplo.mathlogic.homework2;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import me.shemplo.mathlogic.homework2.expression.Const;
import me.shemplo.mathlogic.homework2.expression.Expression;
import me.shemplo.mathlogic.homework2.expression.Function;
import me.shemplo.mathlogic.homework2.expression.Operations.And;
import me.shemplo.mathlogic.homework2.expression.Operations.Any;
import me.shemplo.mathlogic.homework2.expression.Operations.Binary;
import me.shemplo.mathlogic.homework2.expression.Operations.Exist;
import me.shemplo.mathlogic.homework2.expression.Operations.Implication;
import me.shemplo.mathlogic.homework2.expression.Operations.Quantor;
import me.shemplo.mathlogic.homework2.expression.Operations.Unary;
import me.shemplo.mathlogic.homework2.expression.Predicate;
import me.shemplo.mathlogic.homework2.expression.Variable;

public class Matcher {

	public boolean any = false;
	public boolean anySub = false;
	public boolean change = false;
	public boolean ext = false;
	public boolean extSub = false;
	
	public boolean structEqual (Expression left, Expression right, Map <String, Expression> exps) {
		if (left == null || right == null) { return false; }
		
		if (left.getClass () == right.getClass ()) {
			if (left instanceof Binary) {
				Binary bleft = (Binary) left;
				Binary bright = (Binary) right;
				return structEqual (bleft.left, bright.left, exps)
							&& structEqual (bleft.right, bright.right, exps);
			}
			
			if (left instanceof Const) {
				Const cleft = (Const) left;
				Const cright = (Const) right;
				return cleft.name.equals (cright.name);
			}
			
			if (left instanceof Predicate || left instanceof Function) {
				Function pleft = (Function) left;
				Function pright = (Function) right;
				if (pleft.vars.size () != pright.vars.size ()
							|| !pleft.name.equals (pright.name)) {
					return false;
				}
				
				boolean flag = true;
				for (int i = 0; flag && i < pleft.vars.size (); i ++) {
					flag = structEqual (pleft.vars.get (i), pright.vars.get (i), exps);
				}
				
				return flag;
			}
			
			if (left instanceof Quantor) {
				Quantor qleft = (Quantor) left;
				Quantor qright = (Quantor) right;
				return structEqual (qleft.variable, qright.variable, exps)
							&& structEqual (qleft.expression, qright.expression, exps);
			}
			
			if (left instanceof Unary) {
				Unary uleft = (Unary) left;
				Unary uright = (Unary) right;
				return structEqual (uleft.left, uright.left, exps);
			}
			
			if (left instanceof Variable) {
				Variable vleft = (Variable) left;
				Variable vright = (Variable) right;
				if (vleft.name.equals (vright.name)) {
					if (!exps.containsKey (vright.name)) {
						exps.put (vleft.name, right);
					} else { return exps.get (vright.name).toString ().equals (vleft.name); }
				}
				
				String name = vright.name;
				if (exps.containsKey (name)) { return deepEqual (left, exps.get (name)); }
				
				exps.put (name, left);
				return true;
			}
		}
		
		if (right instanceof Variable) {
			Variable vright = (Variable) right;
			String name = vright.name;
			if (exps.containsKey (name)) { return deepEqual (left, exps.get (name)); }
			
			exps.put (name, left);
			return true;
		}
		
		return false;
	}
	
	/////////////////////////////////////////////////////////////////////
	
	public boolean deepEqual (Expression left, Expression right) {
		if (left == null || right == null) { return false; }
		
		if (left.getClass () == right.getClass ()) {
			//System.out.println ("Left: " + left + ", right: " + right);
			
			if (left instanceof Binary) {
				//System.out.println ("1");
				Binary bleft = (Binary) left;
				Binary bright = (Binary) right;
				return deepEqual (bleft.left, bright.left)
							&& deepEqual (bleft.right, bright.right);
			}
			
			if (left instanceof Const) {
				//System.out.println ("2");
				Const cleft = (Const) left;
				Const cright = (Const) right;
				return cleft.name.equals (cright.name);
			}
			
			if (left instanceof Predicate || left instanceof Function) {
				//System.out.println ("34");
				Function pleft = (Function) left;
				Function pright = (Function) right;
				if (pleft.vars.size () != pright.vars.size ()
							|| !pleft.name.equals (pright.name)) {
					return false;
				}
				
				boolean flag = true;
				for (int i = 0; flag && i < pleft.vars.size (); i ++) {
					//System.out.println (i + " " + pleft.vars.get (i) + " " + pright.vars.get (i));
					
					flag &= pleft.vars.get (i).toString ().equals (pright.vars.get (i).toString ());
					if (!flag) {  break; }
				}
				
				//System.out.println (flag);
				return flag;
			}
			
			if (left instanceof Quantor) {
				//System.out.println ("5");
				Quantor qleft = (Quantor) left;
				Quantor qright = (Quantor) right;
				return deepEqual (qleft.expression, qright.expression)
							&& deepEqual (qleft.variable, qright.variable);
			}
			
			if (left instanceof Unary) {
				//System.out.println ("6");
				Unary uleft = (Unary) left;
				Unary uright = (Unary) right;
				return deepEqual (uleft.left, uright.left);
			}
			
			if (left instanceof Variable) {
				//System.out.println ("7");
				Variable vleft = (Variable) left;
				Variable vright = (Variable) right;
				return vleft.name.equals (vright.name);
			}
		}
		
		return false;
	}
	
	/*
	 	CLASS
		5.1
		CLASS
		7
		CLASS
		4
		CLASS
		4
		CLASS
		7
		CLASS
		7
		CLASS
		7
		
		/////
		CLASS
		5
		CLASS
		34
	 */
	
	///////////////////////////////
	
	public int matchProposalAxiom (Expression expression, Expression [] axioms) {
		//System.out.println (expression);
		
		for (int i = 0; i < axioms.length; i ++) {
			if (structEqual (expression, axioms [i], new HashMap <> ())) {
				return i + 1;
			}
		}
		
		Map <String, Expression> map = new HashMap <> ();
		if (expression instanceof Implication) {
			Implication imp = (Implication) expression;
			if (imp.left instanceof Any) {
				Any any = (Any) imp.left;
				
				if (structEqual (imp.right, any.expression, map)) {
					Expression back = expression.getInstance ();
					Expression tmp = getSubstitution (imp.left, any.variable, map.get (any.variable.name));
					expression = back;
					
					if (deepEqual (imp.right, ((Any) tmp).expression)) {
						Expression exp = map.get (any.variable.name);
						if (deepEqual (exp, any.variable)
								|| matchQuantor (any.expression, any.variable, false, false, exp)) {
							return 11;
						}
					} else {
						change = true;
						structEqual (imp.right, any.expression, map);
					}
				}
			}
		}
		
		map = new HashMap <> ();
		if (expression instanceof Implication) {
			Implication imp = (Implication) expression;
			if (imp.right instanceof Exist) {
				Exist exist = (Exist) imp.right;
				
				if (structEqual (imp.left, exist.expression, map)) {
					Expression back = expression.getInstance ();
					Expression tmp = getSubstitution (imp.right, exist.variable, map.get (exist.variable.name));
					expression = back;
					
					if (deepEqual (imp.left, ((Exist) tmp).expression)) {
						Expression exp = map.get (exist.variable.name);
						if (deepEqual (exp, exist.variable)
								|| matchQuantor (exist.expression, exist.variable, false, false, exp)) {
							return 12;
						}
					} else {
						change = true;
					}
				}
			}
		}
		
		return -1;
	}
	
	public int matchFormalAxiom (Expression expression, Expression [] axioms) {
		for (int j = 0; j < axioms.length; j++) {
            if (deepEqual (axioms [j], expression)) {
                return j + 1;
            }
        }
		
        return -1;
	}
	
	public Expression getSubstitution (Expression exp, Variable var, Expression sub) {
		if (exp instanceof Binary) {
			Binary bexp = (Binary) exp;
			bexp.left = getSubstitution (bexp.left, var, sub);
			bexp.right = getSubstitution (bexp.right, var, sub);
			
			return bexp;
		}
		
		if (exp instanceof Function || exp instanceof Predicate) {
			Function fexp = (Function) exp;
            fexp.vars = fexp.vars.stream ()
            						.map (e -> getSubstitution (e, var, sub))
            						.collect (Collectors.toCollection(ArrayList::new));
            return fexp;
        }
		
		if (exp instanceof Quantor) {
			Quantor qexp = (Quantor) exp;
			deepEqual (qexp.variable, sub);
			qexp.expression = getSubstitution (qexp.expression, var, sub);
			
			return qexp;
		}
		
		if (exp instanceof Unary) {
			Unary uexp = (Unary) exp;
			uexp.left = getSubstitution (uexp.left, var, sub);
			
			return uexp;
		}
		
		if (exp instanceof Variable) {
			if (deepEqual (exp, var)) { return sub; }
		}
		
		return exp;
	}
	
	public boolean matchQuantor (Expression exp, Variable var, boolean b1, boolean b2, Expression exp2) {
		if (exp instanceof Variable && deepEqual (exp, var)) { return b1 || !b2; }
		if (exp instanceof Variable && deepEqual (exp, var)) { return b1 || !b2; }
		
		if (exp instanceof Quantor && deepEqual (((Quantor) exp).variable, exp2)) {
			return matchQuantor (((Quantor) exp).expression, var, b1, true, exp2);
		}
		
		if (exp instanceof Quantor && deepEqual (((Quantor) exp).variable, var)) { return true; }
		
		if (exp instanceof Binary) {
			Binary bexp = (Binary) exp;
			return matchQuantor (bexp.left, var, b1, b2, exp2)
						&& matchQuantor (bexp.right, var, b1, b2, exp2);
		}
		
		if (exp instanceof Unary) {
			Unary uexp = (Unary) exp;
			return matchQuantor (uexp.left, var, b1, b2, exp2);
		}
		
		if (exp instanceof Predicate) {
			Predicate pexp = (Predicate) exp;
			return !(pexp.vars.contains (var)) || b1 || !b2;
		}
		
		return !(exp instanceof Function) || (((Function) exp).vars.contains (var) && !b1 && b2);
	}
	
	public int matchAny (Expression exp, List <Expression> exps, List <Expression> supts) {
		int index = -1;
		if (!(exp instanceof Implication)) return index;
		Implication iexp = (Implication) exp;
		
		for (int i = exps.size () - 1; i >= 0; i --) {
			Expression tmp = exps.get (i);
			if (tmp instanceof Implication && deepEqual (((Implication) tmp).left, iexp.left)) {
				Implication itmp = (Implication) tmp;
				if (iexp.right instanceof Any && deepEqual (itmp.right, ((Any) iexp.right).expression)) {
					Any aiexp = (Any) iexp.right;
					this.any = true;
					
					if (hasFreeInstance (aiexp.variable, iexp.left)) {
						int sz = supts.size ();
						if (!supts.isEmpty () && !hasFreeInstance (aiexp.variable, supts.get (sz - 1))) {
							this.anySub = true;
							return -1;
						}
						
						index = i;
						break;
					}
				}
			}
		}
		
		return index;
	}
	
	public int matchExist (Expression exp, List <Expression> exps, List <Expression> supts) {
		int index = -1;
		if (!(exp instanceof Implication)) return index;
		Implication iexp = (Implication) exp;
		
		for (int i = exps.size () - 1; i >= 0; i --) {
			Expression tmp = exps.get (i);
			//System.out.println (i + " > > > ");
			//System.out.println ("TMP: " + tmp);
			//System.out.println ((tmp instanceof Implication ? ((Implication) tmp).right : ""));
			if (tmp instanceof Implication && deepEqual (((Implication) tmp).right, iexp.right)) {
				Implication itmp = (Implication) tmp;
				if (iexp.left instanceof Exist && deepEqual (itmp.left, ((Exist) iexp.left).expression)) {
					Exist eiexp = (Exist) iexp.left;
					this.ext = true;
					
					if (hasFreeInstance (eiexp.variable, iexp.right)) {
						int sz = supts.size ();
						if (!supts.isEmpty () && !hasFreeInstance (eiexp.variable, supts.get (sz - 1))) {
							this.anySub = true;
							return -1;
						}
						
						index = i;
						//System.out.println ("I: " + index + " exp: " + exp);
						break;
					}
				}
			}
		}
		
		//System.out.println ("I: " + index + " exp: " + exp);
		return index;
	}
	
	public boolean hasFreeInstance (Variable var, Expression exp) {
		return matchFreeInstance (var, exp, false);
	}
	
	public boolean matchFreeInstance (Variable var, Expression exp, boolean isQuantor) {
		if (exp instanceof Binary) {
			Binary bexp = (Binary) exp;
			return matchFreeInstance (var, bexp.left, isQuantor)
						&& matchFreeInstance (var, bexp.right, isQuantor);
		}
		
		if (exp instanceof Predicate || exp instanceof Function) {
			boolean flag = false;
			Function fexp = (Function) exp;
			for (int i = 0; i < fexp.vars.size (); i ++) {
				if (!(matchFreeInstance (var, fexp.vars.get (i), isQuantor))) {
					flag = true;
					break;
				}
			}
			
			return !flag || isQuantor;
		}
		
		if (exp instanceof Quantor) {
			Quantor qexp = (Quantor) exp;
			return deepEqual (qexp.variable, var) 
						|| matchFreeInstance (var, qexp.expression, isQuantor);
		}
		
		if (exp instanceof Unary) {
			Unary uexp = (Unary) exp;
			return matchFreeInstance (var, uexp.left, isQuantor);
		}
		
		if (exp instanceof Variable && deepEqual (exp, var)) { /*System.out.println ("1");*/ return isQuantor; }
		
		return true;
	}
	
	public boolean finalCheck (Expression e) {
		if (e instanceof Implication) {
			Implication iexp = (Implication) e;
			if (iexp.left instanceof And 
						&& ((And) iexp.left).right instanceof Any) {
				HashMap <String, Expression> map = new HashMap<>();
	            if (structEqual (((And) iexp.left).left, iexp.right, map)) {
	                Variable var = ((Any) ((And) iexp.left).right).variable;
	                if (map.get (var.name) instanceof Const) {
	                    Expression tmp = e.getInstance ();
	                    Expression sub = getSubstitution (((Implication) tmp).right, var, map.get (var.name));
	                    if (deepEqual (sub, ((And) ((Implication) tmp).left).left)) {
	                        map.clear ();
	                        Expression temp = ((Any) ((And) ((Implication) tmp).left).right).expression;
	                        
	                        if (temp instanceof Implication) {
	                        	Implication itemp = (Implication) temp;
	                        	if (structEqual (itemp.right, itemp.left, map)) {
	                        		return true;
	                        	}
	                        }
	                    }
	                }
	            }
			}
        }
		
        return false;
	}
	
}
