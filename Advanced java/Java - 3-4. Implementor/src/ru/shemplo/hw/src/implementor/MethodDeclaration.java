package ru.shemplo.hw.src.implementor;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class MethodDeclaration {

	private static final String OBJ_NAME = Object.class.getName ();
	
	private List <Generic> ARGUMENTS, PARAMETERS;
	private List <String> PARAMS_NAME;
	private Generic RETURN;
	private int MODIFIERS;
	
	public final boolean HAS_THROW;
	private final Method METHOD;
	
	private boolean indep;
	
	public MethodDeclaration (Method method) {
		this.METHOD = method;
		
		this.HAS_THROW = METHOD.getExceptionTypes ().length > 0;
		
		String returnName = METHOD.getGenericReturnType ().getTypeName ();
		this.RETURN = GenericParser.parseGeneric (returnName);
		
		TypeVariable <?> [] params = METHOD.getTypeParameters ();
		this.PARAMS_NAME = new ArrayList <> ();
		this.PARAMETERS = new ArrayList <> ();
		for (TypeVariable <?> type : params) { 
			Type [] bounds = type.getBounds ();
			String boundsName = boundsToString (bounds);
			if (bounds.length == 1 
					&& bounds [0].getTypeName ().equals (OBJ_NAME)) {
				boundsName = ""; // Parameter has no bounds
			}
			
			String typeName = type.getName ();
			PARAMS_NAME.add (typeName);
			
			if (boundsName.length () > 0) {
				typeName += " extends " + boundsToString (bounds);
			}
			
			Generic generic = GenericParser.parseGeneric (typeName);
			PARAMETERS.add (generic);
		}
		
		Type [] args = METHOD.getGenericParameterTypes ();
		this.ARGUMENTS = new ArrayList <> ();
		for (Type type : args) {
			String typeName = type.getTypeName ();
			Generic generic = GenericParser.parseGeneric (typeName);
			ARGUMENTS.add (generic);
		}
		
		this.MODIFIERS = METHOD.getModifiers () & ~Modifier.ABSTRACT;
		this.indep = _isIndependent ();
	}
	
	public void replace (String name, Generic generic) {
		if (RETURN.getName ().equals (name)) {
			RETURN = generic;
		} else {
			RETURN.replace (name, generic);
		}
		
		for (int i = 0; i < ARGUMENTS.size (); i++) {
			Generic argument = ARGUMENTS.get (i);
			if (argument.getName ().equals (name)) {
				ARGUMENTS.set (i, generic);
			} else {
				argument.replace (name, generic);
			}
		}
		
		this.indep = _isIndependent ();
	}
	
	@Override
	public String toString () {
		StringBuilder sb = new StringBuilder ();
		sb.append (Modifier.toString (MODIFIERS));
		sb.append (" ");
		if (PARAMS_NAME.size () > 0) {
			sb.append ("<");
			for (int i = 0; i < PARAMETERS.size (); i++) {
				sb.append (PARAMETERS.get (i).toCode ());
				if (i < PARAMETERS.size () -  1) {
					sb.append (", ");
				}
			}
			sb.append ("> ");
		}
		
		sb.append (RETURN.toCode ());
		sb.append (" ");
		
		sb.append (METHOD.getName ());
		sb.append (" (");
		for (int i = 0; i < ARGUMENTS.size (); i++) {
			sb.append (ARGUMENTS.get (i).toCode ());
			sb.append (" ");
			sb.append ("arg");
			sb.append (i);
			
			if (i < ARGUMENTS.size () - 1) {
				sb.append (", ");
			}
		}
		sb.append (") ");
		
		if (HAS_THROW) {
			sb.append ("throw Exception");
		}
		
		return sb.toString ();
	}
	
	private String boundsToString (Type [] bounds) {
		StringBuilder sb = new StringBuilder ();
		for (int i = 0; i < bounds.length; i++) {
			Type type = bounds [i];
			sb.append (type.getTypeName ());
			
			if (i < bounds.length - 1) {
				sb.append (" & ");
			}
		}
		return sb.toString ();
	}
	
	public boolean isIndependent () { return indep; }
	
	private boolean _isIndependent () {
		if (PARAMS_NAME.size () > 0) {
			List <String> names = RETURN.getParametersName ();
			if (!isSubset (PARAMS_NAME, names)) {
				return false;
			}
			
			for (Generic generic : ARGUMENTS) {
				names = generic.getParametersName ();
				if (!isSubset (PARAMS_NAME, names)) {
					return false;
				}
			}
		} else {
			if (!RETURN.isConstant ()) {
				return false; // Because there is a parameter
			}
			
			for (Generic generic : ARGUMENTS) {
				if (!generic.isConstant ()) {
					return false; // Because there is a parameter
				}
			}
		}
		
		return true;
	}
	
	private boolean isSubset (List <String> a, List <String> b) {
		if (a == null || b == null) { return false; }
		
		Set <String> tmp = new HashSet <> (a);
		for (String string : b) {
			if (!tmp.contains (string)) {
				return false;
			}
		}
		
		return true;
	}
	
}
