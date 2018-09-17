package ru.shemplo.hw.src.implementor;

import java.util.ArrayList;
import java.util.List;

public class Generic {
	
	private static enum GenericType {
		PARAMETER, ARGUMENT, TUPLE;
	}
	
	private final List <Generic> GENERICS;
	private final GenericType TYPE;
	private Class <?> CLASS;
	private String NAME;
	
	public Generic (String name, Generic bounds) {
		Class <?> type = Utils.getClass (name);
		if (type == null) {
			this.TYPE = GenericType.PARAMETER;
		} else {
			this.TYPE = GenericType.ARGUMENT;
			this.CLASS = type;
		}
		
		this.GENERICS = new ArrayList <> ();
		if (bounds != null) {
			GENERICS.add (bounds);
		}
		
		this.NAME = name;
	}
	
	public Generic (String name) {
		this (name, null);
	}
	
	public Generic (String name, char split, List <Generic> generics) {
		this.GENERICS = new ArrayList <> (generics);
		this.TYPE = GenericType.TUPLE;
		this.NAME = name;
	}
	
	@Override
	public String toString () {
		return toString ("");
	}
	
	private String toString (String prefix) {
		StringBuilder sb = new StringBuilder ();
		sb.append (prefix);
		sb.append (">> ");
		sb.append (TYPE);
		sb.append (" generic\n");
		
		sb.append (prefix);
		sb.append ("~> Name: ");
		sb.append (NAME);
		sb.append ("\n");
		
		if (GENERICS.size () > 0) {
			sb.append (prefix);
			sb.append ("+> Generics:\n");
			for (Generic generic : GENERICS) {
				if (generic == null) { continue; }
				sb.append (generic.toString (prefix + "| "));
			}
		}
		
		return sb.toString ();
	}
	
	public String toCode () {
		StringBuilder sb = new StringBuilder ();
		switch (TYPE) {
			case ARGUMENT:
				sb.append (NAME);
				if (GENERICS.size () > 0) {
					sb.append (" ");
					sb.append (GENERICS.get (0).toCode ());
				}
				break;
				
			case PARAMETER:
				sb.append (NAME);
				if (GENERICS.size () > 0) {
					sb.append (" ");
					sb.append (GENERICS.get (0).toCode ());
				}
				break;
				
			case TUPLE:
				if (NAME.equals ("tuple")) {
					sb.append ("<");
					for (int i = 0; i < GENERICS.size (); i++) {
						sb.append (GENERICS.get (i).toCode ());
						if (i < GENERICS.size () - 1) {
							sb.append (", ");
						}
					}
					sb.append (">");
				} else {
					sb.append (NAME);
					sb.append (" ");
					for (int i = 0; i < GENERICS.size (); i++) {
						sb.append (GENERICS.get (i).toCode ());
						if (i < GENERICS.size () - 1) {
							sb.append (" & ");
						}
					}
				}
				break;
		}
		return sb.toString ();
	}
	
	public boolean isConstant () {
		if (TYPE.equals (GenericType.PARAMETER)) {
			return false;
		}
		
		for (Generic generic : GENERICS) {
			if (!generic.isConstant ()) {
				return false;
			}
		}
		
		return true;
	}
	
	public List <String> getParametersName () {
		List <String> params = new ArrayList <> ();
		if (TYPE.equals (GenericType.PARAMETER)
				&& GENERICS.size () == 0) {
			// It must be a parameter entry 
			// -> not declaration
			params.add (NAME);
		}
		
		for (int i = 0; i < GENERICS.size (); i++) {
			Generic child = GENERICS.get (i);
			params.addAll (child.getParametersName ());
		}
		
		return params;
	}
	
	public void replace (String name, Generic generic) {
		for (int i = 0; i < GENERICS.size (); i++) {
			Generic child = GENERICS.get (i);
			if (child.TYPE.equals (GenericType.PARAMETER) 
					&& child.NAME.equals (name)) {
				GENERICS.set (i, generic);
			} else { child.replace (name, generic); }
		}
	}
	
	public List <Generic> getTypeParameters (Class <?> token) {
		if (TYPE.equals (GenericType.ARGUMENT) && this.CLASS == token) {
			return GENERICS.get (0).splitToSimple ();
		}
		
		return null;
	}
	
	public List <Generic> splitToSimple () {
		List <Generic> simple = new ArrayList <> ();
		if (!TYPE.equals (GenericType.TUPLE)) {
			simple.add (this);
		} else { simple.addAll (GENERICS); }
		
		return simple;
	}
	
	public String getName () {
		return NAME;
	}
	
}
