package ru.shemplo.hw.src.implementor;

import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

public class ImplementTree {

	private final Node ROOT;
	
	public ImplementTree (Class <?> token) {
		this.ROOT = new Node (token);
	}
	
	public List <MethodDeclaration> getMethods () {
		return ROOT.getMethods ();
	}
	
	public static class Node {
		
		private final List <MethodDeclaration> METHODS;
		private final List <String> GENERICS_NAMES;
		
		private final List <Generic> SUPER_GENERICS;
		private final List <Node> SUPER;
		
		private final Class <?> TOKEN;
		
		public Node (Class <?> token) {
			//System.out.println (token);
			this.GENERICS_NAMES = new ArrayList <> ();
			this.SUPER_GENERICS = new ArrayList <> ();
			this.METHODS = new ArrayList <> ();
			this.SUPER = new ArrayList <> ();
			this.TOKEN = token;
			
			TypeVariable <?> [] params = token.getTypeParameters ();
			Stream.of (params).forEach (p -> GENERICS_NAMES.add (p.getName ()));
			
			Class <?> sp = token.getSuperclass ();
			if (sp != null && sp != Object.class) {
				SUPER.add (new Node (sp));
				
				Type type = token.getGenericSuperclass ();
				if (!type.getTypeName ().equals (Object.class.getName ())) {
					String genericName = type.getTypeName ();
					Generic g = GenericParser
								.parseGeneric (genericName);
					SUPER_GENERICS.add (g);
				}
			}
			
			Type [] intfs_gen = token.getGenericInterfaces ();
			Class <?> [] intfs = token.getInterfaces ();
			for (int i = 0; i < intfs.length; i ++) {
				SUPER.add (new Node (intfs [i]));
				
				Type type = intfs_gen [i];
				if (!type.getTypeName ().equals (intfs [i].getName ())) {
					String genericName = type.getTypeName ();
					Generic g = GenericParser
								.parseGeneric (genericName);
					SUPER_GENERICS.add (g);
				} else { SUPER_GENERICS.add (null); }
			}
			
			for (Method method : token.getDeclaredMethods ()) {
				METHODS.add (new MethodDeclaration (method));
			}
			
			for (int i = 0; i < SUPER.size (); i++) {
				Node node = SUPER.get (i);
				List <String> PARAMS_NAME = node.GENERICS_NAMES;
				if (PARAMS_NAME.size () == 0) { continue; }
				
				Generic superParam = SUPER_GENERICS.get (i);
				List <Generic> simple = superParam.getTypeParameters (node.TOKEN);
				for (int j = 0; j < PARAMS_NAME.size (); j++) {
					// Propagating local type to one level upper
					node.replace (PARAMS_NAME.get (j), simple.get (j));
				}
			}
		}
		
		public void replace (String name, Generic generic) {
			for (MethodDeclaration method : METHODS) {
				method.replace (name, generic);
			}
			
			for (int i = 0; i < SUPER.size (); i++) {
				SUPER.get (i).replace (name, generic);
			}
		}
		
		public List <MethodDeclaration> getMethods () {
			List <MethodDeclaration> methods = new ArrayList <> (METHODS);
			for (Node sup : SUPER) { methods.addAll (sup.getMethods ()); }
			return methods;
		}
		
	}
	
}
