package ru.shemplo.hw.src.implementor;

import static java.nio.charset.StandardCharsets.UTF_8;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Implementor_dep {
	
	public void implement (Class <?> token, Path root) {
		if (token == null) {
			String message = "Class type-token can't be null";
			throw new IllegalArgumentException (message);
		} else if (root == null) {
			String message = "Path to destination folder can't be null";
			throw new IllegalArgumentException (message);
		} else if (token.isEnum () || token == Enum.class) {
			// Challenge: enum class can't be implemented
			String message = "Enum class can't be extended";
			throw new IllegalArgumentException (message);
		} else if (token.isArray ()) {
			String message = "Array is not a class-type token and can't be implemented";
			throw new IllegalArgumentException (message);
		} else if (Modifier.isFinal (token.getModifiers ())) {
			String message = "Final class can't be extended";
			throw new IllegalArgumentException (message);
		}
		
		// Also here must be check for a isPrimitive () but it's not necessary
		// because all primitive types (there are 9 of them) declared as final
		
		// Challenge: default package == null
		Package pkg = token.getPackage ();
		File destination = root.toFile ();
		if (pkg != null) {
			String path = pkg.getName ().replace (".", File.separator);
			destination = new File (root.toFile (), path);
		}
		destination.mkdirs ();
		
		String fileName = token.getSimpleName () + "Impl.java";
		File implFile = new File (destination, fileName);
		try (
			OutputStream os = new FileOutputStream (implFile);
			Writer writer = new OutputStreamWriter (os, UTF_8);
		) {
			String code = generateClassCode_dep (token);
			writer.write (code);
			writer.flush ();
			
			generateClasCode (token);
		} catch (IOException ioe) {
			
		}
	}
	
	private final String generateClasCode (Class <?> token) {
		HeredityGraph graph = new HeredityGraph (token);
		System.out.println (graph.getSuperClasses ());
		System.out.println (graph.getInterfaces ());
		
		System.out.println (graph.getUnimplementedMethods ());
		return "";
	}
	
	private static class HeredityGraph {
		
		private final Map <Class <?>, Node> KNOWN;
		private final Node ROOT;
		
		public HeredityGraph (Class <?> from) {
			this.KNOWN = new HashMap <> ();
			this.ROOT = makeNode (from);
			ROOT.propagateTypeParameters ();
		}
		
		@Override
		public String toString () {
			return ROOT != null ? ROOT.toString () : "Empty";
		}
		
		public List <Class <?>> getInterfaces () {
			List <Class <?>> list = new ArrayList <> ();
			Set <Class <?>> known = new HashSet <> ();
			if (ROOT == null) { return list; }
			
			Queue <Node> queue = new LinkedList <> ();
			queue.add (ROOT);
			
			while (!queue.isEmpty ()) {
				Node node = queue.poll ();
				if (node.EXTENDS != null) {
					queue.add (node.EXTENDS);
				}
				
				if (node.IMPLEMENT == null) {
					continue;
				}
				
				for (Node impl : node.IMPLEMENT) {
					if (known.contains (impl.TOKEN)) {
						continue;
					}
					
					known.add (impl.TOKEN);
					list.add (impl.TOKEN);
					queue.add (impl);
				}
			}
			
			return list;
		}
		
		public List <Class <?>> getSuperClasses () {
			List <Class <?>> list = new ArrayList <> ();
			Set <Class <?>> known = new HashSet <> ();
			if (ROOT == null) { return list; }
			
			Queue <Node> queue = new LinkedList <> ();
			queue.add (ROOT);
			
			while (!queue.isEmpty ()) {
				Node node = queue.poll ();
				if (node.EXTENDS == null) {
					continue;
				}
				
				if (!known.contains (node.EXTENDS.TOKEN)) {
					known.add (node.EXTENDS.TOKEN);
					list.add (node.EXTENDS.TOKEN);
					queue.add (node.EXTENDS);
				}
			}
			
			return list;
		}
		
		@SuppressWarnings ("unused")
		public Map <String, String> getTypeParametersTranslation () {
			Map <String, String> transes = new HashMap <> ();
			
			Queue <Node> queue = new LinkedList <> ();
			queue.add (ROOT);
			
			while (!queue.isEmpty ()) {
				Node node = queue.poll ();
			}
			
			return transes;
		}
		
		public List <Method> getUnimplementedMethods () {
			List <Method> methods = new ArrayList <> ();
			//List <Class <?>> intfs = getInterfaces ();
			return methods;
		}
		
		private Node makeNode (Class <?> token) {
			if (token == null) { return null; }
			if (KNOWN.containsKey (token)) {
				return KNOWN.get (token);
			}
			
			Class <?> ext = token.getSuperclass ();
			if (ext == Object.class) { ext = null; }
			
			Class <?> [] impls = token.getInterfaces ();
			List <Node> intfs = new ArrayList <> ();
			for (Class <?> impl : impls) {
				Node node = makeNode (impl);
				if (node != null) { intfs.add (node); }
			}
			
			Node node = new Node (token, makeNode (ext), intfs);
			KNOWN.put (token, node);
			return node;
		}
		
		private static class Node {
			
			public final List <Node> IMPLEMENT;
			public final Class <?> TOKEN;
			public final Node EXTENDS;
			
			public Node (Class <?> token, Node exts, List <Node> impls) {
				this.IMPLEMENT = Collections.unmodifiableList (impls);
				this.EXTENDS = exts;
				this.TOKEN = token;
			}
			
			public void propagateTypeParameters () {
				TypeVariable <?> [] params = TOKEN.getTypeParameters ();
				if (params == null || params.length == 0) { return; }
				
				List <String> paramNames = new ArrayList <> ();
				for (int i = 0; i < params.length; i ++) {
					paramNames.add (params [i].getName ());
				}
			}
			
			@SuppressWarnings ("unused")
			public void propagateTypeParameter (String name, String value) {
				
			}
			
			@Override
			public String toString () {
				StringBuilder sb = new StringBuilder ();
				sb.append ("Class: ");
				sb.append (TOKEN);
				if (EXTENDS != null) {
					sb.append ("\n");
					sb.append ("Extends: ");
					sb.append (EXTENDS);
				}
				
				if (IMPLEMENT != null && IMPLEMENT.size () > 0) {
					sb.append ("\n");
					sb.append ("Implements:\n");
					for (Node node : IMPLEMENT) {
						sb.append (node);
						sb.append ("\n");
					}
				}
				sb.append ("\n");
				return sb.toString ();
			}
			
		}
		
	}
	
	public static final List <String> fetchClasses (String string) {
		List <String> classes = new ArrayList <> ();
		if (string == null || string.length () == 0) {
			return classes;
		}
		
		Pattern classPattern = Pattern.compile ("([\\w]+(\\.[\\w]+)+(\\$[\\w]+)?)", 
												Pattern.UNICODE_CHARACTER_CLASS);
		
		Matcher matcher = classPattern.matcher (string);
		while (matcher.find ()) { classes.add (matcher.group (0)); }
		
		return classes;
	}
	
	private final String generateClassCode_dep (Class <?> token) {
		StringBuilder sb = new StringBuilder ();
		Package pkg = token.getPackage ();
		if (pkg != null) {
			sb.append ("package ");
			sb.append (pkg.getName ());
			sb.append (";\n\n");
		}
		
		List <Class <?>> imports = getImportClasses_dep (token);
		for (Class <?> type : imports) {
			sb.append ("import ");
			sb.append (type.getCanonicalName ());
			sb.append (";\n");
		}
		sb.append ("\n");
		
		sb.append (getClassDeclaration_dep (token));
			Method [] methods = null;
			try {
				methods = token.getDeclaredMethods ();
			} catch (SecurityException se) {
				// This can cause SE only for package access
				// (it was checked before -> OK)
				methods = token.getMethods ();
			}
			
			for (Method method : methods) {
				int mods = method.getModifiers ();
				if (Modifier.isFinal (mods)
						|| Modifier.isNative (mods)
						|| Modifier.isPrivate (mods)) {
					continue;
				}
				
				sb.append ("\t");
				sb.append (Modifier.toString (mods & ~Modifier.ABSTRACT));
				sb.append (" ");
				
				String typeParams = getMethodTypeParameters_dep (method);
				if (typeParams.length () > 0) {
					sb.append (typeParams);
					sb.append (" ");
				}
				
				Type type = method.getGenericReturnType ();
				sb.append (simplifyEntries_dep (type.getTypeName ()));
				sb.append (" ");
				
				sb.append (method.getName ());
				sb.append (" (");
				Type [] params = method.getGenericParameterTypes ();
				for (int i = 0; i < params.length; i++) {
					type = params [i];
					String paramType = simplifyEntries_dep (type.getTypeName ());
					if (i == params.length - 1 && method.isVarArgs ()) {
						paramType = paramType.substring (0, paramType.length () - 2);
						paramType += "...";
					}
					
					sb.append (paramType);
					sb.append (" arg");
					sb.append (i);
					
					if (i < params.length - 1) {
						sb.append (", ");
					}
				}
				sb.append (") ");
				
				Type [] exceptions = method.getGenericExceptionTypes ();
				if (exceptions != null && exceptions.length > 0) {
					sb.append ("throws ");
				}
				
				for (int i = 0; i < exceptions.length; i++) {
					type = exceptions [i];
					sb.append (simplifyEntries_dep (type.getTypeName ()));
					if (i < exceptions.length - 1) {
						sb.append (", ");
					} else {
						sb.append (" ");
					}
				}
				
				sb.append ("{\n");
				sb.append ("\t\t");
				sb.append (getDefaultValue_dep (method.getReturnType ()));
				sb.append ("\n");
				sb.append ("\t}\n\t\n");
			}
		sb.append ("}\n");
		
		return sb.toString ();
	}
	
	private final List <Class <?>> getImportClasses_dep (Class <?> token) {
		Set <Class <?>> imports = new HashSet <> ();
		List <String> classes = new ArrayList <> ();
		
		Method [] methods = token.getDeclaredMethods ();
		for (Method method : methods) {
			Type type = method.getGenericReturnType ();
			String type2String = type.getTypeName ();
			classes.addAll (fetchClasses (type2String));
			
			Type [] types = method.getGenericParameterTypes ();
			for (int i = 0; i < types.length; i ++) {
				type = types [i]; type2String = type.getTypeName ();
				classes.addAll (fetchClasses (type2String));
			}
			
			types = method.getGenericExceptionTypes ();
			for (int i = 0; i < types.length; i ++) {
				type = types [i]; type2String = type.getTypeName ();
				classes.addAll (fetchClasses (type2String));
			}
		}
		
		TypeVariable <?> [] params = token.getTypeParameters ();
		if (params != null) {
			for (TypeVariable <?> param : params) {
				Type [] types = param.getBounds ();
				for (int i = 0; i < types.length; i++) {
					Type type = types [i]; 
					String type2String = type.getTypeName ();
					classes.addAll (fetchClasses (type2String));
				}
			}
		}
		
		for (String cl : classes) {
			try {
				imports.add (Class.forName (cl));
			} catch (ClassNotFoundException cnfe) {
				// This is not important
				// It means that fetched string is not a class
				// or such class is not defined
			}
		}
		
		return new ArrayList <> (imports);
	}
	
	private String getClassDeclaration_dep (Class <?> token) {
		StringBuilder sb = new StringBuilder ();
		sb.append ("public class ");
		sb.append (token.getSimpleName ());
		sb.append ("Impl ");
		
		String params = getClassTypeParameters_dep (token, true);
		if (params.length () > 0) {
			sb.append (params);
			sb.append (" ");
		}
		
		if (params.length () > 64) {
			sb.append ("\n\t");
		}
		
		if (token.isInterface ()) {
			sb.append ("implements ");
		} else {
			sb.append ("extends ");
		}
		
		sb.append (token.getSimpleName ());
		sb.append (" ");
		
		params = getClassTypeParameters_dep (token, false);
		if (params.length () > 0) {
			sb.append (params);
			sb.append (" ");
		}
		
		sb.append ("{\n\n");
		
		return sb.toString ();
	}
	
	private String getClassTypeParameters_dep (Class <?> token, boolean bounds) {
		TypeVariable <?> [] params = token.getTypeParameters ();
		return getTypeParametrs_dep (params, bounds);
	}
	
	private String getMethodTypeParameters_dep (Method method) {
		TypeVariable <?> [] params = method.getTypeParameters ();
		return getTypeParametrs_dep (params, true);
	}
	
	private String getTypeParametrs_dep (TypeVariable <?> [] params, boolean bounds) {
		StringBuilder sb = new StringBuilder ();
		if (params != null && params.length > 0) {
			sb.append ("<");
			int current = 0;
			for (TypeVariable <?> param : params) {
				sb.append (param.getName ());
				Type [] types = param.getBounds ();
				if (bounds && types != null && types.length > 0) {
					sb.append (" extends ");
					for (int i = 0; i < types.length; i++) {
						Type type = types [i];
						sb.append (simplifyEntries_dep (type.getTypeName ()));
						if (i < types.length - 1) { sb.append (" & "); }
					}
				}
				
				if (current < params.length - 1) {
					sb.append (", ");
				}
				
				current ++;
			}
			sb.append (">");
		}
		
		return sb.toString ();
	}
	
	private String simplifyEntries_dep (String string) {
		List <String> classes = fetchClasses (string);
		for (String cl : classes) {
			try {
				Class <?> dummy = Class.forName (cl);
				string = string.replace (cl, dummy.getSimpleName ());
			} catch (ClassNotFoundException cnfe) {}
		}
		
		return string;
	}
	
	private String getDefaultValue_dep (Class <?> token) {
		if (token == void.class) {
			return "";
		} else if (token == boolean.class) {
			return "return false;";
		} else if (token.isPrimitive ()) {
			return "return 0;";
		} else {
			return "return null;";
		}
	}
	
}
