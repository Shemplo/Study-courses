package ru.shemplo.hw.src.implementor;

import java.nio.file.Path;
import java.util.List;

public class Implementor {

	public void implement (Class <?> token, Path root) {
		ImplementTree tree = new ImplementTree (token);
		List <MethodDeclaration> methods = tree.getMethods ();
		for (MethodDeclaration declaration : methods) {
			System.out.println (GenericParser.simplifyString (declaration.toString ()).getKey ());
		}
	}
	
}
