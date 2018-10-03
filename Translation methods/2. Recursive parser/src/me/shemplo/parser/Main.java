package me.shemplo.parser;

import com.sun.org.apache.xerces.internal.impl.xpath.regex.ParseException;

public class Main {

	public static void main (String... args) {
		try {
			System.out.println (Parser.parse ("int[] a;"));
			System.out.println (Parser.parse ("int[][] a;"));
			System.out.println (Parser.parse ("int[][] a, *b, **c;"));
			System.out.println (Parser.parse ("int[][] a; double[] *b, **c;"));
		} catch (ParseException pe) {
			System.err.println (pe.getMessage () + pe.getLocation ());
		}
		//System.out.println (Parser.parse ("int a;"));
		//System.out.println (Parser.parse ("int a, *b, ***c;"));
		//System.out.println (Parser.parse ("int ***a, ******b, ***c;"));
		//System.out.println (Parser.parse ("int ***a, ******b, ***c; int x, **y, ********z;"));
		//System.out.println (Parser.parse ("int *a, b; double c, *d; float e;"));
		//System.out.println (Parser.parse ("int ***a, ******b, ***c; double n, *m;"));
		//System.out.println (Parser.parse ("int a, *b, **c; double n, *m;"));
	}
	
}
