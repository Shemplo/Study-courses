package me.shemplo.mathlogic.homework3;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.util.NoSuchElementException;
import java.util.Scanner;

public class Main {
	
	private static final String TEMP_PROOF = "support/template.proof";
	private static final String PROOF = "proof.out";
	private static final String INPUT = "proof.in";
	
	private static BufferedReader brP = null;
	private static PrintWriter pw = null;
	
	public static void main (String [] args) throws Exception {
		File tempP = new File (TEMP_PROOF);
		File proof = new File (PROOF);
		File input = new File (INPUT);
		
		brP = new BufferedReader (
				new InputStreamReader (
					new FileInputStream (tempP), "UTF-8"));
		pw = new PrintWriter (
				new OutputStreamWriter (
					new FileOutputStream (proof), "UTF-8"));
		////////////////////////////////////////////////////
		
		int a = 0;
		try (Scanner sc = new Scanner (input)) {
			a = Integer.parseInt (sc.next ());
		} catch (NoSuchElementException nsee) {
			System.err.println ("[ERROR] Number is not found in input file");
			closeStreams ();
			System.exit (1);
		} catch (NumberFormatException nfe) {
			System.err.println ("[ERROR] Number must have an integer value");
			closeStreams ();
			System.exit (1);
		}
		
		if (a < 0) {
			System.err.println ("[ERROR] Number's value must be positive");
			closeStreams ();
			System.exit (1);
		}
		System.out.println ("[LOG] Generating proof for a = " + a);
		
		String number = makeNumber (a), tmp = "";
		pw.println ("|-(_+0')*(_+0')=(_*_)+(0''*_)+0'".replaceAll ("_", number));
		while ((tmp = brP.readLine ()) != null) { pw.println (tmp); }
		
		String predicate = "@a((a+0')*(a+0')=(a*a)+(0''*a)+0')->"
								+ ("((_+0')*(_+0')=(_*_)+(0''*_)+0')".replaceAll ("_", number));
		pw.println (predicate.replaceAll ("_", number));
		pw.println ("((_+0')*(_+0')=(_*_)+(0''*_)+0')".replaceAll ("_", number));
		
		System.out.println ("[LOG] Completed");
		closeStreams ();
	}
	
	private static String makeNumber (int number) {
		StringBuilder sb = new StringBuilder ("0");
		for (int i = 0; i < number; i ++) { sb.append ("'"); }
		return sb.toString ();
	}
	
	private static void closeStreams () throws Exception {
		brP.close ();
		pw.close ();
	}
	
}
