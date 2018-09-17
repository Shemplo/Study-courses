package ru.shemplo.hw.src.walk;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;
import java.security.NoSuchAlgorithmException;
import java.util.Objects;

import com.sun.org.apache.xpath.internal.functions.WrongNumberArgsException;

import ru.shemplo.hw.src.hash.Hash;

public class WalkNR {

	private static Writer writer;
	
	public static void main (String... args) throws WrongNumberArgsException, 
													FileNotFoundException,
													IOException {
		// Possible that send Error messages would be better than cause exceptions
		// So I mostly prefer to do the second one due to stack trace
		
		if (args == null || args.length == 0) { // Check for null required b/c it's varArg
			throw new WrongNumberArgsException ("Two arguments was expected: <input file> <output file>");
			// System.err.println ("Two arguments was expected: <input file> <output file>");
		} else if (args.length == 1) {
			throw new WrongNumberArgsException ("One more argument was expected: <output file>");
			// System.err.println ("One more argument was expected: <output file>");
		}
		
		// Preventing situation of NPE caused by wrong arguments
		Objects.requireNonNull (args [0], "wrong path to input file");
		Objects.requireNonNull (args [1], "wrong path to output file");
		
		File inputFile = new File (args [0]),
				outputFile = new File (args [1]);
		
		try (
			OutputStream os = new FileOutputStream (outputFile);
			// It's important what chars we gonna write to an output file b/c
			// in path can be unicode chars (2 bytes) such as Russian letters
			Writer w = new OutputStreamWriter (os, "UTF-8");
		) {
			// Assign a using writer
			WalkNR.writer = w;
			
			try (
				InputStream is = new FileInputStream (inputFile);
					
				// It's important what chars we gonna read from an input file b/c
				// in path can be unicode chars (2 bytes) such as Russian letters
				Reader r = new InputStreamReader (is, "UTF-8");
				BufferedReader br = new BufferedReader (r);
			) {
				String line;
				
				while ((line = br.readLine ()) != null) {
					// Initialization of start file or directory
					File start = new File (line);
					
					// Run file system walker
					walk (start);
				}
			}
			/* Here can be catch for each Exception:
			 * catch (NoSuchFileException nsfe) {
			 * 	System.err.println ("Input file doesn't exist");
			 * } catch (SecurityException se) {
			 * 	System.err.println ("Access to input file is denied");
			 * } catch (IOException ioe) {
			 * 	System.err.println ("Failed to read from input file");
			 * }
			 */ 
		}
		/* Here can be catch for each Exception:
		 * catch (SecurityException se) {
		 * 	System.err.println ("Access to output file is denied");
		 * } catch (IOException ioe) {
		 * 	System.err.println ("Failed to write in output file");
		 * }
		 */
	}
	
	private static void walk (File file) throws IOException {
		if (file.exists () && file.isDirectory ()) { // It's a folder
			// Getting a list of file names inside a folder
			// Doesn't used `listFiles` from considerations, that
			// directory may contains too many files and no reasons
			// to create descriptors of all of them at one time
			String [] files = file.list ();
			// Check if this folder is not empty
			if (files == null) { return; }

			for (String fileName : files) {
				// Creating new descriptor to a folder
				File levelFile = new File (file, fileName);
				
				// Go inside the folder
				walk (levelFile);
			}
		} else if (file.exists () && file.isFile ()) { // It's a file
			try {
				// Calculating hash from file descriptor
				String hash = Hash.hash ("fnv", file);
				writer.append (hash);
			} catch (IOException ioe) {
				writer.append (Hash.DEFAULT_HASH);
			} catch (NoSuchAlgorithmException e) {
				// That's impossible ... just ignore
			} finally {
				writer.append (' ').append (file.toString ())
						.append (System.lineSeparator ());
			}
		} else if (file != null) { // Only God knows what it is;
			writer.append (Hash.DEFAULT_HASH);
			writer.append (' ').append (file.toString ())
					.append (System.lineSeparator ());
		}
	}
	
}
