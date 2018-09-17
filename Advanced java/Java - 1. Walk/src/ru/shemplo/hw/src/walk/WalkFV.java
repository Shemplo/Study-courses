package ru.shemplo.hw.src.walk;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.security.NoSuchAlgorithmException;
import java.util.Objects;

import com.sun.org.apache.xpath.internal.functions.WrongNumberArgsException;

import ru.shemplo.hw.src.hash.Hash;

public class WalkFV {
	
	private static WritingFileVisitor <Path> visitor;
	
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
		
		Objects.requireNonNull (args [0], "wrong path to input file");
		Objects.requireNonNull (args [1], "wrong path to output file");
		
		// May cause InvalidPathException
		/* To prevent this:
		 * Path p = null;
		 * try {
		 * 	p = Paths.get (<given string>);
		 * } catch (InvalidPathException ipe) {
		 * 	System.err.println ("Invalid path to the input file");
		 * }
		 * 
		 * return p;
		 * 
		 * And wrap it into a separate method to prevent copy-paste
		 */
		Path input = Paths.get (args [0]),
				output = Paths.get (args [1]);
		
		try (
			// UTF-8 used by default
			Writer writer = Files.newBufferedWriter (output);
		) {
			visitor = new WritingFileVisitor <> (writer);
			
			try (
				// UTF-8 used by default
				Reader reader = Files.newBufferedReader (input);
				BufferedReader br = new BufferedReader (reader);
			) {
				String line;
				
				while ((line = br.readLine ()) != null) {
					Path start = Paths.get (line); // Getting path to the resource mentioned in file
					Files.walkFileTree (start, visitor); // Run walk throw the file system tree
					
					/* To prevent causing exceptions as variant:
					 * Path start = getPathFromString (line); // See comment before
					 * Files.walkFileTree (start, visitor);
					 * 
					 * In this case if `start` is wrong (== null) than in visitor
					 * will be called method `visitFileFailed` and default hash will be written
					 */
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
	
	private static final class WritingFileVisitor <T extends Path> extends SimpleFileVisitor <T> {
		
		private final Writer WRITER;
		
		public WritingFileVisitor (Writer writer) {
			Objects.requireNonNull (writer, "null writer given as argument");
			this.WRITER = writer;
		}
		
		private void writeHash (Path file, String hash) throws IOException {
			// WRITER << "hash filename\n"
			WRITER.append (hash).append (' ').append (file.toString ())
					.append (System.lineSeparator ());
		}
		
		@Override
		public FileVisitResult visitFile (T path, BasicFileAttributes attrs) throws IOException {
			try {
				// Assumed that path is never null (due to FileVisitor contract)
				writeHash (path, Hash.hash ("fnv", path.toFile ()));
			} catch (NoSuchAlgorithmException nsae) {
				// That's impossible ... just ignore
			}
			
			return FileVisitResult.CONTINUE;
		}
		
		@Override
		public FileVisitResult visitFileFailed (T path, IOException ioe) throws IOException {
			writeHash (path, Hash.DEFAULT_HASH);
			return FileVisitResult.CONTINUE;
		}
		
	}
	
}
