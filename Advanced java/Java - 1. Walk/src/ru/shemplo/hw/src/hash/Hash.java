package ru.shemplo.hw.src.hash;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.AccessDeniedException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

public class Hash {

	public static final String DEFAULT_HASH = "00000000";
	
	public static String hash (String method, File file) throws NoSuchAlgorithmException, 
																FileNotFoundException, 
																AccessDeniedException,
																IOException {
		// Not implemented in Java hash function
		// (Best way to register FVN hash function in the security provider list)
		// (See MessageDigest#getInstance(String, String) for more details)
		if (method.toLowerCase ().equals ("fnv")) {
			return runFNVHash (file);
		}
		
		// Getting algorithm of hashing by the given name
		MessageDigest algorithm = MessageDigest.getInstance (method);
		byte [] buffer = new byte [1 << 10];
		
		try (
			// Opening file for reading
			InputStream is = new FileInputStream (file);
			// Encoding is not important b/c hash is calculated by bytes data
		) {
			// Reading by blocks update hash
			while (is.read (buffer) != -1) {
				algorithm.update (buffer);
			}
		}
		
		// Converting to hash
		byte [] hash = algorithm.digest ();
		StringBuffer sb = new StringBuffer ();

		for (int i = 0; i < hash.length; ++i) {
			String hex = Integer.toHexString ((hash [i] & 0xFF) | 0x100);
			sb.append (hex.substring (1, 3));
		}

		// Turning back hash value
		return sb.toString ();
	}
	
	// ===| CUSTOM FNV HASH FUNCTION IMPLEMENTATION |=== //
	
	private static final int INITIAL_HASH = 0x811c9dc5;
	private static final int PRIME_NUMBER = 0x01000193;
	
	private static String runFNVHash (File file) throws FileNotFoundException, 
														AccessDeniedException, 
														IOException {
		byte [] buffer = new byte [1 << 10];
		int hash = INITIAL_HASH, read;
		
		try (
			// Opening file for reading
			InputStream is = new FileInputStream (file);
			// Encoding is not important b/c hash is calculated by bytes data
		) {
			// Reading by blocks update hash
			while ((read = is.read (buffer)) != -1) {
				for (int i = 0; i < read; i ++) {
					hash = (hash * PRIME_NUMBER) ^ (buffer [i] & 0xff);
				}
			}
		}
		
		// Converting integer representation to string
		return String.format ("%08x", hash);
	}
	
}
