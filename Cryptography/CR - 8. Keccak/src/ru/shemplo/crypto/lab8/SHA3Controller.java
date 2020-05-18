package ru.shemplo.crypto.lab8;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

public class SHA3Controller {
    
    public static void main (String ... args) throws IOException, NoSuchAlgorithmException {
        final var testMessage = "The quick brown fox jumps over the lazy dog";
        
        System.out.println (">> HW 8. SHA-3 (Keccak)<<");
        final var algorithm = MessageDigest.getInstance ("SHA3-512");
        final var keccakHash = new Keccak (1600 - 1024, 0x6, 512);
        
        while (true) {
            System.out.print ("Select mode [digest from (i)nput, digest from (f)ile, (s)top]: ");
            String mode = Utils.readInput ().toLowerCase ();
            if (mode.equals ("i")) {
                System.out.println ("Test message: " + testMessage);
                System.out.print ("Input: ");
                String input = Utils.readInput ();
                
                final var startI = System.currentTimeMillis ();
                final var actualHash = keccakHash.digest (input.getBytes ());
                final var endI = System.currentTimeMillis ();
                
                System.out.format ("Implemented hash (%4dms): ", endI - startI);
                for (byte b : actualHash) {
                    System.out.format ("%02x", b);
                }
                System.out.println ();
                
                final var startE = System.currentTimeMillis ();
                final var expectedHash = algorithm.digest (input.getBytes ());
                final var endE = System.currentTimeMillis ();
                System.out.format ("Java SHA-3 hash  (%4dms): ", endE - startE);
                for (byte b : expectedHash) {
                    System.out.format ("%02x", b);
                }
                System.out.println ();
            } else if (mode.equals ("f")) {
                final var fileBytes = Files.readAllBytes (Paths.get ("test.txt"));
                
                final var startI = System.currentTimeMillis ();
                final var actualHash = keccakHash.digest (fileBytes);
                final var endI = System.currentTimeMillis ();
                
                System.out.format ("Implemented hash (%3dms): ", endI - startI);
                for (byte b : actualHash) {
                    System.out.format ("%02x", b);
                }
                System.out.println ();
                
                final var startE = System.currentTimeMillis ();
                final var expectedHash = algorithm.digest (fileBytes);
                final var endE = System.currentTimeMillis ();
                System.out.format ("Java SHA-3 hash  (%3dms): ", endE - startE);
                for (byte b : expectedHash) {
                    System.out.format ("%02x", b);
                }
                System.out.println ();
            } else if (mode.equals ("s")) {
                break;
            }
        }
    }
    
}
