package ru.shemplo.crypto.lab5;

import java.io.IOException;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.util.NoSuchElementException;

public class RunRSAController {
    
    public static final BigInteger EXPONENT = BigInteger.valueOf (5);
    
    public static void main (String ... args) throws IOException {
        System.out.println (">> HW 5. RSA cipher <<");
        
        while (true) {
            System.out.print ("Select mode [(e)ncrypt, (d)ecrypt, (en)crypt-decrypt, (sm)all exponent hack, (s)top]: ");
            String mode = Utils.readInput ().toLowerCase ();
            if (mode.equals ("e")) {
                RunRSACipher.main ();
            } else if (mode.equals ("d")) {
                RunRSADecipher.main ();
            } else if (mode.equals ("en")) {
                System.out.print ("Message: ");
                String input = Utils.readInput ();
                
                Tup4 <BigInteger [], ?, ?, ?> cipher = null;
                boolean correct = false;
                String decrypted = "";
                int attempts = 0;
                
                while (!correct && attempts < 10) {
                    final var tmp = RunRSACipher.encrypt (input.getBytes (StandardCharsets.UTF_8));
                    cipher = tmp;
                    
                    decrypted = new String (RunRSADecipher.decryptB (tmp.T1, tmp.T3, tmp.T4));
                    correct = input.equals (decrypted);
                    attempts++;
                }
                
                System.out.println ();
                //System.out.println ("Cipher: " + Arrays.toString (cipher.T1));
                System.out.println ("Exponent: " + cipher.T2);
                System.out.println ("Secret key: " + cipher.T3);
                System.out.println ("Modulus: " + cipher.T4);
                
                System.out.println ();
                System.out.println ("Message: " + decrypted);
                System.out.println ("Iteration: " + attempts);
                System.out.println ("Correct: " + correct);
            } else if (mode.equals ("sm")) {
                try {
                    RunSmallExponentHack.main ();
                    System.out.println ();
                } catch (NoSuchElementException nsee) {
                    System.out.println ("Hack failed :(");
                }
            } else if (mode.equals ("s")) {
                break;
            }
        }
    }
    
}
