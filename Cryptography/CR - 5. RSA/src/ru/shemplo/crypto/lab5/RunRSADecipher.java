package ru.shemplo.crypto.lab5;

import static java.math.BigInteger.valueOf;

import java.io.IOException;
import java.math.BigInteger;

public class RunRSADecipher {
    
    public static void main (String ... args) throws IOException {
        System.out.print ("Encoded message: ");
        String input = Utils.readInput ();
        System.out.print ("Secret key: ");
        String key = Utils.readInput ();
        System.out.print ("Modulus: ");
        String mod = Utils.readInput ();
        
        System.out.println ();
        final var chars = input.toCharArray ();
        final var codes = new int [chars.length];
        System.arraycopy (chars, 0, codes, 0, chars.length);
        final var decrypted = decrypt (codes, new BigInteger (key), new BigInteger (mod));
        System.out.println (new String (decrypted));
    }
    
    public static char [] decrypt (int [] message, BigInteger key, BigInteger mod) {
        final char [] buffer = new char [message.length];
        
        for (int i = 0; i < message.length; i++) {
            final var m = valueOf (message [i]).modPow (key, mod);
            //System.out.println (message [i] + " | " + ((int) message [i]) + " | " + m + " | " 
              //                  + m.intValue () + " | " + ((char) m.intValue ()));
            buffer [i] = (char) m.intValue ();
        }
        
        return buffer;
    }
    
}
