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
        System.out.println (decrypt (input, new BigInteger (key), new BigInteger (mod)));
    }
    
    public static String decrypt (String input, BigInteger key, BigInteger mod) {
        final char [] buffer     = new char [input.length ()], 
                      characters = input.toCharArray ();
        for (int i = 0; i < characters.length; i++) {
            buffer [i] = (char) valueOf (characters [i]).modPow (key, mod).intValue ();
        }
        
        return new String (buffer);
    }
    
}
