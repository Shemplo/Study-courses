package ru.shemplo.crypto.lab5;

import static java.math.BigInteger.*;

import java.io.IOException;
import java.math.BigInteger;
import java.security.SecureRandom;
import java.util.Random;

public class RunRSACipher {
    
    private static final Random RANDOM = new SecureRandom ();
    
    public static void main (String ... args) throws IOException {
        System.out.print ("Message: ");
        String input = Utils.readInput ();
        
        final var cipher = encrypt (input);
        
        System.out.println ();
        System.out.println ("Cipher: " + cipher.T1);
        System.out.println ("Exponent: " + cipher.T2);
        System.out.println ("Secret key: " + cipher.T3);
        System.out.println ("Modulus: " + cipher.T4);
    }
    
    public static Tup4 <String, BigInteger, BigInteger, BigInteger> encrypt (String input) {
        BigInteger p, q, e, n, phi;
        
        do {
            p = getPrime (); q = getPrime ();
            
            n = p.multiply (q);
            phi = p.subtract (ONE).multiply (q.subtract (ONE));
            
            e = BigInteger.valueOf (RANDOM.nextInt (phi.intValue ()));
        } while (!e.gcd (phi).equals (ONE));
        
        final var d = e.modInverse (phi);
        
        return new Tup4 <> (encrypt (input, e, n), e, d, n);
    }
    
    public static Tup4 <String, BigInteger, BigInteger, BigInteger> encrypt (String input, BigInteger e) {
        BigInteger p, q, n, phi;
        
        do {
            p = getPrime (); q = getPrime ();
            
            n = p.multiply (q);
            phi = p.subtract (ONE).multiply (q.subtract (ONE));
        } while (!e.gcd (phi).equals (ONE));
        
        final var d = e.modInverse (phi);
        
        return new Tup4 <> (encrypt (input, e, n), e, d, n);
    }
    
    public static BigInteger getPrime () {
        while (true) {
            final var tmp = BigInteger.valueOf (RANDOM.nextInt (256));
            if (tmp.isProbablePrime (20)) { return tmp; }
        }
    }
    
    private static String encrypt (String message, BigInteger key, BigInteger mod) {
        final char [] buffer     = new char [message.length ()], 
                      characters = message.toCharArray ();
        for (int i = 0; i < characters.length; i++) {
            buffer [i] = (char) valueOf (characters [i]).modPow (key, mod).intValue ();
        }
        
        return new String (buffer);
    }
    
}
