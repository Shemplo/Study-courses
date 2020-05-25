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
        
        final var cipher = encrypt (input.toCharArray ());
        
        System.out.println ();
        System.out.println ("Cipher: " + new String (cipher.T1, 0, cipher.T1.length));
        System.out.println ("Exponent: " + cipher.T2);
        System.out.println ("Secret key: " + cipher.T3);
        System.out.println ("Modulus: " + cipher.T4);
    }
    
    public static Tup4 <int [], BigInteger, BigInteger, BigInteger> encrypt (char [] input) {
        BigInteger p, q, e, n, phi;
        
        final var charBound = valueOf (Character.MAX_CODE_POINT);
        
        do {
            p = getPrime (); q = getPrime ();
            
            n = p.multiply (q);
            phi = p.subtract (ONE).multiply (q.subtract (ONE));
            
            e = BigInteger.valueOf (RANDOM.nextInt (phi.intValue ()));
        } while (!e.gcd (phi).equals (ONE) || n.compareTo (charBound) >= 0);
        
        final var d = e.modInverse (phi);
        
        return new Tup4 <> (encrypt (input, e, n), e, d, n);
    }
    
    public static Tup4 <int [], BigInteger, BigInteger, BigInteger> encrypt (char [] input, BigInteger e) {
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
        final var lowerBound = valueOf (100);
        
        while (true) {
            final var tmp = valueOf (RANDOM.nextInt (256 * 2));
            if (tmp.isProbablePrime (30) && tmp.compareTo (lowerBound) > 0) { 
                return tmp; 
            }
        }
    }
    
    public static int [] encrypt (char [] message, BigInteger key, BigInteger mod) {
        final int [] buffer = new int [message.length];
        
        for (int i = 0; i < message.length; i++) {
            final var m = valueOf (message [i]).modPow (key, mod);
            //System.out.println (message [i] + " | " + ((int) message [i]) + " | " + m + " | " 
              //                  + m.intValue () + " | " + ((char) m.intValue ()));
            buffer [i] = m.intValue ();
        }
        
        return buffer;
    }
    
}
