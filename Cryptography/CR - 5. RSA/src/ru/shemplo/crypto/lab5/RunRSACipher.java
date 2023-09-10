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
        //System.out.println ("Cipher: " + new String (cipher.T1, 0, cipher.T1.length));
        System.out.println ("Exponent: " + cipher.T2);
        System.out.println ("Secret key: " + cipher.T3);
        System.out.println ("Modulus: " + cipher.T4);
    }
    
    public static Tup4 <BigInteger [], BigInteger, BigInteger, BigInteger> encrypt (char [] input) {
        final var key = generateKeyPair (64 * 4);
        return new Tup4 <> (encrypt (input, key.T1, key.T3), key.T1, key.T2, key.T3);
    }
    
    public static Tup4 <BigInteger [], BigInteger, BigInteger, BigInteger> encrypt (byte [] input) {
        final var key = generateKeyPair (64 * 4);
        return new Tup4 <> (encrypt (input, key.T1, key.T3), key.T1, key.T2, key.T3);
    }
    
    public static Tup4 <BigInteger [], BigInteger, BigInteger, BigInteger> encrypt (char [] input, BigInteger e) {
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
            //final var tmp = valueOf (RANDOM.nextInt (1024 * 1024));
            final var tmp = valueOf (Math.abs (RANDOM.nextLong ()));
            if (tmp.isProbablePrime (30) && tmp.compareTo (lowerBound) > 0) { 
                return tmp; 
            }
        }
    }
    
    public static BigInteger getPrime (int bits) {
        return BigInteger.probablePrime (bits, RANDOM);
    }
    
    public static Tup4 <BigInteger, BigInteger, BigInteger, Integer> generateKeyPair (int bits) {
        BigInteger p, q, e, n, phi;
        
        do {
            p = getPrime (bits); q = getPrime (bits);
            
            n = p.multiply (q);
            phi = p.subtract (ONE).multiply (q.subtract (ONE));
            
            e = new BigInteger (2 * bits - 1, RANDOM);
        } while (!e.gcd (phi).equals (ONE));
        
        final var d = e.modInverse (phi);
        
        return new Tup4 <> (e, d, n, bits);
    }
    
    public static BigInteger [] encrypt (char [] message, BigInteger key, BigInteger mod) {
        final BigInteger [] buffer = new BigInteger [message.length];
        
        for (int i = 0; i < message.length; i++) {
            buffer [i] = valueOf (message [i]).modPow (key, mod);
        }
        
        return buffer;
    }
    
    public static BigInteger [] encrypt (byte [] message, BigInteger key, BigInteger mod) {
        final BigInteger [] buffer = new BigInteger [message.length];
        
        for (int i = 0; i < message.length; i++) {
            buffer [i] = valueOf (message [i]).add (valueOf (128)).modPow (key, mod);
        }
        
        return buffer;
    }
    
    public static BigInteger [] encryptBlock (byte [] message, int blockSize, BigInteger key, BigInteger mod) {
        final var length = message.length / blockSize + (message.length % blockSize > 0 ? 1 : 0);
        final BigInteger [] buffer = new BigInteger [length];
        
        for (int i = 0; i < buffer.length; i++) {
            var tmp = BigInteger.ZERO;
            for (int j = 0; j < blockSize; j++) {
                tmp = tmp.shiftLeft (8);
                if (i * blockSize + j < message.length) {                    
                    tmp = tmp.or (valueOf (message [i * blockSize + j] + 128));
                }
            }
            
            buffer [i] = tmp.modPow (key, mod);
        }
        
        return buffer;
    }
    
}
