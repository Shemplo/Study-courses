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
        final var codes = new BigInteger [chars.length];
        //System.arraycopy (chars, 0, codes, 0, chars.length);
        for (int i = 0; i < chars.length; i++) {
            codes [i] = valueOf (chars [i]);
        }
        final var decrypted = decrypt (codes, new BigInteger (key), new BigInteger (mod));
        System.out.println (new String (decrypted));
    }
    
    public static char [] decrypt (BigInteger [] message, BigInteger key, BigInteger mod) {
        final char [] buffer = new char [message.length];
        
        for (int i = 0; i < message.length; i++) {
            buffer [i] = (char) message [i].modPow (key, mod).intValue ();
        }
        
        return buffer;
    }
    
    public static byte [] decryptB (BigInteger [] message, BigInteger key, BigInteger mod) {
        final byte [] buffer = new byte [message.length];
        
        for (int i = 0; i < message.length; i++) {
            buffer [i] = (byte) (message [i].modPow (key, mod).intValue () - 128);
        }
        
        return buffer;
    }
    
    public static byte [] decryptBlock (BigInteger [] message, int blockSize, BigInteger key, BigInteger mod) {
        final byte [] buffer = new byte [message.length * blockSize];
        
        for (int i = 0; i < message.length; i++) {
            var tmp = message [i].modPow (key, mod);
            for (int j = 0; j < blockSize; j++) {
                buffer [(i + 1) * blockSize - j - 1] = (byte) (tmp.and (valueOf (0xFF)).intValue () - 128);
                tmp = tmp.shiftRight (8);
            }
            
        }
        
        return buffer;
    }
    
}
