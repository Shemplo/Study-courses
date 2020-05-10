package ru.shemplo.crypto.lab3;

import java.util.Random;

public class A5Cipher {
    
    private final int [] x = new int [19],
                         y = new int [22],
                         z = new int [23];
    
    public A5Cipher () {
        final var random = new Random ();
        
        Utils.randomizeArray (x, random, 2);
        Utils.randomizeArray (y, random, 2);
        Utils.randomizeArray (z, random, 2);
    }
    
    public Tup4 <char [], int [], Void, Void> encrypt (char [] message) {
        int [] key = new int [message.length];
        
        for (int i = 0; i < message.length; i++) {
            int m = Utils.majority (x [8], y [10], x [10]);
            if (x [8] == m) {
                int t = x [13] ^ x [16] ^ x [17] ^ x [18];
                System.arraycopy (x, 0, x, 1, x.length - 1);
                x [0] = t;
            }
            if (y [10] == m) {
                int t = y [20] ^ y [21];
                System.arraycopy (y, 0, y, 1, y.length - 1);
                y [0] = t;
            }
            if (x [10] == m) {
                int t = z [7] ^ z [20] ^ z [21] ^ z [22];
                System.arraycopy (z, 0, z, 1, z.length - 1);
                z [0] = t;
            }
            
            key [i] = x [18] ^ y [21] ^ z [22];
        }
        
        return new Tup4 <> (decrypt (message, key), key, null, null);
    }
    
    public char [] decrypt (char [] cipher, int [] key) {
        char [] message = new char [cipher.length];
        for (int i = 0; i < cipher.length; i++) {
            message [i] = (char) (cipher [i] ^ key [i]);
        }
        
        return message;
    }
    
}
