package ru.shemplo.crypto.lab2;

import static ru.shemplo.crypto.lab2.Permutations.*;
import static ru.shemplo.crypto.lab2.Utils.*;

import java.io.IOException;
import java.util.Arrays;

public class RunDESCipher {
    
    private static final byte [] rotations = { 1, 1, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 1 };
    
    public static void main (String ... args) throws IOException {
        System.out.print ("Message: ");
        String input = Utils.readInput ();
        System.out.print ("Key: ");
        String key = Utils.readInput ();
        
        byte [] cipher = encrypt (input.getBytes (), key.getBytes ());
        System.out.println ();
        System.out.println (new String (cipher));
    }
    
    public static byte [] encrypt (byte [] message, byte [] key) {
        if (message.length % 8 > 0) {
            int length = (message.length / 8 + 1) * 8;
            message = Arrays.copyOf (message, length);
        }
        
        byte [] cipher = new byte [message.length];
        
        long lkey = getLongFromBytes (key, 0);
        for (int i = 0; i < message.length; i += 8) {
            encrypt (message, i, cipher, i, lkey);
        }
        
        return cipher;
    }
    
    private static void encrypt (
        byte [] message, int messageOffset, 
        byte [] cipher, int ciphertextOffset, 
        long key
    ) {
        long m = getLongFromBytes (message, messageOffset);
        long c = encrypt (m, key);
        getBytesFromLong (cipher, ciphertextOffset, c);
    }
    
    public static long encrypt (long m, long key) {
        long subkeys [] = createSubkeys (key, rotations);
        long ip = permuteI (m);
        
        int l = (int) (ip >> 32);
        int r = (int) (ip & 0xFFFFFFFFL);
        
        for (int i = 0; i < 16; i++) {
            int previous_l = l;
            
            l = r;
            r = previous_l ^ feistel (r, subkeys [i]);
        }
        
        long rl = (r & 0xFFFFFFFFL) << 32 | (l & 0xFFFFFFFFL);
        long fp = permuteF (rl);
        
        return fp;
    }
    
}
